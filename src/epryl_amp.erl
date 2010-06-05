%%%-------------------------------------------------------------------
%%% @author Dave Peticolas <dave@krondo.com>
%%% @copyright (C) 2008, Dave Peticolas
%%% @doc
%%% AMP protocol utilities. See:
%%%   [http://twistedmatrix.com/documents/current/api/twisted.protocols.amp.html]
%%%
%%% This module implements low-level encoding and decoding of AMP
%%% Boxes. An Amp Box is represented in Erlang as a list of key/value
%%% pairs. This type is referred to as a Box.
%%%
%%%    Box = [KVP]
%%%    KVP = {Key, Value}
%%%    Key = string()
%%%    Value = term()
%%%
%%% @end
%%% Created :  7 Apr 2008 by Dave Peticolas <dave@krondo.com>
%%%-------------------------------------------------------------------
-module(epryl_amp).

%% API
-export([make_error/2,
         encode_ask/3, encode_answer/3, encode_error/3,
         encode_response/4,
         new_decoder/1, decode_box/2,
         decode_header/1, decode_command_header/1,
         encode_box/2]).

-include("epryl_amp.hrl").
-include("eunit.hrl").

-record(decoder, {orig_protocol, protocol, remainder, box=[]}).


%%--------------------------------------------------------------------
%% @doc Given an error atom key and a string description, return a box
%% that can be returned by an ask handler as the error information for
%% an error response.
%%
%% @spec make_error(Key::atom(), Description::string()) -> box()
%% @end
%%--------------------------------------------------------------------
make_error(Key, Description) when is_atom(Key), is_list(Description) ->
    [{?AMP_KEY_ERROR_CODE, Key},
     {?AMP_KEY_ERROR_DESCRIPTION, Description}].


%%--------------------------------------------------------------------
%% @doc Given an amp_command record, a message id, and a box, return a
%% binary encoding of the Amp Box that would implement the call.
%%
%% @spec encode_ask(Command::amp_record(), Id::string(), Box::box()) -> binary()
%% @end
%%--------------------------------------------------------------------
encode_ask(Command, Id, Box)
  when is_record(Command, amp_command), is_list(Id), is_list(Box)->
    [_ | _] = Box, % no empty boxes
    encode_box([{?AMP_KEY_ASK, string, []},
                {?AMP_KEY_COMMAND, string, []}
                | Command#amp_command.arguments],
               [{?AMP_KEY_ASK, Id},
                {?AMP_KEY_COMMAND, Command#amp_command.name}
                | Box]).


%%--------------------------------------------------------------------
%% @doc Given an amp_command record, a message id, and a box, return a
%% binary encoding of the AmpBox that would implement the answer box for
%% a call.
%%
%% @spec encode_answer(Command::amp_record(), Id::string(),
%%                     Box::box()) -> binary()
%% @end
%%--------------------------------------------------------------------
encode_answer(Command, Id, Box)
  when is_record(Command, amp_command), is_list(Id), is_list(Box)->
    [_ | _] = Box, % no empty boxes
    encode_box([{?AMP_KEY_ANSWER, string, []} | Command#amp_command.response],
               [{?AMP_KEY_ANSWER, Id} | Box]).

%%--------------------------------------------------------------------
%% @doc Given an amp_command record, a message id, and a box, return a
%% binary encoding of the Amp Box that would implement the error box for
%% a call.
%%
%% @spec encode_error(Command::amp_record(), Id::string(),
%%                    Box::box()) -> binary()
%% @end
%%--------------------------------------------------------------------
encode_error(Command, Id, Box)
  when is_record(Command, amp_command), is_list(Id), is_list(Box) ->
    {value, {_, ErrorKey}} = lists:keysearch(?AMP_KEY_ERROR_CODE, 1, Box),
    Box2 = lists:keyreplace(?AMP_KEY_ERROR_CODE, 1, Box,
                            {?AMP_KEY_ERROR_CODE,
                             error_name(ErrorKey, Command)}),
    encode_box([{?AMP_KEY_ERROR, string, []} | ?AMP_ERROR_PROTOCOL],
               [{?AMP_KEY_ERROR, Id} | Box2]).

%%--------------------------------------------------------------------
%% @doc Given an amp_command record, a message id, and a box, return a
%% binary encoding of the AmpBox that would implement the error box for
%% a call.
%%
%% @spec encode_response(Response::atom(), Command::amp_record(),
%%                       Id::string(), Box::box()) -> binary()
%% @end
%%--------------------------------------------------------------------
encode_response(answer, Command, Id, Box) ->
    encode_answer(Command, Id, Box);
encode_response(error, Command, Id, Box) ->
    encode_error(Command, Id, Box).

%%--------------------------------------------------------------------
%% @doc Return a new decoder object suitable for unserializing a wire
%% format of an Amp box. Decoders are required arguments for decode_box/2.
%%
%% @spec new_decoder(Protocol::list()) -> decoder()
%% @end
%%--------------------------------------------------------------------
new_decoder(Protocol) when is_list(Protocol) ->
    [_ | _] = Protocol, % no empty boxes
    EmptyBin = <<>>,
    #decoder{orig_protocol=Protocol, protocol=Protocol, remainder=EmptyBin}.

%%--------------------------------------------------------------------
%% @doc Decode a part (or whole) of a box.
%% If the complete box is decoded, return the box we decoded and the
%% unprocessed bytes. If we are not done, return the new state of the
%% decoder.
%%
%% @spec (Decoder::decoder(), Packet::binary()) -> Result
%%
%%        Result = {not_done, decoder()} |
%%                 {done, Box::box(), Rest::binary()}
%% @end
%%--------------------------------------------------------------------
decode_box(Decoder, Packet) when is_record(Decoder, decoder),
                                 is_binary(Packet) ->
    Whole = erlang:concat_binary([Decoder#decoder.remainder, Packet]),
    Result = decode_box(Decoder#decoder.protocol,
                        Decoder#decoder.box, Whole),
    case Result of
        {not_done, Protocol, Box, Rest} ->
            {not_done, Decoder#decoder{protocol=Protocol, box=Box,
                                       remainder=Rest}};
        {done, Box, Rest} ->
            [_ | _] = Box, % no empty boxes
            {done, lists:reverse(Box), Rest}
    end.

%%--------------------------------------------------------------------
%% @doc Match a key/value pair encoded at the front of an incoming box.
%% Return a tuple indicating the type of incoming box and the Id for the
%% message, plus the remaining bytes in the packet. The function will crash
%% if the kvp has the wrong name.
%%
%% @spec (Packet::binary()) -> Result
%%
%%        Result = not_enough | {BoxType, Id::string(), Remaining::binary()}
%%        BoxType = ask | answer | error
%% @end
%%--------------------------------------------------------------------
decode_header(Packet) when is_binary(Packet) ->
    case match_kvp(Packet) of
        not_enough ->
            not_enough;
        {Key, ValBin, Remaining} ->
            Id = decode_value(ValBin, string),
            BoxType = box_type(Key),
            {BoxType, Id, Remaining}
    end.

%%--------------------------------------------------------------------
%% @doc Match a key/value pair encoding the command name of an ask box.
%% Return a tuple with the command name and the remaining bytes in the
%% packet. The function will crash if the kvp was the wrong name.
%%
%% @spec (Packet::binary()) -> Result
%%
%%        Result = not_enough | {CommandName::string(), Remaining::binary()}
%% @end
%%--------------------------------------------------------------------
decode_command_header(Packet) when is_binary(Packet) ->
    case match_kvp(Packet) of
        not_enough ->
            not_enough;
        {?AMP_KEY_COMMAND, ValBin, Remaining} ->
            CommandName = decode_value(ValBin, string),
            {CommandName, Remaining}
    end.

%%--------------------------------------------------------------------
%% @doc Given an AmpList protocol and a box, return a binary encoding
%% of the box that matches the protocol.
%%
%% @spec encode_box(Protocol::AmpList, Box::box()) -> binary()
%% @end
%%--------------------------------------------------------------------
encode_box(Protocol, Box) when is_list(Protocol), is_list(Box) ->
    IOList = encode_box_int(Protocol, Box),
    [_, _ | _] = IOList, % no empty boxes
    list_to_binary(IOList).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% @private
% @spec (Protocol::list(), Box::box()) -> iolist()
% @doc Encode the box according to the given protocol into the IOList.
encode_box_int([], _Box) ->
    [<<0, 0>>];
encode_box_int([{Key, Type, Options} | Protocol], Box) ->
    case lists:keysearch(Key, 1, Box) of
        {value, {Key, Value}} ->
            [_ | _] = Key, % no empty keys
            EncKeyLength = encode_length(length(Key), ?AMP_MAX_KEY_LEN),
            EncValue = encode_value(Value, Type),
            EncLength = encode_length(iolist_size(EncValue), ?AMP_MAX_VAL_LEN),
            [EncKeyLength, Key, EncLength, EncValue
             | encode_box_int(Protocol, Box)];
        false ->
            true = proplists:get_bool(optional, Options),
            encode_box_int(Protocol, Box)
    end.

% @private
% @spec (Protocol::list(), Box::box(), Packet::binary()) -> Result
%      Result = {not_done, Protocol, Box, Rest::binary()} |
%               {done, Box, Rest::binary()}
% @doc Decode the packet as much as possible and return the results.
decode_box(Protocol, Box, Packet) when size(Packet) < 2 ->
    {not_done, Protocol, Box, Packet};
decode_box([], Box, <<0, 0, Rest/binary>>) ->
    {done, Box, Rest};
decode_box([{_, _, Options} | Protocol], Box, <<0, 0, _/binary>> = Packet) ->
    true = proplists:get_bool(optional, Options),
    decode_box(Protocol, Box, Packet);
decode_box(Protocol, Box, Packet) ->
    case match_kvp(Packet) of
        not_enough ->
            {not_done, Protocol, Box, Packet};
        {Key, ValBin, Rest} ->
            {Type, NewProtocol} = consume_key(Key, Protocol),
            Value = decode_value(ValBin, Type),
            decode_box(NewProtocol, [{Key, Value} | Box], Rest)
    end.

% @private
% @spec (Packet::binary()) ->
%        {Key::string(), ValBin::binary(), Rest::binary()} | not_enough
% @doc Match a key/value pair encoded at the front of Packet.
% Return a tuple with the key name, the value still encoded as a binary,
% and the remaining bytes from Packet that were not used in the kvp. Or,
% if there are not enough bytes to decode the first kvp, return not_enough.
match_kvp(<<0, KeyLen, KeyBytes:KeyLen/binary,
            ValLen:16/unsigned-big, Val:ValLen/binary, Rest/binary>>) ->
    {erlang:binary_to_list(KeyBytes), Val, Rest};
match_kvp(<<0, _/binary>>) ->
    not_enough;
match_kvp(<<>>) ->
    not_enough.


% @private
% @spec (Key::string(), Protocol::list())
%                   -> {Type::atom(), NewProtocol::list()}
% @doc Consume the given key from the Protocol and return the
% type of that key and the remaining protocol.
consume_key(Key, Protocol) ->
    {value, {Key, Type, _Options}, Protocol2} = lists:keytake(Key, 1, Protocol),
    {Type, Protocol2}.


% @private
% @spec (Length::integer(), Max::integer()) -> binary()
% @doc Encode a length, given the maximum value of that length.
encode_length(Length, Max) when Length =< Max ->
    <<Length:16/unsigned-big>>.


% @private
% @spec (Value::term(), Type::atom()) -> iolist()
% @doc Encode a value given its type into an iolist.
encode_value(Value, integer) when is_integer(Value) ->
    integer_to_list(Value);
encode_value(Value, float) when is_float(Value) ->
    float_to_list(Value);
encode_value(Value, float) when is_integer(Value) ->
    float_to_list(float(Value));
encode_value(Value, boolean) when is_boolean(Value) ->
    case Value of
        true -> <<"True">>;
        false -> <<"False">>
    end;
encode_value(Value, string) when is_list(Value) ; is_binary(Value) ->
    Value;
encode_value(Value, binary) when is_list(Value) ; is_binary(Value) ->
    Value;
encode_value(Value, {amplist, Protocol}) ->
    [encode_box(Protocol, Box) || Box <- Value].


% @private
% @spec (ValBin::binary(), Type::atom()) -> term()
% @doc Decode a value in binary form given its type.
decode_value(ValBin, integer) ->
    erlang:list_to_integer(erlang:binary_to_list(ValBin));
decode_value(ValBin, float) ->
    Val = erlang:binary_to_list(ValBin),
    erlang:list_to_float(ensure_decimal(Val));
decode_value(<<"True">>, boolean) ->
    true;
decode_value(<<"False">>, boolean) ->
    false;
decode_value(ValBin, string) ->
    erlang:binary_to_list(ValBin);
decode_value(ValBin, binary) ->
    ValBin;
decode_value(ValBin, {amplist, Protocol}) ->
    decode_amplist(ValBin, Protocol).


% @private
% @spec (ValBin::binary(), Protocol::list()) -> list()
% @doc Decode an amplist value and return the list of boxes (kvpair lists).
decode_amplist(<<>>, _Protocol) ->
    [];
decode_amplist(ValBin, Protocol) ->
    {done, KVPairs, Rest} = decode_box(Protocol, [], ValBin),
    [lists:reverse(KVPairs) | decode_amplist(Rest, Protocol)].


% @private
% @spec (String::string()) -> string()
% @doc Return the original string, if it has a decimal point,
% or the original string followed by .0 if not.
ensure_decimal(String) ->
    case lists:member($., String) of
        true ->
            String;
        false ->
            String ++ ".0"
    end.


% @private
% @spec (Key::string()) -> atom()
% @doc Return a box type atom given the key.
box_type(?AMP_KEY_ASK) ->
    ask;
box_type(?AMP_KEY_ANSWER) ->
    answer;
box_type(?AMP_KEY_ERROR) ->
    error.


% @private
% @spec (Key::atom(), Command::amp_command()) -> Name::string()
% @doc Return the string encoding of an error given its atom key.
error_name(Key, Command) ->
    {value, {_, Name, _Options}} = lists:keysearch(Key, 1,
                                                   Command#amp_command.errors),
    Name.


%%%===================================================================
%%% Tests
%%%===================================================================

encode_test_() ->
    [
     ?_assertMatch(<<0, 1, $a, 0, 1, $1, 0, 0>>,
                   encode_box([{"a", integer, []}], [{"a", 1}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{"aa", string, []}], [{"aa", "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{"aa", string, []}], [{"aa", <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{"aa", string, []}], [{"aa", "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{"aa", binary, []}], [{"aa", <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{"aa", binary, []}], [{"aa", "xyz"}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 3, "xyz", 0, 0>>,
                   encode_box([{"aa", binary, []}], [{"aa", <<"xyz">>}])),
     ?_assertMatch(<<0, 2, $a, $a, 0, 26, "1.50000000000000000000e+00", 0, 0>>,
                   encode_box([{"aa", float, []}], [{"aa", 1.5}])),
     ?_assertMatch(<<0, 1, $a, 0, 4, "True", 0, 0>>,
                   encode_box([{"a", boolean, []}], [{"a", true}])),
     ?_assertMatch(<<0, 1, $a, 0, 5, "False", 0, 0>>,
                   encode_box([{"a", boolean, []}], [{"a", false}])),
     ?_assertMatch(<<0, 1, $a, 0, 1, $1, 0, 0>>,
                   encode_box([{"b", integer, [optional]}, {"a", integer, []}],
                               [{"a", 1}])),
     ?_assertMatch(<<0, 1, $b, 0, 1, $2, 0, 1, $a, 0, 1, $1, 0, 0>>,
                   encode_box([{"b", integer, [optional]}, {"a", integer, []}],
                               [{"a", 1}, {"b", 2}])),
     ?_assertMatch(<<0, 1, $a, 0, 8, 0, 1, $b, 0, 1, $5, 0, 0, 0, 0>>,
                   encode_box([{"a", {amplist, [{"b", integer, []}]}, []}],
                              [{"a", [[{"b", 5}]]}])),
     ?_assertMatch(<<0, 1, $a, 0, 16, 0, 1, $b, 0, 1, $5, 0, 0,
                                      0, 1, $b, 0, 1, $0, 0, 0, 0, 0>>,
                   encode_box([{"a", {amplist, [{"b", integer, []}]}, []}],
                              [{"a", [[{"b", 5}], [{"b", 0}]]}])),
     ?_assertError(_, encode_box([{[256], string, []}], [{[256], "xyz"}])),
     ?_assertError(_, encode_box([{"aa", string, []}], [{"aa", 1}])),
     ?_assertError(_, encode_box([{"", string, []}], [{"", "a"}])),
     ?_assertError(_, encode_box([{"aa", float, []}], [{"aa", "apple"}])),
     ?_assertError(_, encode_box([{"b", integer, [optional]}], [{"a", 1}])),
     ?_assertError(_, encode_box([{"b", integer, [optional]}], []))
    ].

encode_ask_test() ->
    Cmd = #amp_command{name="n", arguments=[{"a", string, []}], response=nil},
    Bin = encode_ask(Cmd, "1", [{"a", "A"}]),
    ?assertMatch(Bin, <<0, 4, "_ask", 0, 1, "1",
                        0, 8, "_command", 0, 1, "n",
                        0, 1, "a", 0, 1, "A", 0, 0>>),
    ?assertMatch({ask, "1", _}, decode_header(Bin)).

encode_answer_test() ->
    Cmd = #amp_command{name="n", arguments=nil, response=[{"b", string, []}]},
    Bin = encode_answer(Cmd, "1", [{"b", "B"}]),
    ?assertMatch(Bin, <<0, 7, "_answer", 0, 1, "1",
                        0, 1, "b", 0, 1, "B", 0, 0>>),
    ?assertMatch({answer, "1", _}, decode_header(Bin)).

encode_error_test() ->
    Cmd = #amp_command{arguments=nil, response=nil,
                       errors=[{a, "A", []}, {b, "B", [fatal]}]},
    Err1 = make_error(a, "AA"),
    Bin1 = encode_response(error, Cmd, "1", Err1),
    ?assertMatch(Bin1, <<0, 6, "_error", 0, 1, "1",
                         0, 11, "_error_code", 0, 1, "A",
                         0, 18, "_error_description", 0, 2, "AA", 0, 0>>),
    {error, "1", Rest1} = decode_header(Bin1),
    Decoder = new_decoder(?AMP_ERROR_PROTOCOL),
    Out1 = {done, [{?AMP_KEY_ERROR_CODE, "A"},
                   {?AMP_KEY_ERROR_DESCRIPTION, "AA"}], <<>>},
    ?assertMatch(Out1, decode_box(Decoder, Rest1)),

    Err2 = make_error(b, "BB"),
    Bin2 = encode_response(error, Cmd, "2", Err2),
    {error, "2", Rest2} = decode_header(Bin2),
    Out2 = {done, [{?AMP_KEY_ERROR_CODE, "B"},
                   {?AMP_KEY_ERROR_DESCRIPTION, "BB"}], <<>>},
    ?assertMatch(Out2, decode_box(Decoder, Rest2)).


test_one_by_one(Decoder, <<Byte, Input/binary>>) ->
    case decode_box(Decoder, <<Byte>>) of
        {not_done, Decoder1} ->
            test_one_by_one(Decoder1, Input);
        Result ->
            Result
    end.

decode_1_test() ->
    Protocol = [{"a", integer, []}],
    Decoder1 = new_decoder(Protocol),
    {not_done, Decoder2} = decode_box(Decoder1, <<0>>),
    ?assert(Decoder2#decoder.protocol == Protocol),
    ?assert(Decoder2#decoder.remainder == <<0>>),
    ?assert(Decoder2#decoder.box == []),

    {not_done, Decoder3} = decode_box(Decoder2, <<1, $a>>),

    ?assert(Decoder2#decoder.protocol == Protocol),
    ?assert(Decoder3#decoder.remainder == <<0, 1, $a>>),
    ?assert(Decoder3#decoder.box == []).

decode_2_test() ->
    Protocol = [{"a", integer, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 1, $a, 0, 1, $1, 0, 0>>,
    ?assertMatch({done, [{"a", 1}], <<>>}, decode_box(Decoder, Input)).

decode_3_test() ->
    Protocol = [{"a", integer, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 1, $a, 0, 1, $4, 0, 0, 14>>,
    ?assertMatch({done, [{"a", 4}], <<14>>}, decode_box(Decoder, Input)).

decode_4_test() ->
    Protocol = [{"name", string, []}, {"billy o", integer, []}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 7, "billy o", 0, 5, "12345",
               0, 4, "name", 0, 5, "nimbo",
               0, 0>>,
    Output = {done, [{"billy o", 12345}, {"name", "nimbo"}], <<>>},
    ?assertMatch(Output, test_one_by_one(Decoder, Input1)),
    Input2 = <<0, 4, "name", 0, 5, "nimbo",
               0, 7, "billy o", 0, 5, "12345",
               0, 0>>,
    Output2 = {done, [{"name", "nimbo"}, {"billy o", 12345}], <<>>},
    ?assertMatch(Output2, test_one_by_one(Decoder, Input2)).

decode_5_test() ->
    Protocol = [{"name", string, []}],
    Decoder = new_decoder(Protocol),
    Input = <<1, 4, "name", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_6_test() ->
    Protocol = [{"name", string, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 4, "namx", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_7_test() ->
    Protocol = [{"q", integer, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 1, "q", 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_8_test() ->
    Protocol = [{";", float, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 1, $;, 0, 3, "1.5", 0, 0, 1, 2, 3>>,
    ?assertMatch({done, [{";", 1.5}], <<1, 2, 3>>}, decode_box(Decoder, Input)).

decode_9_test() ->
    Protocol = [{"//", boolean, []}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 2, "//", 0, 4, "True", 0, 0>>,
    ?assertMatch({done, [{"//", true}], <<>>}, decode_box(Decoder, Input1)),
    Input2 = <<0, 2, "//", 0, 5, "False", 0, 0>>,
    ?assertMatch({done, [{"//", false}], <<>>}, decode_box(Decoder, Input2)).

decode_10_test() ->
    Protocol = [{"a", integer, [optional]}, {"b", integer, []}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 1, $a, 0, 1, $4, 0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({done, [{"a", 4}, {"b", 5}], <<>>}, decode_box(Decoder, Input1)),
    Input2 = <<0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({done, [{"b", 5}], <<>>}, decode_box(Decoder, Input2)).

decode_11_test() ->
    Protocol = [{"a", integer, []}, {"b", integer, [optional]}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 1, $a, 0, 1, $4, 0, 1, $b, 0, 1, $5, 0, 0>>,
    ?assertMatch({done, [{"a", 4}, {"b", 5}], <<>>}, decode_box(Decoder, Input1)),
    Input2 = <<0, 1, $a, 0, 1, $5, 0, 0>>,
    ?assertMatch({done, [{"a", 5}], <<>>}, decode_box(Decoder, Input2)).

decode_12_test() ->
    SubProto = [{"h", integer, []}, {"g", integer, [optional]}],
    Protocol = [{"'", integer, []}, {"s", {amplist, SubProto}, [optional]}],
    Decoder = new_decoder(Protocol),

    Input1 = <<0, 1, $', 0, 1, $0, 0, 0, 1, 2, 3>>,
    ?assertMatch({done, [{"'", 0}], <<1, 2, 3>>}, decode_box(Decoder, Input1)),

    Input2 = <<0, 1, $', 0, 1, $0,
              0, 1, $s, 0, 14,
                0, 1, $h, 0, 1, $1, 0, 1, $g, 0, 1, $2, 0, 0,
              0, 0, 1, 2, 3>>,
    Output2 = [{"'", 0}, {"s", [[{"h", 1}, {"g", 2}]]}],
    ?assertMatch({done, Output2, <<1, 2, 3>>}, decode_box(Decoder, Input2)),

    Input3 = <<0, 1, $', 0, 1, $0,
              0, 1, $s, 0, 22,
                0, 1, $h, 0, 1, $1, 0, 1, $g, 0, 1, $2, 0, 0,
                0, 1, $h, 0, 1, $5, 0, 0,
              0, 0, 1, 2, 3>>,
    Output3 = [{"'", 0}, {"s", [[{"h", 1}, {"g", 2}], [{"h", 5}]]}],
    ?assertMatch({done, Output3, <<1, 2, 3>>}, decode_box(Decoder, Input3)).

decode_13_test() ->
    Protocol = [{"name", string, [optional]}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 0>>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_14_test() ->
    Protocol = [{"", string, []}],
    Decoder = new_decoder(Protocol),
    Input = <<0, 0, 0, 5, "nimbo">>,
    ?assertError(_, test_one_by_one(Decoder, Input)).

decode_15_test() ->
    Protocol = [{"name", binary, []}, {"billy o", integer, []}],
    Decoder = new_decoder(Protocol),
    Input1 = <<0, 7, "billy o", 0, 5, "12345",
               0, 4, "name", 0, 5, "nimbo",
               0, 0>>,
    Output = {done, [{"billy o", 12345}, {"name", <<"nimbo">>}], <<>>},
    ?assertMatch(Output, test_one_by_one(Decoder, Input1)).

decode_header_test_() ->
    [
     ?_assertMatch(not_enough, decode_header(<<>>)),
     ?_assertMatch(not_enough, decode_header(<<0, 4, "_ask">>)),
     ?_assertMatch({ask, "a", <<>>}, decode_header(<<0, 4, "_ask", 0, 1, $a>>)),
     ?_assertMatch({ask, "a", <<9>>},
                   decode_header(<<0, 4, "_ask", 0, 1, $a, 9>>)),
     ?_assertMatch({answer, "a", <<>>},
                   decode_header(<<0, 7, "_answer", 0, 1, $a>>)),
     ?_assertMatch({error, "a", <<>>},
                   decode_header(<<0, 6, "_error", 0, 1, $a>>)),
     ?_assertError(_, decode_header(<<0, 4, "_bad", 0, 1, $a>>))
     ].
