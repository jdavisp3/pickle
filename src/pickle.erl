%% Copyright (c) 2013, Dave Peticolas <dave@krondo.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%
%% @doc
%% A module for converting Erlang terms to and from Python pickles.
%%
%% Python pickles are binary representations of Python objects.
%% See [http://python.org/dev/peps/pep-0307/] for details.

-module(pickle).

%% API
-export([pickle_to_term/1, term_to_pickle/1, term_to_pickle/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("pickle.hrl").

-define(MAXINT, 2147483647). % threshold to switch to Python long ints
-define(MININT, -2147483648).
-define(IS_LONG(Number), ((Number > ?MAXINT) or (Number < ?MININT))).

% state of stack machine which implements pickle protocol
-record(mach, {
          stack=[] :: list(),
          memo :: dict(),
          done=false :: boolean()
         }).

%% pickler options
-record(encoder, {
          proto=2 :: 0..2,
          bin=true :: boolean()
         }).

%% @doc Turn a Python pickle into an Erlang term.
%%
%% The input binary must contain a complete Python
%% pickle and no more. For any other case, the function
%% will fail. The pickle must be in version 2 of the pickle
%% format.
%%
%% Only a subset of the complete pickle format is supported,
%% according to the following table:
%% ```
%%    Python                Erlang
%%    -------------------------------------
%%    None                  none (atom)
%%    True                  true (atom)
%%    False                 false (atom)
%%    string                binary
%%    list                  list
%%    tuple                 tuple
%%    integer               number
%%    long                  number
%%    dictionary            dict
%%    float                 float
%%    unicode               #pickle_unicode{} (record)
%%    object                #pickle_object{} (record)
%%    class / function      #pickle_global{} (record)
%% '''
%% Recursive and mutually recursive references are not
%% supported, i.e., lists cannot refer to themselves, etc.
%%
-spec pickle_to_term(Pickle::binary()) -> term().
pickle_to_term(Pickle) when is_binary(Pickle) ->
    {Val, <<>>} = start_machine(Pickle),
    Val.

%% @doc Turn an Erlang term into a Python pickle.
%%
%% The conversion will result in a pickle using version 2 codes and will
%% be performed according to the following table:
%% ```
%%    Erlang                Python
%%    -------------------------------------
%%    none (atom)           None
%%    true (atom)           True
%%    false (atom)          False
%%    binary                string
%%    list                  list
%%    tuple                 tuple
%%    number =< MAXINT      integer
%%    number > MAXINT       long
%%    dict                  dictionary
%%    float                 float
%%    bitstring             *not supported*
%%    arbitrary atom        *not supported*
%% '''
-spec term_to_pickle(Term::term()) -> Pickle::binary().
term_to_pickle(Term) ->
    %% protocol v2 by default, unlike python's v0
    term_to_pickle(Term, 2).

-spec term_to_pickle(Term::term(), Protocol::0..2) -> Pickle::binary().
term_to_pickle(Term, Protocol) when (Protocol >= 0) and (Protocol =< 2) ->
    start_encode(Term, #encoder{proto=trunc(Protocol),
                                bin=Protocol >= 1}).


%%% Internal functions

%% @private
%% @doc Create a new stack machine record
-spec new_mach() -> #mach{}.
new_mach() ->
    #mach{memo=dict:new()}.


%% @private
%% @doc Start running a new stack machine and return
%% the result of running it.
-spec start_machine(Pickle::binary()) -> {term(), binary()}.
start_machine(<<16#80, P, Rest/binary>> = _Pickle) when P >= 2 ->
    run_machine(new_mach(), Rest);
start_machine(Pickle) ->
    %% protocol < 2
    run_machine(new_mach(), Pickle).


%% @private
%% @doc Run the given stack machine on a pickle, or
%% the remains of one. The result of the run and the
%% remains of the binary are returned.
-spec run_machine(Mach::#mach{}, PickleRest::binary()) -> {term(), binary()}.
run_machine(#mach{stack=[Val], done=true} = _Mach, Rest) ->
    {Val, Rest};
run_machine(_Mach, <<>>) ->
    erlang:error(incomplete_pickle);
run_machine(Mach, Rest) ->
    {NewMach, NewRest} = step_machine(Mach, Rest),
    run_machine(NewMach, NewRest).


%% @private
%% @doc Advance the stack machine one step and return the new state
%% and the remaining pickle.
-spec step_machine(Mach::#mach{}, PickleRest::binary()) ->
                   {NewMach::#mach{}, NewPickleRest::binary()}.

%% FLOAT - text representation
step_machine(#mach{stack=Stack} = Mach, <<$F, Rest/binary>>) ->
    [BinFloat, Rest2] = binary:split(Rest, <<$\n>>),
    Float = list_to_float(binary_to_list(BinFloat)),
    {Mach#mach{stack=[Float | Stack]}, Rest2};

% 32-bit signed numbers
step_machine(Mach, <<$J, Num:32/little-signed, Rest/binary>>) ->
    NewStack = [Num | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

%% TRUE | FALSE - text protocol
step_machine(#mach{stack=Stack} = Mach, <<$I, "01\n", Rest/binary>>) ->
    {Mach#mach{stack=[true | Stack]}, Rest};
step_machine(#mach{stack=Stack} = Mach, <<$I, "00\n", Rest/binary>>) ->
    {Mach#mach{stack=[false | Stack]}, Rest};

%% INT
step_machine(Mach, <<$I, Rest/binary>>) ->
  [BinInt, Rest2] = binary:split(Rest, <<$\n>>),
  NewStack = [list_to_integer(binary_to_list(BinInt)) | Mach#mach.stack],
  {Mach#mach{stack=NewStack}, Rest2};

% byte numbers
step_machine(Mach, <<$K, Num, Rest/binary>>) ->
    NewStack = [Num | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

%% LONG
step_machine(Mach, <<$L, Rest/binary>>) ->
    [BinInt, Rest2] = binary:split(Rest, <<"L\n">>),
    NewStack = [list_to_integer(binary_to_list(BinInt)) | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest2};

% 16-bit numbers
step_machine(Mach, <<$M, Num:16/little-unsigned, Rest/binary>>) ->
    NewStack = [Num | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% long integer, 1-byte length
step_machine(Mach, <<16#8a, Len, Num:Len/little-signed-unit:8, Rest/binary>>) ->
    NewStack = [Num | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% long integer, 4-byte length
step_machine(Mach, <<16#8b, Len:32/little-signed,
                   Num:Len/little-signed-unit:8, Rest/binary>>) ->
    NewStack = [Num | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% float
step_machine(Mach, <<$G, Num/big-signed-float, Rest/binary>>) ->
    NewStack = [Num | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% none
step_machine(Mach, <<$N, Rest/binary>>) ->
    NewStack = [none | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

%% STRING
step_machine(#mach{stack=Stack} = Mach, <<$S, Rest/binary>>) ->
    [ReprString, Rest2] = binary:split(Rest, <<$\n>>),
    UnquotedReprString = case ReprString of
                             <<Q, _/binary>> when (Q == $') orelse (Q == $") ->
                                 binary:part(ReprString, 1, byte_size(ReprString) - 2);
                             _ -> ReprString
                         end,
    String = unrepr_string(UnquotedReprString),
    {Mach#mach{stack=[String | Stack]}, Rest2};

% string, 4-byte length
step_machine(Mach, <<$T, Len:32/little-signed, Bin:Len/binary, Rest/binary>>) ->
    NewStack = [Bin | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% string, 1-byte length
step_machine(Mach, <<$U, Len, Bin:Len/binary, Rest/binary>>) ->
    NewStack = [Bin | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% true
step_machine(Mach, <<16#88, Rest/binary>>) ->
    NewStack = [true | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% false
step_machine(Mach, <<16#89, Rest/binary>>) ->
    NewStack = [false | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

%% LIST - build list from items on stack; like $e, but creates a new list
step_machine(#mach{stack=Stack} = Mach, <<$l, Rest/binary>>) ->
    {Slice, Stack1} = pop_to_mark(Stack),    % 'Slice' is empty in modern pickle
    NewList = add_slice([], Slice),
    {Mach#mach{stack=[{list, NewList} | Stack1]}, Rest};

% empty list
step_machine(Mach, <<$], Rest/binary>>) ->
    NewStack = [{list, []} | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% list append
step_machine(Mach, <<$a, Rest/binary>>) ->
    [Val, {list, List} | Stack] = Mach#mach.stack,
    NewList = [finalize(Val) | List],
    NewStack = [{list, NewList} | Stack],
    {Mach#mach{stack=NewStack}, Rest};

% list extend
step_machine(Mach, <<$e, Rest/binary>>) ->
    {Slice, [{list, List} | Stack]} = pop_to_mark(Mach#mach.stack),
    NewList = add_slice(List, Slice),
    NewStack = [{list, NewList} | Stack],
    {Mach#mach{stack=NewStack}, Rest};

% mark object
step_machine(Mach, <<$(, Rest/binary>>) ->
    NewStack = [mark | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% empty tuple
step_machine(Mach, <<$), Rest/binary>>) ->
    NewStack = [{} | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% 1-tuple
step_machine(Mach, <<16#85, Rest/binary>>) ->
    [Val | Stack] = Mach#mach.stack,
    NewStack = [{finalize(Val)} | Stack],
    {Mach#mach{stack=NewStack}, Rest};

% 2-tuple
step_machine(Mach, <<16#86, Rest/binary>>) ->
    [V2, V1 | Stack] = Mach#mach.stack,
    NewStack = [{finalize(V1), finalize(V2)} | Stack],
    {Mach#mach{stack=NewStack}, Rest};

% 3-tuple
step_machine(Mach, <<16#87, Rest/binary>>) ->
    [V3, V2, V1 | Stack1] = Mach#mach.stack,
    NewStack = [{finalize(V1), finalize(V2), finalize(V3)} | Stack1],
    {Mach#mach{stack=NewStack}, Rest};

% tuple
step_machine(Mach, <<$t, Rest/binary>>) ->
    {Slice, Stack} = pop_to_mark(Mach#mach.stack),
    Tuple = erlang:list_to_tuple([finalize(V) || V <- Slice]),
    NewStack = [Tuple | Stack],
    {Mach#mach{stack=NewStack}, Rest};

%% DICT - build dict from items on stack; like $u, but creates a new dict
step_machine(#mach{stack=Stack} = Mach, <<$d, Rest/binary>>) ->
    {Slice, Stack1} = pop_to_mark(Stack),    % 'Slice' is empty in modern pickle
    KeyVals = [finalize(T) || T <- Slice],
    NewStack = set_items([{dictionary, dict:new()} | Stack1], KeyVals),
    {Mach#mach{stack=NewStack}, Rest};

% empty dictionary
step_machine(Mach, <<$}, Rest/binary>>) ->
    NewStack = [{dictionary, dict:new()} | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% set item on dictionary
step_machine(Mach, <<$s, Rest/binary>>) ->
    [V, K, {dictionary, D} | Stack] = Mach#mach.stack,
    NewDict = dict:store(finalize(K), finalize(V), D),
    NewStack = [{dictionary, NewDict} | Stack],
    {Mach#mach{stack=NewStack}, Rest};

% set items on dictionary
step_machine(Mach, <<$u, Rest/binary>>) ->
    {Slice, Stack} = pop_to_mark(Mach#mach.stack),
    KeyVals = [finalize(T) || T <- Slice],
    NewStack = set_items(Stack, KeyVals),
    {Mach#mach{stack=NewStack}, Rest};

% BINPUT set to memo
step_machine(Mach, <<$q, Index, Rest/binary>>) ->
    [Val | _] = Mach#mach.stack,
    NewMemo = dict:store(Index, Val, Mach#mach.memo),
    {Mach#mach{memo=NewMemo}, Rest};

%% BINGET
step_machine(Mach, <<$h, Index, Rest/binary>>) ->
    Obj = dict:fetch(Index, Mach#mach.memo),
    NewStack = [Obj | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

%% global
step_machine(Mach, <<$c, Rest/binary>>) ->
    [Module, Rest2] = binary:split(Rest, <<"\n">>),
    [Name, Rest3] = binary:split(Rest2, <<"\n">>),
    NewStack = [#pickle_global{module=Module, name=Name} | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest3};

%% NEWOBJ
step_machine(Mach, <<16#81, Rest/binary>>) ->
    [Args, Cls | Stack1] = Mach#mach.stack,
    NewStack = [#pickle_object{class=Cls, new_args=Args} | Stack1],
    {Mach#mach{stack=NewStack}, Rest};

%% BUILD
step_machine(Mach, <<$b, Rest/binary>>) ->
    [State, Object | Stack1] = Mach#mach.stack,
    NewStack = [Object#pickle_object{state=finalize(State)} | Stack1],
    {Mach#mach{stack=NewStack}, Rest};

%% BINUNICODE
step_machine(Mach, <<$X, Len:32/integer-little, Rest/binary>>) ->
    <<Unicoded:Len/binary, Rest2/binary>> = Rest,
    NewStack = [#pickle_unicode{value = Unicoded} | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest2};

% end of pickle
step_machine(Mach, <<$., Rest/binary>>) ->
    [Val] = Mach#mach.stack,
    {Mach#mach{stack=[finalize(Val)], done=true}, Rest}.


%% @private
%% @doc Transform an intermediate value created by
%% the stack machine into its final form. Mainly used
%% to reverse lists.
-spec finalize(Val::term()) -> term().
finalize({list, List}) ->
    lists:reverse(List);
finalize({dictionary, Dict}) ->
    Dict;
finalize(Val) ->
    Val.


%% @private
%% @doc Pop items off the stack until the mark is reached.
%% The mark is discarded. A tuple with the slice of elements
%% in front of the mark and the stack without the slice or
%% the mark is returned.
-spec pop_to_mark(Stack::list()) -> {Slice::list(), NewStack::list()}.
pop_to_mark(Stack) ->
    pop_to_mark([], Stack).

pop_to_mark(Slice, [mark | Stack]) ->
    {Slice, Stack};
pop_to_mark(Slice, [Val | Stack]) ->
    pop_to_mark([Val | Slice], Stack).


%% @private
%% @doc Adds the slice to the list. The new list is returned.
-spec add_slice(List::list(), Slice::list()) -> NewList::list().
add_slice(List, []) ->
    List;
add_slice(List, [Val | Slice]) ->
    add_slice([finalize(Val) | List], Slice).

%% @private
%% @doc Add the key-val pairs to the dict at the top of the stack.
-spec set_items(Stack::list(), KeyVals::list()) -> NewStack::list().
set_items([{dictionary, Dict} | Stack], KeyVals) ->
    NewDict = update_dict(Dict, KeyVals),
    [{dictionary, NewDict} | Stack].

%% @private
%% @doc Add the key-val pairs to the given dict. Return the new dict.
-spec update_dict(Dict::dict(), KeyVals::list()) -> NewStack::list().
update_dict(Dict, []) ->
    Dict;
update_dict(Dict, [Key, Val | KeyVals]) ->
    update_dict(dict:store(Key, Val, Dict), KeyVals).


%% @private
%% @doc encode the given Term as a Python pickle.
-spec start_encode(Term::term(), Encoder::#encoder{}) -> Pickle::binary().
start_encode(Term, #encoder{proto=Proto} = Encoder) ->
    Initial = if Proto >= 2 ->
                      [<<16#80, Proto>>];
                 true ->
                      []
              end,
    IOList = finish_encode(encode_term(Term, Initial, Encoder)),
    iolist_to_binary(lists:reverse(IOList)).

%% @private
%% @doc encode a single Term into a Python pickle, given the
%% pickle encoded so far as a reversed iolist.
-spec encode_term(Term::term(), PickleSoFar::iolist(), Encoder::#encoder{}) ->
                         Pickle::iolist().

% None
encode_term(none, Pickle, _) ->
    ["N" | Pickle];

% True
encode_term(true, Pickle, #encoder{proto=P}) when P >= 2 ->
    [16#88 | Pickle];
encode_term(true, Pickle, _) ->
    [<<"I01\n">> | Pickle];

% False
encode_term(false, Pickle, #encoder{proto=P}) when P >= 2 ->
    [16#89 | Pickle];
encode_term(false, Pickle, _) ->
    [<<"I00\n">> | Pickle];

%% floats in text protocol
encode_term(Number, Pickle, #encoder{bin=false}) when is_float(Number) ->
    %% float_to_binary/1 creates more verbose output
    [<<$F, (iolist_to_binary(io_lib:format("~w", [Number])))/binary, $\n>> | Pickle];

%% integer / long in text protocol
encode_term(Number, Pickle, #encoder{bin=false})
  when is_integer(Number), not(?IS_LONG(Number)) ->
    %% signed int
    %% [-2147483648, 2147483647]
    [<<$I, (list_to_binary(integer_to_list(Number)))/binary, $\n>> | Pickle];
encode_term(Number, Pickle, #encoder{proto=P})
  when is_integer(Number), ?IS_LONG(Number), (P < 2) ->
    %% signed long
    [<<$L, (list_to_binary(integer_to_list(Number)))/binary, $L, $\n>> | Pickle];

% floats
encode_term(Number, Pickle, _) when is_float(Number) ->
    [<<$G, Number/big-signed-float>> | Pickle];

% tiny integers
encode_term(Number, Pickle, _)
  when is_number(Number), Number >= 0, Number =< 255 ->
    [<<$K, Number>> | Pickle];

% medium integers
encode_term(Number, Pickle, _)
  when is_number(Number), Number >= 256, Number =< 65535 ->
    [<<$M, Number:16/little-unsigned>> | Pickle];

% big integers
encode_term(Number, Pickle, _)
  when is_number(Number), not(?IS_LONG(Number)) ->
    [<<$J, Number:32/little-signed>> | Pickle];

% empty tuples
encode_term({}, Pickle, #encoder{proto=0}) ->
    [<<$(, $t>> | Pickle];
encode_term({}, Pickle, _) ->
    [$) | Pickle];

% dictionaries
encode_term(Dict, Pickle, #encoder{bin=true} = Encoder)
  when is_tuple(Dict), element(1, Dict) =:= dict ->
    NewPickle = [$} | Pickle],
    case dict:size(Dict) of
        0 ->
            NewPickle;
        _ ->
            [$u | encode_dict(Dict, [$( | NewPickle], Encoder)]
    end;
encode_term(Dict, Pickle, Encoder)
  when is_tuple(Dict), element(1, Dict) =:= dict ->
    encode_dict(Dict, [<<$(, $d>> | Pickle], Encoder);

% long integers
encode_term(Number, Pickle, _) when is_number(Number) ->
    Encoding = encode_long(Number, <<>>),
    Header = long_header(byte_size(Encoding)),
    [Encoding, Header | Pickle];

% binaries (Python strings)
encode_term(Binary, Pickle, #encoder{bin=false}) when is_binary(Binary) ->
    [$\n, $', repr_string(Binary), $', $S | Pickle];
encode_term(Binary, Pickle, _) when is_binary(Binary) ->
    P1 = [string_header(size(Binary)) | Pickle],
    [Binary | P1];

% 1-tuples
encode_term({Term}, Pickle, #encoder{proto=P} = Encoder) when P >= 2 ->
    [16#85 | encode_term(Term, Pickle, Encoder)];

% 2-tuples
encode_term({T1, T2}, Pickle, #encoder{proto=P} = Encoder) when P >= 2 ->
    [16#86 | encode_term(T2, encode_term(T1, Pickle, Encoder), Encoder)];

% 3-tuples
encode_term({T1, T2, T3}, Pickle, #encoder{proto=P} = Encoder) when P >= 2 ->
    T1P = encode_term(T1, Pickle, Encoder),
    T2P = encode_term(T2, T1P, Encoder),
    [16#87 | encode_term(T3, T2P, Encoder)];

% big tuples or proto < 2
encode_term(Tuple, Pickle, Encoder) when is_tuple(Tuple) ->
    [$t | encode_tuple(Tuple, [$( | Pickle], 1, Encoder)];

% empty lists
encode_term([], Pickle, #encoder{bin=false}) ->
    [<<$(, $l>> | Pickle];
encode_term([], Pickle, _) ->
    [$] | Pickle];

% non-empty lists
encode_term(List, Pickle, #encoder{bin=true} = Encoder) when is_list(List) ->
    encode_list_bin(List, [$(, $] | Pickle], Encoder);
encode_term(List, Pickle, #encoder{bin=false} = Encoder) when is_list(List) ->
    encode_list_txt(List, [<<$(, $l>> | Pickle], Encoder).

%% @private
%% @doc finish the encoding
-spec finish_encode(Pickle::iolist()) -> Pickle::iolist().
finish_encode(Pickle) ->
    ["." | Pickle].

%% @private
%% @doc Return the appropriate control code for a Python string
%% of the given length.
-spec string_header(Length::non_neg_integer()) -> binary().
string_header(Length) when Length < 256 ->
    <<$U, Length>>;
string_header(Length) ->
    <<$T, Length:32/little-signed>>.

%% @private
%% @doc Return the appropriate control code for a Python Long
%% of the given length in bytes.
-spec long_header(NumBytes::pos_integer()) -> binary().
long_header(NumBytes) when NumBytes >= 1, NumBytes =< 255 ->
    <<16#8a, NumBytes>>;
long_header(NumBytes) ->
    <<16#8b, NumBytes:32/little-signed>>.

%% @private
%% @doc Return the encoding for a Python Long
-spec encode_long(Number::number(), Bytes::list(byte())) -> tuple().
encode_long(Number, Bytes) when Number >= -128, Number =< 127 ->
    <<Bytes/binary, Number>>;
encode_long(Number, Bytes) ->
    encode_long(Number bsr 8, <<Bytes/binary, (Number band 255)>>).

%% @private
%% @doc Add the encoding for a tuple to the given iolist,
%% except for the initial control character.
-spec encode_tuple(Tuple::tuple(), Pickle::iolist(),
                   Index::number(), Encoder::#encoder{}) -> iolist().
encode_tuple(Tuple, Pickle, Index, _) when Index > size(Tuple) ->
    Pickle;
encode_tuple(Tuple, Pickle, Index, Encoder) ->
    PElement = encode_term(element(Index, Tuple), Pickle, Encoder),
    encode_tuple(Tuple, PElement, Index + 1, Encoder).

%% @private
%% @doc Add the encoding for a list to the given iolist; protocol > 0
-spec encode_list_bin(List::list(), Pickle::iolist(), Encoder::#encoder{}) -> iolist().
encode_list_bin([], Pickle, _) ->
    [$e | Pickle];
encode_list_bin([Term | List], Pickle, Encoder) ->
    PElement = encode_term(Term, Pickle, Encoder),
    encode_list_bin(List, PElement, Encoder).

%% @private
%% @doc Add the encoding for a list to the given iolist; protocol 0
encode_list_txt([], Pickle, _) ->
    Pickle;
encode_list_txt([Term | List], Pickle, Encoder) ->
    PElement = [$a | encode_term(Term, Pickle, Encoder)],
    encode_list_txt(List, PElement, Encoder).


%% @private
%% @doc Add the encoding for a dictionary to the given iolist.
-spec encode_dict(Dict::dict(), Pickle::iolist(), Encoder::#encoder{}) -> iolist().
encode_dict(Dict, Pickle, Encoder) ->
    {ResPickle, _} = dict:fold(fun encode_dict_kv/3, {Pickle, Encoder}, Dict),
    ResPickle.

%% @private
%% @doc Add the encoding for a dictionary key/value pair to the given iolist.
-spec encode_dict_kv(Key::term(), Value::term(), Pickle::iolist()) -> iolist().
encode_dict_kv(Key, Value, {Pickle, #encoder{bin=true} = Encoder}) ->
    PKey = encode_term(Key, Pickle, Encoder),
    {encode_term(Value, PKey, Encoder), Encoder};
encode_dict_kv(Key, Value, {Pickle, Encoder}) ->
    PKey = encode_term(Key, Pickle, Encoder),
    {[$s | encode_term(Value, PKey, Encoder)], Encoder}.



%% @private
%% @doc like python's repr() for binary string
-spec repr_string(ByteString::binary()) -> ReprString::binary().
repr_string(Bin) ->
    << <<(repr_char(C))/binary>> || <<C>> <= Bin>>.

repr_char($\t) -> <<"\\t">>;
repr_char($\n) -> <<"\\n">>;
repr_char($\r) -> <<"\\r">>;
repr_char($\\) -> <<"\\\\">>;
repr_char($') -> <<"\\'">>;
repr_char(C) when (C >= 32), (C =< $~) -> <<C>>;
repr_char(C) ->
    HChar = fun(N) when N < 10 -> $0 + N;
               (N) when N < 16 -> $W + N
            end,
    <<$\\, $x, (HChar(C div 16)), (HChar(C rem 16))>>.

%% @private
%% @doc like python's str.decode("string-escape")
-spec unrepr_string(ReprString::binary()) -> ByteString::binary().
unrepr_string(<<"\\t", Rest/binary>>) ->
    <<$\t, (unrepr_string(Rest))/binary>>;
unrepr_string(<<"\\n", Rest/binary>>) ->
    <<$\n, (unrepr_string(Rest))/binary>>;
unrepr_string(<<"\\r", Rest/binary>>) ->
    <<$\r, (unrepr_string(Rest))/binary>>;
unrepr_string(<<"\\\\", Rest/binary>>) ->
    <<$\\, (unrepr_string(Rest))/binary>>;
unrepr_string(<<"\\'", Rest/binary>>) ->
    <<$', (unrepr_string(Rest))/binary>>;
unrepr_string(<<"\\\"", Rest/binary>>) ->
    <<$", (unrepr_string(Rest))/binary>>;
unrepr_string(<<$\\, $x, Hex1, Hex2, Rest/binary>>) ->
    UnHChar = fun(C) when C < $W -> C - $0;
                 (C) when C > $W -> C - $W
              end,
    <<(UnHChar(Hex1)):4, (UnHChar(Hex2)):4, (unrepr_string(Rest))/binary>>;
unrepr_string(<<C, Rest/binary>>) ->
    <<C, (unrepr_string(Rest))/binary>>;
unrepr_string(<<>>) -> <<>>.

% Tests

-ifdef(TEST).

big_string_pickle() ->
    Start = <<16#80, 2, $T, 0, 1, 0, 0>>,
    Middle = big_string(),
    <<Start/binary, Middle/binary, $.>>.

big_string() ->
    binary:copy(<<$a>>, 256).

pickle_to_term_test_() ->
    [
     ?_assertError(incomplete_pickle, pickle_to_term(<<16#80, 2>>)),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $I, "255", $\n, $.>>), 255),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $J, 0, 0, 0, 0, $.>>), 0),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $J, 255, 0, 0, 0, $.>>), 255),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $J, 0, 0, 0, 128, $.>>),
                   trunc(-math:pow(2, 31))),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $J, 255, 255, 255, 255, $.>>), -1),

     ?_assertEqual(pickle_to_term(<<16#80, 2, $K, 1, $.>>), 1),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $K, 255, $.>>), 255),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $M, 255, 0, $.>>), 255),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $M, 255, 255, $.>>), 65535),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $N, $.>>), none),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $T, 0, 0, 0, 0, $q, 0, $.>>),
                   <<>>),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $T, 3, 0, 0, 0,
                                    "abc", $q, 0, $.>>), <<"abc">>),
     ?_assertEqual(pickle_to_term(big_string_pickle()), big_string()),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $U, 0, $q, 0, $.>>), <<>>),
     ?_assertEqual(pickle_to_term(<<16#80, 2, $U, 1, $a, $q, 0, $.>>), <<"a">>),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8a, 0, $.>>), 0),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8a, 1, 1, $.>>), 1),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8a, 2, 255, 0, $.>>), 255),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8a, 4, 16#80,
                                    16#96, 16#98, 16#0, $.>>), 10000000),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8a, 1, 16#f1, $.>>), -15),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8b, 2, 0, 0, 0, 255, 0, $.>>), 255),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8b, 2, 0, 0, 0, 255, 16#7f, $.>>), 32767),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8b, 2, 0, 0, 0, 0, 255, $.>>), -256),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#8b, 0, 0, 0, 0, $.>>), 0),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#88, $.>>), true),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 16#89, $.>>), false),
     ?_assertEqual(pickle_to_term(<<16#80, 2, "]", $.>>), []),
     ?_assertEqual(pickle_to_term(<<16#80, 2, "]", "q", 0, "K", 1, "a", $.>>), [1]),
     ?_assertEqual(pickle_to_term(<<16#80, 2, "]", "q", 0,
                                    "(", "K", 1, "K", 2, "e", $.>>), [1, 2]),
     ?_assertEqual(pickle_to_term(<<16#80, 2, ")", $.>>), {}),
     ?_assertEqual(pickle_to_term(<<16#80, 2, "K", 1, 16#85, "q", 0, $.>>), {1}),
     ?_assertEqual(pickle_to_term(<<16#80, 2, "K", 1, "K", 2, 16#86, "q", 0, $.>>), {1,2}),
     ?_assertEqual(pickle_to_term(<<16#80, 2, "K", 1, "K", 2, "K", 3,
                                    16#87, "q", 0, $.>>), {1,2,3}),
     ?_assertEqual(pickle_to_term(<<16#80, 2, "(", "K", 1, "K", 2, "K", 3, "K", 4,
                                    "t", "q", 0, $.>>), {1,2,3,4}),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 93, 113, 0, 40, 75, 1, 133, 113, 1, 93,
                                    113, 2, 75, 2, 97, 101, 46>>), [{1}, [2]]),
     ?_assertEqual(pickle_to_term(<<128, 2, 75, 1, 93, 113, 0, 40, 75, 2,
                                    75, 3, 101, 75, 4, 135, 113, 1, 46>>), {1, [2, 3], 4}),
     ?_assertEqual(pickle_to_term(<<128, 2, 75, 1, 93, 113, 0, 40, 75, 2,
                                    75, 3, 101, 134, 113, 1, 46>>), {1, [2, 3]}),
     ?_assertEqual(pickle_to_term(<<128, 2, 93, 113, 0, 40, 75, 2,
                                    75, 3, 101, 133, 113, 1, 46>>), {[2, 3]}),
     ?_assertEqual(pickle_to_term(<<128, 2, 40, 75, 1, 93, 113, 0, 40, 75, 2,
                                    75, 3, 101, 75, 4, 75, 5, 116, 113,
                                    1, 46>>), {1, [2, 3], 4, 5}),
     ?_assertEqual(pickle_to_term(<<128, 2, 93, 113, 0, 75, 2, 75, 3, 134,
                                    113, 1, 97, 46>>), [{2, 3}]),
     ?_assert(dicts_are_same(pickle_to_term(<<128, 2, 125, 113, 0, 46>>), dict:new())),
     ?_assert(dicts_are_same(pickle_to_term(<<128, 2, 125, 113, 0, 75,
                                              1, 75, 2, 115, 46>>),
                             dict:store(1, 2, dict:new()))),
     ?_assert(dicts_are_same(pickle_to_term(<<128, 2, 125, 113, 0, 75, 1, 75, 2,
                                              134, 113, 1, 75, 3, 115, 46>>),
                             dict:store({1, 2}, 3, dict:new()))),
     ?_assert(dicts_are_same(pickle_to_term(<<128, 2, 125, 113, 0, 75, 1, 75, 2,
                                              134, 113, 1, 93, 113, 2, 40, 75, 3,
                                              75, 4, 101, 115, 46>>),
                             dict:store({1, 2}, [3, 4], dict:new()))),
     ?_assert(dicts_are_same(pickle_to_term(<<128, 2, 125, 113, 0, 40, 75, 0, 78, 75,
                                              1, 78, 75, 2, 78, 75, 3, 78, 75, 4, 78,
                                              75, 5, 78, 75, 6, 78, 75, 7, 78, 75, 8,
                                              78, 75, 9, 78, 117, 46>>),
                             dict_from_keys(lists:seq(0, 9)))),
     ?_assertEqual(pickle_to_term(<<16#80, 2, 139, 1, 1, 0:258/unit:8, 1, $.>>), 1 bsl 2048),
     ?_assertEqual(pickle_to_term(<<128, 2, 71, 0, 0, 0, 0, 0, 0, 0, 0, 46>>), 0.0),
     ?_assertEqual(pickle_to_term(<<128, 2, 71, 63, 240, 0, 0, 0, 0, 0, 0, 46>>), 1.0),
     ?_assertEqual(pickle_to_term(<<128, 2, 71, 191, 240, 0, 0, 0, 0, 0, 0, 46>>), -1.0),
     ?_assertEqual(pickle_to_term(<<128, 2, 71, 63, 248, 0, 0, 0, 0, 0, 0, 46>>), 1.5),
     ?_assertEqual(pickle_to_term(<<128, 2, 71, 68, 21, 175, 29, 120, 181, 140, 64, 46>>), 1.0e20),
     ?_assertEqual(pickle_to_term(<<128, 2, 71, 196, 21, 175, 29, 120, 181, 140, 64, 46>>), -1.0e20),
     %% pickle.dumps(sum, protocol=2)
     ?_assertEqual(pickle_to_term(<<16#80, 2, $c, "__builtin__", $\n, "sum", $\n, $q, 0, $.>>),
                   #pickle_global{module = <<"__builtin__">>, name = <<"sum">>}),
     %% class MyClass(object):
     %% def __init__(self, arg, kwarg=None):
     %%     self.arg = arg
     %%     self.kwarg = kwarg
     %% def __getstate__(self):
     %%     return (self.arg, self.kwarg)
     %% pickle.dumps(MyClass(1, 2), protocol=2)
     ?_assertEqual(pickle_to_term(
                     <<128,2,99,95,95,109,97,105,110,95,95,10,77,121,67,108,97,115,
                       115,10,113,0,41,129,113,1,75,1,75,2,134,113,2,98,46>>),
                   #pickle_object{class = #pickle_global{module = <<"__main__">>,
                                                         name = <<"MyClass">>},
                                  new_args = {},
                                  state = {1, 2}}),
     %% class MyClass(object):
     %% def __init__(self, arg, kwarg=None):
     %%     self.arg = arg
     %%     self.kwarg = kwarg
     %% pickle.dumps(MyClass(1, 2), protocol=2)
     ?_assertEqual(pickle_to_term(
                     <<128,2,99,95,95,109,97,105,110,95,95,10,77,121,67,108,97,115,
                       115,10,113,0,41, 129,113,1,125,113,2,40,85,5,107,119,97,114,
                       103,113,3,75,2,85,3,97,114,103,113,4,75,1,117,98,46>>),
                   #pickle_object{class = #pickle_global{module = <<"__main__">>,
                                                         name = <<"MyClass">>},
                                  new_args = {},
                                  state = dict:from_list(
                                            [{<<"arg">>, 1}, {<<"kwarg">>, 2}])}),
     %% pickle.dumps(
     %%     [u'\u041f\u0440\u0438\u0432\u0435\u0442, \u043c\u0438\u0440!'],
     %%     protocol=2)
     ?_assertEqual(pickle_to_term(
                     <<128,2,93,113,0,88,21,0,0,0,208,159,209,128,208,184,208,178,208,
                       181,209,130,44,32,208,188,208,184,209,128,33,113,1,97,46>>),
                   [#pickle_unicode{
                       value = <<208,159,209,128,208,184,208,178,208,
                                 181,209,130,44,32,208,188,208,184,209,128,33>>}]),
     %% o = []
     %% pickle.dumps((o, o), protocol=2)
     ?_assertEqual(pickle_to_term(<<16#80, 2, $], $q, 0, $h, 0, 16#86, $q, 1, $.>>),
             {[], []}),

     %% pickle < 2 - no protocol version prefix
     ?_assertEqual(<<"qwe'\"rty">>,
                   pickle_to_term(<<"S\"qwe\\'\\\"rty\"\n.">>)),

     %% test for 'l' opcode (looks like such pickle never generated in practice)
     ?_assertEqual([1, 2, 3], pickle_to_term(<<"(I1\nI2\nI3\nl.">>)),

     %% test for 'd' opcode (looks like such pickle never generated in practice)
     ?_assert(dicts_are_same(pickle_to_term(<<"(I1\nI2\nd.">>),
                             dict:from_list([{1, 2}]))),

     %% most likely never generated in practice
     ?_assertEqual(<<"qwerty">>, pickle_to_term(<<"Sqwerty\n.">>))
     ].

term_to_pickle_test_() ->
    [
     ?_assertEqual(term_to_pickle(0), <<16#80, 2, "K", 0, $.>>),
     ?_assertEqual(term_to_pickle(255), <<16#80, 2, "K", 255, $.>>),
     ?_assertEqual(term_to_pickle(256), <<16#80, 2, "M", 0, 1, $.>>),
     ?_assertEqual(term_to_pickle(65535), <<16#80, 2, "M", 255, 255, $.>>),
     ?_assertEqual(term_to_pickle(-1), <<16#80, 2, "J", 255, 255, 255, 255, $.>>),
     ?_assertEqual(term_to_pickle(65536), <<16#80, 2, "J", 0, 0, 1, 0, $.>>),
     %% in Python the same int may be encoded as LONG or INT depending on CPU
     %% architecture (i686 x86_64).
     %% So, representation is platform-dependent.
     %% We use i686 (32bit) one.
     ?_assertEqual(term_to_pickle(-1 bsl 2048),
                   <<16#80, 2, 139, 1, 1, 0:258/unit:8, 255, $.>>),
     ?_assertEqual(<<16#80, 2, 138, 5, 255, 255, 255, 127, 255, $.>>,
                   term_to_pickle(-2147483649)),
     ?_assertEqual(<<16#80, 2, $J, 0, 0, 0, 128, $.>>,
                   term_to_pickle(-2147483648)),
     ?_assertEqual(<<16#80, 2, $J, 255, 255, 255, 127, $.>>,
                  term_to_pickle(2147483647)),
     ?_assertEqual(<<16#80, 2, 138, 5, 0, 0, 0, 128, 0, $.>>,
                  term_to_pickle(2147483648)),
     ?_assertEqual(term_to_pickle(1 bsl 2048),
                   <<16#80, 2, 139, 1, 1, 0:258/unit:8, 1, $.>>),

     ?_assertEqual(term_to_pickle(none), <<16#80, 2, "N", $.>>),
     ?_assertEqual(term_to_pickle(true), <<16#80, 2, 16#88, $.>>),
     ?_assertEqual(term_to_pickle(false), <<16#80, 2, 16#89, $.>>),
     ?_assertEqual(term_to_pickle(<<"test">>), <<16#80, 2, "U", 4, "test", $.>>),
     ?_assertEqual(term_to_pickle(big_string()), big_string_pickle()),
     ?_assertEqual(term_to_pickle({}), <<16#80, 2, $), $.>>),
     ?_assertEqual(term_to_pickle({1}), <<16#80, 2, $K, 1, 16#85, $.>>),
     ?_assertEqual(term_to_pickle({1, 2}), <<16#80, 2, $K, 1, $K, 2, 16#86, $.>>),
     ?_assertEqual(term_to_pickle({1, 2, 3}), <<16#80, 2, $K, 1, $K, 2, $K, 3, 16#87, $.>>),
     ?_assertEqual(term_to_pickle({1, 2, 3, 4}),
                   <<16#80, 2, $(, $K, 1, $K, 2, $K, 3, $K, 4, $t, $.>>),
     ?_assertEqual(term_to_pickle([]), <<16#80, 2, $], $.>>),
     ?_assertEqual(term_to_pickle([1, 2, 3]),
                   <<16#80, 2, $], $(, $K, 1, $K, 2, $K, 3, $e, $.>>),
     ?_assertEqual(term_to_pickle(dict:new()), <<16#80, 2, $}, $.>>),
     ?_assertEqual(term_to_pickle(dict:from_list([{1,2}])),
                   <<16#80, 2, $}, $(, $K, 1, $K, 2, $u, $.>>),
     ?_assertEqual(term_to_pickle(0.0), <<128, 2, 71, 0, 0, 0, 0, 0, 0, 0, 0, 46>>),
     ?_assertEqual(term_to_pickle(1.0), <<128, 2, 71, 63, 240, 0, 0, 0, 0, 0, 0, 46>>),
     ?_assertEqual(term_to_pickle(-1.0), <<128, 2, 71, 191, 240, 0, 0, 0, 0, 0, 0, 46>>),
     ?_assertEqual(term_to_pickle(1.5), <<128, 2, 71, 63, 248, 0, 0, 0, 0, 0, 0, 46>>),
     ?_assertEqual(term_to_pickle(1.0e20), <<128, 2, 71, 68, 21, 175, 29, 120, 181, 140, 64, 46>>),
     ?_assertEqual(term_to_pickle(-1.0e20), <<128, 2, 71, 196, 21, 175, 29, 120, 181, 140, 64, 46>>)
].

term_to_pickle_v0_test_() ->
    TTP0 = fun(Term) -> term_to_pickle(Term, 0) end,
    [
     %% booleans
     ?_assertEqual(<<"I01\n.">>, TTP0(true)),
     ?_assertEqual(<<"I00\n.">>, TTP0(false)),
     %% floats
     ?_assertEqual(<<"F1.1\n.">>, TTP0(1.1)),
     ?_assertEqual(<<"F-1.1\n.">>, TTP0(-1.1)),
     %% int / long int
     ?_assertEqual(<<"I1\n.">>, TTP0(1)),
     ?_assertEqual(<<"L2147483648L\n.">>, TTP0(2147483648)),
     ?_assertEqual(<<"L-2147483649L\n.">>, TTP0(-2147483649)),
     ?_assertEqual(<<"I-2147483648\n.">>, TTP0(-2147483648)),
     %% bytestring
     %% pickletools.optimize(pickle.dumps(''.join(chr(i) for i in range(256)), protocol=0))
     ?_assertEqual(
        <<"S\'\\x00\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\t\\n\\x0b\\x0c\\r"
          "\\x0e\\x0f\\x10\\x11\\x12\\x13\\x14\\x15\\x16\\x17\\x18\\x19\\x1a"
          "\\x1b\\x1c\\x1d\\x1e\\x1f !\"#$%&\\\'()*+,-./0123456789:;<=>?@ABCDEF"
          "GHIJKLMNOPQRSTUVWXYZ[\\\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\\x7f"
          "\\x80\\x81\\x82\\x83\\x84\\x85\\x86\\x87\\x88\\x89\\x8a\\x8b\\x8c"
          "\\x8d\\x8e\\x8f\\x90\\x91\\x92\\x93\\x94\\x95\\x96\\x97\\x98\\x99"
          "\\x9a\\x9b\\x9c\\x9d\\x9e\\x9f\\xa0\\xa1\\xa2\\xa3\\xa4\\xa5\\xa6"
          "\\xa7\\xa8\\xa9\\xaa\\xab\\xac\\xad\\xae\\xaf\\xb0\\xb1\\xb2\\xb3"
          "\\xb4\\xb5\\xb6\\xb7\\xb8\\xb9\\xba\\xbb\\xbc\\xbd\\xbe\\xbf\\xc0"
          "\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6\\xc7\\xc8\\xc9\\xca\\xcb\\xcc\\xcd"
          "\\xce\\xcf\\xd0\\xd1\\xd2\\xd3\\xd4\\xd5\\xd6\\xd7\\xd8\\xd9\\xda"
          "\\xdb\\xdc\\xdd\\xde\\xdf\\xe0\\xe1\\xe2\\xe3\\xe4\\xe5\\xe6\\xe7"
          "\\xe8\\xe9\\xea\\xeb\\xec\\xed\\xee\\xef\\xf0\\xf1\\xf2\\xf3\\xf4"
          "\\xf5\\xf6\\xf7\\xf8\\xf9\\xfa\\xfb\\xfc\\xfd\\xfe\\xff\'\n.">>,
        TTP0(<< <<C>> || C <- lists:seq(0, 255) >>)),
     %% tuples
     ?_assertEqual(<<"(t.">>, TTP0({})),
     ?_assertEqual(<<"(I1\nt.">>, TTP0({1})),
     ?_assertEqual(<<"(I1\nI2\nI3\nI4\nt.">>, TTP0({1, 2, 3, 4})),
     %% lists
     ?_assertEqual(<<"(l.">>, TTP0([])),
     ?_assertEqual(<<"(lI1\naI2\naI3\na.">>, TTP0([1, 2, 3])),
     %% dict; XXX: dict traversal order isn't defined, so this may fall at some point
     ?_assertEqual(<<"(dI1\nI1\nsI2\nI2\nsI3\nI3\ns.">>,
                   TTP0(dict:from_list([{1, 1}, {2, 2}, {3, 3}])))
    ].

term_to_pickle_v1_test_() ->
    TTP1 = fun(Term) -> term_to_pickle(Term, 1) end,
    [
     ?_assertEqual(<<"L-2147483649L\n.">>, TTP1(-2147483649)),
     ?_assertEqual(<<").">>, TTP1({})),
     ?_assertEqual(<<"(K\x01t.">>, TTP1({1})),
     ?_assertEqual(<<"(K\x01K\x02K\x03K\x04t.">>, TTP1({1, 2, 3, 4}))
    ].

codec_test_() ->
    Terms =
        [0,
         255,
         256,
         65535,
         -1,
         65536,
         -2147483649,
         ?MININT,
         ?MAXINT,
         2147483648,
         none,
         true,
         false,
         <<>>,
         <<"test">>,
         << <<C>> || C <- lists:seq(0, 255) >>,
         {},
         {1},
         {1, 2},
         {1, 2, 3},
         {1, 2, 3, 4},
         [],
         [1, 2, 3],
         0.0,
         1.0,
         -1.0,
         [0.5, [0], {none, <<>>, true}, false]
        ],
    [{iolist_to_binary(io_lib:format("Protocol #~p, ~p", [Protocol, Term])),
      ?_assertEqual(Term, pickle_to_term(term_to_pickle(Term, Protocol)))}
     || Term <- Terms, Protocol <- [0, 1, 2]].

codec_dict_test_() ->
    Dicts = [
             dict:new(),
             dict:from_list([{1, 1}, {2, 2}])
            ],
    [{iolist_to_binary(io_lib:format("Protocol #~p, dict:from_list(~p)",
                                     [Protocol, dict:to_list(Term)])),
      ?_assert(dicts_are_same(Term, pickle_to_term(term_to_pickle(Term, Protocol))))}
     || Term <- Dicts, Protocol <- [0, 1, 2]].

dicts_are_same(D1, D2) ->
    S = dict:size(D1),
    S = dict:size(D2),
    Check = fun(Key) ->
                    {ok, V} = dict:find(Key, D1),
                    {ok, V} =:= dict:find(Key, D2)
            end,
    lists:all(Check, dict:fetch_keys(D1)).

dict_from_keys(Keys) ->
    dict_from_keys(Keys, dict:new()).

dict_from_keys([], D) ->
    D;
dict_from_keys([Key | Keys], D) ->
    dict_from_keys(Keys, dict:store(Key, none, D)).

-endif.
