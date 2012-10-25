%%% @author Dave Peticolas <dave@krondo.com>
%%% @copyright (C) 2008-2011 Dave Peticolas
%%% @doc
%%% A module for converting Erlang terms to and from Python pickles.
%%%
%%% Python pickles are binary representations of Python objects.
%%% See [http://python.org/dev/peps/pep-0307/] for details.
%%%
%%% @todo handle Python unicode strings
%%% @todo handle limited Python objects

-module(epryl_pickle).

%% API
-export([pickle_to_term/1, term_to_pickle/1]).

-include_lib("eunit/include/eunit.hrl").
-include("epryl_pickle.hrl").

-define(MAXINT, 2147483647). % threshold to switch to Python long ints

% state of stack machine which implements pickle protocol
-record(mach, {stack=[], memo, done=false}).


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
%%    unicode               binary
%%    object                #'$object'{} (record)
%%    class / function      #'$global'{} (record)
%% '''
%% Recursive and mutually recursive references are not
%% supported, i.e., lists cannot refer to themselves, etc.
%%
%% @spec pickle_to_term(Pickle::binary()) -> term()
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
%% @spec term_to_pickle(Term::term()) -> Pickle::binary()
term_to_pickle(Term) ->
    start_encode(Term).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Create a new stack machine record
%% @spec new_mach() -> mach()
new_mach() ->
    #mach{memo=dict:new()}.


%% @private
%% @doc Start running a new stack machine and return
%% the result of running it. This function requires
%% pickles to be at protocol version 2 or greater
%% @spec start_machine(Pickle::binary()) -> {term(), binary()}
start_machine(<<16#80, P, Rest/binary>> = _Pickle) when P >= 2 ->
    run_machine(new_mach(), Rest).


%% @private
%% @doc Run the given stack machine on a pickle, or
%% the remains of one. The result of the run and the
%% remains of the binary are returned.
%% @spec run_machine(Mach::mach(), PickleRest::binary()) -> {term(), binary()}
run_machine(#mach{stack=[Val], done=true} = _Mach, Rest) ->
    {Val, Rest};
run_machine(_Mach, <<>>) ->
    erlang:error("Unexpected end of pickle.");
run_machine(Mach, Rest) ->
    {NewMach, NewRest} = step_machine(Mach, Rest),
    run_machine(NewMach, NewRest).


%% @private
%% @doc Advance the stack machine one step and return the new state
%% and the remaining pickle.
%% @spec step_machine(Mach::mach(), PickleRest::binary()) ->
%%                       {NewMach::mach(), NewPickleRest::binary()}

% 32-bit signed numbers
step_machine(Mach, <<$J, Num:32/little-signed, Rest/binary>>) ->
    NewStack = [Num | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

% byte numbers
step_machine(Mach, <<$K, Num, Rest/binary>>) ->
    NewStack = [Num | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest};

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

% set to memo
step_machine(Mach, <<$q, Index, Rest/binary>>) ->
    [Val | _] = Mach#mach.stack,
    NewMemo = dict:store(Index, Val, Mach#mach.memo),
    {Mach#mach{memo=NewMemo}, Rest};

%% global
step_machine(Mach, <<$c, Rest/binary>>) ->
    [Module, Rest2] = binary:split(Rest, <<"\n">>),
    [Name, Rest3] = binary:split(Rest2, <<"\n">>),
    NewStack = [#'$global'{module=Module, name=Name} | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest3};

%% NEWOBJ
step_machine(Mach, <<16#81, Rest/binary>>) ->
    [Args, Cls | Stack1] = Mach#mach.stack,
    NewStack = [#'$object'{class=Cls, new_args=Args} | Stack1],
    {Mach#mach{stack=NewStack}, Rest};

%% BUILD
step_machine(Mach, <<$b, Rest/binary>>) ->
    [State, Object | Stack1] = Mach#mach.stack,
    NewStack = [Object#'$object'{state=finalize(State)} | Stack1],
    {Mach#mach{stack=NewStack}, Rest};

%% BINUNICODE
step_machine(Mach, <<$X, Len:32/integer-little, Rest/binary>>) ->
    <<Unicoded:Len/binary, Rest2/binary>> = Rest,
    NewStack = [Unicoded | Mach#mach.stack],
    {Mach#mach{stack=NewStack}, Rest2};

% end of pickle
step_machine(Mach, <<$., Rest/binary>>) ->
    [Val] = Mach#mach.stack,
    {Mach#mach{stack=[finalize(Val)], done=true}, Rest}.


%% @private
%% @doc Transform an intermediate value created by
%% the stack machine into its final form. Mainly used
%% to reverse lists.
%% @spec finalize(Val::term()) -> term()
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
%% @spec pop_to_mark(Stack::list()) -> {Slice::list(), NewStack::list()}
pop_to_mark(Stack) ->
    pop_to_mark([], Stack).

pop_to_mark(Slice, [mark | Stack]) ->
    {Slice, Stack};
pop_to_mark(Slice, [Val | Stack]) ->
    pop_to_mark([Val | Slice], Stack).


%% @private
%% @doc Adds the slice to the list. The new list is returned.
%% @spec add_slice(List::list(), Slice::list()) -> NewList::list()
add_slice(List, []) ->
    List;
add_slice(List, [Val | Slice]) ->
    add_slice([finalize(Val) | List], Slice).

%% @private
%% @doc Add the key-val pairs to the dict at the top of the stack.
%% @spec set_items(Stack::list(), KeyVals::list()) -> NewStack::list()
set_items([{dictionary, Dict} | Stack], KeyVals) ->
    NewDict = update_dict(Dict, KeyVals),
    [{dictionary, NewDict} | Stack].

%% @private
%% @doc Add the key-val pairs to the given dict. Return the new dict.
%% @spec update_dict(Stack::list(), KeyVals::list()) -> NewStack::list()
update_dict(Dict, []) ->
    Dict;
update_dict(Dict, [Key, Val | KeyVals]) ->
    update_dict(dict:store(Key, Val, Dict), KeyVals).


%% @private
%% @doc encode the given Term as a Python pickle.
%% @spec start_encode(Term::term()) -> Pickle::binary()
start_encode(Term) ->
    IOList = finish_encode(encode_term(Term, [<<16#80, 2>>])),
    list_to_binary(lists:reverse(IOList)).

%% @private
%% @doc encode a single Term into a Python pickle, given the
%% pickle encoded so far as a reversed iolist.
%% @spec encode_term(Term::term(), PickleSoFar::iolist()) -> Pickle::iolist()

% None
encode_term(none, Pickle) ->
    ["N" | Pickle];

% True
encode_term(true, Pickle) ->
    [16#88 | Pickle];

% False
encode_term(false, Pickle) ->
    [16#89 | Pickle];

% floats
encode_term(Number, Pickle) when is_float(Number) ->
    [<<$G, Number/big-signed-float>> | Pickle];

% tiny integers
encode_term(Number, Pickle)
  when is_number(Number), Number >= 0, Number =< 255 ->
    [<<$K, Number>> | Pickle];

% medium integers
encode_term(Number, Pickle)
  when is_number(Number), Number >= 256, Number =< 65535 ->
    [<<$M, Number:16/little-unsigned>> | Pickle];

% big integers
encode_term(Number, Pickle)
  when is_number(Number), Number =< ?MAXINT ->
    [<<$J, Number:32/little-signed>> | Pickle];

% empty tuples
encode_term({}, Pickle) ->
    [$) | Pickle];

% dictionaries
encode_term(Dict, Pickle) when is_tuple(Dict), element(1, Dict) =:= dict ->
    NewPickle = [$} | Pickle],
    case dict:size(Dict) > 0 of
        true ->
            [$u | encode_dict(Dict, [$( | NewPickle])];
        false ->
            NewPickle
    end;

% long integers
encode_term(Number, Pickle) when is_number(Number) ->
    {Encoding, NumBytes} = encode_long(Number, [], 0),
    Header = long_header(NumBytes),
    [lists:reverse(Encoding), Header | Pickle];

% binaries (Python strings)
encode_term(Binary, Pickle) when is_binary(Binary) ->
    P1 = [string_header(size(Binary)) | Pickle],
    [Binary | P1];

% 1-tuples
encode_term({Term}, Pickle) ->
    [16#85 | encode_term(Term, Pickle)];

% 2-tuples
encode_term({T1, T2}, Pickle) ->
    [16#86 | encode_term(T2, encode_term(T1, Pickle))];

% 3-tuples
encode_term({T1, T2, T3}, Pickle) ->
    [16#87 | encode_term(T3, encode_term(T2, encode_term(T1, Pickle)))];

% big tuples
encode_term(Tuple, Pickle) when is_tuple(Tuple) ->
    [$t | encode_tuple(Tuple, [$( | Pickle], 1)];

% empty lists
encode_term([], Pickle) ->
    [$] | Pickle];

% non-empty lists
encode_term(List, Pickle) when is_list(List) ->
    encode_list(List, [$(, $] | Pickle]).

%% @private
%% @doc finish the encoding
%% @spec finish_encode(Pickle::binary()) -> Pickle::binary()
finish_encode(Pickle) ->
    ["." | Pickle].

%% @private
%% @doc Return the appropriate control code for a Python string
%% of the given length.
%% @spec string_header(Length::number()) -> char()
string_header(Length) ->
    case Length < 256 of
        true -> <<$U, Length>>;
        false -> <<$T, Length:32/little-signed>>
    end.

%% @private
%% @doc Return the appropriate control code for a Python Long
%% of the given length in bytes.
%% @spec long_header(NumBytes::number()) -> char()
long_header(NumBytes) when NumBytes >= 1, NumBytes =< 255 ->
    <<16#8a, NumBytes>>;
long_header(NumBytes) ->
    <<16#8b, NumBytes:32/signed-little>>.

%% @private
%% @doc Return the encoding for a Python Long in a reversed list,
%% except for the initial control character.
%% @spec encode_long(Number::number(), Bytes::list(), NumBytes::number()) -> list()
encode_long(Number, Bytes, NumBytes) when Number >= -128, Number =< 127 ->
    {[Number | Bytes], NumBytes + 1};
encode_long(Number, Bytes, NumBytes) ->
    encode_long(Number bsr 8, [Number band 255 | Bytes], NumBytes + 1).

%% @private
%% @doc Add the encoding for a tuple to the given iolist,
%% except for the initial control character.
%% @spec encode_tuple(Tuple::tuple(), Pickle::iolist(), Index::number()) -> iolist()
encode_tuple(Tuple, Pickle, Index) when Index > size(Tuple) ->
    Pickle;
encode_tuple(Tuple, Pickle, Index) ->
    encode_tuple(Tuple, encode_term(element(Index, Tuple), Pickle), Index + 1).

%% @private
%% @doc Add the encoding for a list to the given iolist.
%% @spec encode_list(List::list(), Pickle::iolist()) -> iolist()
encode_list([], Pickle) ->
    [$e | Pickle];
encode_list([Term | List], Pickle) ->
    encode_list(List, encode_term(Term, Pickle)).

%% @private
%% @doc Add the encoding for a dictionary to the given iolist.
%% @spec encode_dict(Dict::dict(), Pickle::iolist()) -> iolist()
encode_dict(Dict, Pickle) ->
    dict:fold(fun encode_dict_kv/3, Pickle, Dict).

%% @private
%% @doc Add the encoding for a dictionary key/value pair to the given iolist.
%% @spec encode_dict_kv(Key::term(), Value::term(), Pickle::iolist()) -> iolist()
encode_dict_kv(Key, Value, Pickle) ->
    encode_term(Value, encode_term(Key, Pickle)).


% Tests

pickle_to_term_test_() ->
    [
     ?_assert(pickle_to_term(<<16#80, 2, $J, 0, 0, 0, 0, $.>>) == 0),
     ?_assert(pickle_to_term(<<16#80, 2, $J, 255, 0, 0, 0, $.>>) == 255),
     ?_assert(pickle_to_term(<<16#80, 2, $J, 0, 0, 0, 128, $.>>) == -math:pow(2, 31)),
     ?_assert(pickle_to_term(<<16#80, 2, $K, 1, $.>>) == 1),
     ?_assert(pickle_to_term(<<16#80, 2, $K, 255, $.>>) == 255),
     ?_assert(pickle_to_term(<<16#80, 2, $M, 255, 0, $.>>) == 255),
     ?_assert(pickle_to_term(<<16#80, 2, $M, 255, 255, $.>>) == 65535),
     ?_assert(pickle_to_term(<<16#80, 2, $N, $.>>) == none),
     ?_assert(pickle_to_term(<<16#80, 2, $T, 0, 0, 0, 0, $q, 0, $.>>) == <<>>),
     ?_assert(pickle_to_term(<<16#80, 2, $T, 3, 0, 0, 0,
                              "abc", $q, 0, $.>>) == <<"abc">>),
     ?_assert(pickle_to_term(<<16#80, 2, $U, 0, $q, 0, $.>>) == <<>>),
     ?_assert(pickle_to_term(<<16#80, 2, $U, 1, $a, $q, 0, $.>>) == <<"a">>),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8a, 0, $.>>) == 0),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8a, 1, 1, $.>>) == 1),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8a, 2, 255, 0, $.>>) == 255),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8a, 4,
                               16#80, 16#96, 16#98, 16#0, $.>>) == 10000000),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8a, 1, 16#f1, $.>>) == -15),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8b, 2, 0, 0, 0, 255, 0, $.>>) == 255),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8b, 2, 0, 0, 0, 255, 16#7f, $.>>) == 32767),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8b, 2, 0, 0, 0, 0, 255, $.>>) == -256),
     ?_assert(pickle_to_term(<<16#80, 2, 16#8b, 0, 0, 0, 0, $.>>) == 0),
     ?_assert(pickle_to_term(<<16#80, 2, 16#88, $.>>) == true),
     ?_assert(pickle_to_term(<<16#80, 2, 16#89, $.>>) == false),
     ?_assert(pickle_to_term(<<16#80, 2, "]", $.>>) == []),
     ?_assert(pickle_to_term(<<16#80, 2, "]", "q", 0, "K", 1, "a", $.>>) == [1]),
     ?_assert(pickle_to_term(<<16#80, 2, "]", "q", 0,
                               "(", "K", 1, "K", 2, "e", $.>>) == [1, 2]),
     ?_assert(pickle_to_term(<<16#80, 2, ")", $.>>) == {}),
     ?_assert(pickle_to_term(<<16#80, 2, "K", 1, 16#85, "q", 0, $.>>) == {1}),
     ?_assert(pickle_to_term(<<16#80, 2, "K", 1, "K", 2, 16#86, "q", 0, $.>>) == {1,2}),
     ?_assert(pickle_to_term(<<16#80, 2, "K", 1, "K", 2, "K", 3,
                               16#87, "q", 0, $.>>) == {1,2,3}),
     ?_assert(pickle_to_term(<<16#80, 2, "(", "K", 1, "K", 2, "K", 3, "K", 4,
                               "t", "q", 0, $.>>) == {1,2,3,4}),
     ?_assert(pickle_to_term(<<16#80, 2, 93, 113, 0, 40, 75, 1, 133, 113, 1, 93,
                               113, 2, 75, 2, 97, 101, 46>>) == [{1}, [2]]),
     ?_assert(pickle_to_term(<<128, 2, 75, 1, 93, 113, 0, 40, 75, 2,
                               75, 3, 101, 75, 4, 135, 113, 1, 46>>) == {1, [2, 3], 4}),
     ?_assert(pickle_to_term(<<128, 2, 75, 1, 93, 113, 0, 40, 75, 2,
                               75, 3, 101, 134, 113, 1, 46>>) == {1, [2, 3]}),
     ?_assert(pickle_to_term(<<128, 2, 93, 113, 0, 40, 75, 2,
                               75, 3, 101, 133, 113, 1, 46>>) == {[2, 3]}),
     ?_assert(pickle_to_term(<<128, 2, 40, 75, 1, 93, 113, 0, 40, 75, 2,
                               75, 3, 101, 75, 4, 75, 5, 116, 113,
                               1, 46>>) == {1, [2, 3], 4, 5}),
     ?_assert(pickle_to_term(<<128, 2, 93, 113, 0, 75, 2, 75, 3, 134,
                               113, 1, 97, 46>>) == [{2, 3}]),
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
     ?_assert(pickle_to_term(<<16#80, 2, 139, 1, 1, 0:258/unit:8, 1, $.>>) == 1 bsl 2048),
     ?_assert(pickle_to_term(<<128, 2, 71, 0, 0, 0, 0, 0, 0, 0, 0, 46>>) == 0.0),
     ?_assert(pickle_to_term(<<128, 2, 71, 63, 240, 0, 0, 0, 0, 0, 0, 46>>) == 1.0),
     ?_assert(pickle_to_term(<<128, 2, 71, 191, 240, 0, 0, 0, 0, 0, 0, 46>>) == -1.0),
     ?_assert(pickle_to_term(<<128, 2, 71, 63, 248, 0, 0, 0, 0, 0, 0, 46>>) == 1.5),
     ?_assert(pickle_to_term(<<128, 2, 71, 68, 21, 175, 29, 120, 181, 140, 64, 46>>) == 1.0e20),
     ?_assert(pickle_to_term(<<128, 2, 71, 196, 21, 175, 29, 120, 181, 140, 64, 46>>) == -1.0e20),
     %% pickle.dumps(sum, protocol=2)
     ?_assert(pickle_to_term(<<16#80, 2, $c, "__builtin__", $\n, "sum", $\n, $q, 0, $.>>)
              == #'$global'{module = <<"__builtin__">>, name = <<"sum">>}),
     %% class MyClass(object):
     %% def __init__(self, arg, kwarg=None):
     %%     self.arg = arg
     %%     self.kwarg = kwarg
     %% def __getstate__(self):
     %%     return (self.arg, self.kwarg)
     %% pickle.dumps(MyClass(1, 2), protocol=2)
     ?_assert(pickle_to_term(
                <<128,2,99,95,95,109,97,105,110,95,95,10,77,121,67,108,97,115,
                  115,10,113,0,41,129,113,1,75,1,75,2,134,113,2,98,46>>)
              == #'$object'{class = #'$global'{module = <<"__main__">>,
                                               name = <<"MyClass">>},
                            new_args = {},
                            state = {1, 2}}),
     %% class MyClass(object):
     %% def __init__(self, arg, kwarg=None):
     %%     self.arg = arg
     %%     self.kwarg = kwarg
     %% pickle.dumps(MyClass(1, 2), protocol=2)
     ?_assert(pickle_to_term(
               <<128,2,99,95,95,109,97,105,110,95,95,10,77,121,67,108,97,115,
                 115,10,113,0,41, 129,113,1,125,113,2,40,85,5,107,119,97,114,
                 103,113,3,75,2,85,3,97,114,103,113,4,75,1,117,98,46>>)
              == #'$object'{class = #'$global'{module = <<"__main__">>,
                                               name = <<"MyClass">>},
                            new_args = {},
                           state = dict:from_list(
                                     [{<<"arg">>, 1}, {<<"kwarg">>, 2}])}),
     %% pickle.dumps(
     %%     [u'\u041f\u0440\u0438\u0432\u0435\u0442, \u043c\u0438\u0440!'],
     %%     protocol=2)
     ?_assert(pickle_to_term(
                <<128,2,93,113,0,88,21,0,0,0,208,159,209,128,208,184,208,178,208,
                  181,209,130,44,32,208,188,208,184,209,128,33,113,1,97,46>>)
              == [<<208,159,209,128,208,184,208,178,208,
                    181,209,130,44,32,208,188,208,184,209,128,33>>])
     ].

term_to_pickle_test_() ->
    [
     ?_assert(term_to_pickle(0) == <<16#80, 2, "K", 0, $.>>),
     ?_assert(term_to_pickle(255) == <<16#80, 2, "K", 255, $.>>),
     ?_assert(term_to_pickle(256) == <<16#80, 2, "M", 0, 1, $.>>),
     ?_assert(term_to_pickle(65535) == <<16#80, 2, "M", 255, 255, $.>>),
     ?_assert(term_to_pickle(-1) == <<16#80, 2, "J", 255, 255, 255, 255, $.>>),
     ?_assert(term_to_pickle(65536) == <<16#80, 2, "J", 0, 0, 1, 0, $.>>),
     ?_assert(term_to_pickle(2147483648) == <<16#80, 2, 138, 5, 0, 0, 0, 128, 0, $.>>),
     ?_assert(term_to_pickle(1 bsl 2048) == <<16#80, 2, 139, 1, 1,
                                             0:258/unit:8, 1, $.>>),
     ?_assert(term_to_pickle(none) == <<16#80, 2, "N", $.>>),
     ?_assert(term_to_pickle(true) == <<16#80, 2, 16#88, $.>>),
     ?_assert(term_to_pickle(false) == <<16#80, 2, 16#89, $.>>),
     ?_assert(term_to_pickle(<<"test">>) == <<16#80, 2, "U", 4, "test", $.>>),
     ?_assert(term_to_pickle({}) == <<16#80, 2, $), $.>>),
     ?_assert(term_to_pickle({1}) == <<16#80, 2, $K, 1, 16#85, $.>>),
     ?_assert(term_to_pickle({1, 2}) == <<16#80, 2, $K, 1, $K, 2, 16#86, $.>>),
     ?_assert(term_to_pickle({1, 2, 3}) == <<16#80, 2, $K, 1, $K, 2, $K, 3, 16#87, $.>>),
     ?_assert(term_to_pickle({1, 2, 3, 4}) == <<16#80, 2, $(, $K, 1, $K, 2, $K, 3,
                                                $K, 4, $t, $.>>),
     ?_assert(term_to_pickle([]) == <<16#80, 2, $], $.>>),
     ?_assert(term_to_pickle([1, 2, 3]) == <<16#80, 2, $], $(,
                                             $K, 1, $K, 2, $K, 3, $e, $.>>),
     ?_assert(term_to_pickle(dict:new()) == <<16#80, 2, $}, $.>>),
     ?_assert(term_to_pickle(dict:from_list([{1,2}])) == <<16#80, 2, $}, $(, $K, 1,
                                                           $K, 2, $u, $.>>),
     ?_assert(term_to_pickle(0.0) == <<128, 2, 71, 0, 0, 0, 0, 0, 0, 0, 0, 46>>),
     ?_assert(term_to_pickle(1.0) == <<128, 2, 71, 63, 240, 0, 0, 0, 0, 0, 0, 46>>),
     ?_assert(term_to_pickle(-1.0) == <<128, 2, 71, 191, 240, 0, 0, 0, 0, 0, 0, 46>>),
     ?_assert(term_to_pickle(1.5) == <<128, 2, 71, 63, 248, 0, 0, 0, 0, 0, 0, 46>>),
     ?_assert(term_to_pickle(1.0e20) == <<128, 2, 71, 68, 21, 175, 29, 120, 181, 140, 64, 46>>),
     ?_assert(term_to_pickle(-1.0e20) == <<128, 2, 71, 196, 21, 175, 29, 120, 181, 140, 64, 46>>)
].


dicts_are_same(D1, D2) ->
    S1 = dict:size(D1),
    S2 = dict:size(D2),
    case S1 =:= S2 of
        false ->
            false;
        true ->
            Check = fun(Key) ->
                            {ok, V} = dict:find(Key, D1),
                            {ok, V} =:= dict:find(Key, D2)
                    end,
            lists:all(Check, dict:fetch_keys(D1))
    end.

dict_from_keys(Keys) ->
    dict_from_keys(Keys, dict:new()).

dict_from_keys([], D) ->
    D;
dict_from_keys([Key | Keys], D) ->
    dict_from_keys(Keys, dict:store(Key, none, D)).
