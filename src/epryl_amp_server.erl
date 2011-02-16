%%% @author Dave Peticolas <dave@krondo.com>
%%% @copyright (C) 2008-2011, Dave Peticolas
%%% @doc
%%% AMP client. See:
%%%  [http://twistedmatrix.com/documents/current/api/twisted.protocols.amp.html]
%%% @end
%%% Created : 27 Apr 2008 by Dave Peticolas <dave@krondo.com>

-module(epryl_amp_server).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, connect/3,
         ask/3, call/3, call/4, respond/4]).

%% gen_socket callback
-export([set_socket/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-include("epryl_amp.hrl").

-define(SERVER, ?MODULE). 
-define(TIMEOUT, 10000).
-define(DEFAULT_MAX_PENDING, 1000).

-record(state, {socket=none,
                nextid=0,
                inbox=none, % inbox process Pid
                outbox=none, % outbox process Pid
                handlers, % dict: command name -> {Command, Handler}
                questions, % dict: Id -> Question (pending questions we asked)
                answers, % dict: ExternalId -> Answer (pending answers we
                         % have been asked),
                max_pending % maximum # of pending q's & a's
               }).

%% @doc
%% Starts the server with a list of command handlers.
%%
%% Each Handler function should be a 4-arity function that takes the
%% Server Pid, an amp_command record, a command Id of unspecified
%% type, and a KVPairs list representing the amp box.
%%
%% @spec start_link(HandlerSpecs) -> {ok, Pid} | ignore | {error, Error}
%%        HandlerSpecs = [HSpec]
%%        HSpec = {Command::amp_command(), Handler::function()}
start_link(HandlerSpecs) when is_list(HandlerSpecs) ->
    start_link(HandlerSpecs, []).

%% @doc
%% Starts the server with a list of command handlers and options.
%%
%% @spec start_link(HandlerSpecs, Options) -> {ok, Pid} | ignore | {error, Error}
%%        Options = [Options]
%%        Options = {max_pending, integer()}
start_link(HandlerSpecs, Options)
  when is_list(HandlerSpecs), is_list(Options) ->
    gen_server:start_link(?MODULE, [HandlerSpecs, Options], []).


%% @doc
%% Connect to an AMP server. The SupNamePrefix is the prefix used to
%% name the gen_socket top-level supervisor.
%%
%% @spec connect(SupNamePrefix::string(), Host::string(),
%%               Port::integer()) -> {ok, Pid}
connect(SupNamePrefix, Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port,
                                   [{packet, raw}, binary, {active, false}],
                                   ?TIMEOUT),
    gen_socket_sup:handle_socket(SupNamePrefix, epryl_amp_server, Socket).

%% @doc
%% Send a question to the peer. The return value includes a QuestionKey.
%% Once an answer, or a legal error, comes back from the peer, a message
%% will be sent to the calling process with the following form:
%%
%%          AmpAnswer | AmpError
%%          AmpAnswer = {amp_answer, QuestionKey, KVPairs}
%%          AmpError = {amp_error, QuestionKey, Error, KVPairs}
%%
%% In the case of an error, the Error term will be the atom which
%% maps to the error code given in the errors list in the Command.
%%
%% @spec ask(Pid, Proto, KVPairs::list()) -> Result
%%         Proto = amp_command() | string()
%%         Result = {ok, QuestionKey}
%%         QuestionKey = {Pid, Id}
ask(Pid, Command, KVPairs)
  when is_record(Command, amp_command), is_list(KVPairs) ->
    gen_server:call(Pid, {ask, Command, KVPairs});

ask(Pid, Name, KVPairs)
  when is_list(Name), is_list(KVPairs) ->
    {ok, Command} = gen_server:call(Pid, {lookup_command, Name}),
    ask(Pid, Command, KVPairs).

%% @doc
%% Send a question to the peer. The call blocks until the peer returns
%% an answer or the standard gen_server timeout expires.
%%
%% In the case of an error, the Error term will be the atom which
%% maps to the error code given in the errors list in the Command.
%%
%% @spec call(Pid, Proto, KVPairs::list()) -> Result
%%         Proto = amp_command() | string()
%%         Result = {ok, KVPairs} | {error, Error, KVPairs}
call(Pid, Command, KVPairs)
  when is_record(Command, amp_command), is_list(KVPairs) ->
    gen_server:call(Pid, {call, Command, KVPairs});

call(Pid, Name, KVPairs)
  when is_list(Name), is_list(KVPairs) ->
    {ok, Command} = gen_server:call(Pid, {lookup_command, Name}),
    call(Pid, Command, KVPairs).

%% @doc
%% Send a question to the peer. The call blocks until the peer returns
%% an answer or the given timeout expires.
%%
%% In the case of an error, the Error term will be the atom which
%% maps to the error code given in the errors list in the Command.
%%
%% @spec call(Pid, Proto, KVPairs::list(), Timeout) -> Result
%%         Proto = amp_command() | string()
%%         Timeout = int() | infinity
%%         Result = {ok, KVPairs} | {error, Error, KVPairs}
call(Pid, Command, KVPairs, Timeout)
  when is_record(Command, amp_command), is_list(KVPairs) ->
    gen_server:call(Pid, {call, Command, KVPairs}, Timeout);

call(Pid, Name, KVPairs, Timeout)
  when is_list(Name), is_list(KVPairs) ->
    {ok, Command} = gen_server:call(Pid, {lookup_command, Name}),
    call(Pid, Command, KVPairs, Timeout).

%% @doc
%% Respond to a question from the peer, with either an answer or an error.
%% This call must be used to respond to the peer when the Handler returns
%% noreply. The Id must be the Id supplied to the handler originally.
%%
%% @spec respond(Pid, Response::atom(), Id::term(), KVPairs::kvpairs()) -> ok
%%         Response = answer | error
respond(Pid, Response, Id, KVPairs) when is_atom(Response), is_list(KVPairs) ->
    gen_server:cast(Pid, {respond, Response, Id, KVPairs}).

%% @doc
%% The set_socket callback is used by the gen_socket supervisor.
set_socket(Pid, Socket) ->
    ok = gen_server:call(Pid, {set_socket, Socket}).


init([HandlerSpecs, Options]) ->
    case lists:keysearch(max_pending, 1, Options) of
        {value, {max_pending, MaxPending}} when is_integer(MaxPending) ->
            ok;
        false ->
            MaxPending = ?DEFAULT_MAX_PENDING
    end,
    InitState = #state{questions=dict:new(), handlers=dict:new(),
                       answers=dict:new(), max_pending=MaxPending},
    {ok, add_handlers(HandlerSpecs, InitState)}.


% Receive our socket from the socket supervisor.
handle_call({set_socket, Socket}, _From, State) ->
    Inbox = start_inbox(Socket),
    Outbox = start_outbox(Socket),
    {reply, ok, State#state{socket=Socket, inbox=Inbox, outbox=Outbox}};

% Asynchronous api for outgoing questions.
handle_call({ask, Command, KVPairs}, From, State) ->
    Question = {ask, From, Command},
    {Id, NewState} = ask_question(State, Question, KVPairs),
    {reply, {ok, {self(), Id}}, NewState};

% Synchronous api for outgoing questions.
handle_call({call, Command, KVPairs}, From, State) ->
    Question = {call, From, Command},
    {_, NewState} = ask_question(State, Question, KVPairs),
    case Command#amp_command.requires_answer of
        true ->
            {noreply, NewState};
        false ->
            {reply, ok, NewState}
    end;

% Lookup a command record by name.
handle_call({lookup_command, Name}, _From, State) ->
    case dict:find(Name, State#state.handlers) of
        {ok, {Command, _Handler}} ->
            Result = {ok, Command};
        error ->
            Result = error
    end,
    {reply, Result, State};

% Return the amp_command record for the question we asked given its Id.
% This is used by the inbox process.
handle_call({get_command_by_id, Id}, _From, State) ->
    {_, _, Command} = dict:fetch(Id, State#state.questions),
    {reply, Command, State};

% Return the amp_command record for the command we are handling given
% its name. This is used by the inbox process.
handle_call({get_command_by_name, CommandName}, _From, State) ->
    {Command, _} = dict:fetch(CommandName, State#state.handlers),
    {reply, Command, State};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.


% We got a box from the inbox process.
handle_cast({box, BoxTag, KVPairs}, State) ->
    {noreply, process_box(BoxTag, KVPairs, State)};

% A handler has a response for a question from the peer
handle_cast({respond, Response, ExternalId, KVPairs}, State) ->
    {{Command, Id}, Answers} = dict_take(ExternalId, State#state.answers),
    send_response(Response, Command, Id, KVPairs, State),
    {noreply, State#state{answers=Answers}};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(Info, State) ->
    error_logger:error_msg("Unexpected message: ~p\n", [Info]),
    {stop, bad_message, State}.


terminate(_Reason, State) ->
    stop_child(State#state.inbox),
    stop_child(State#state.outbox).

code_change(_OldVsn, State, _Extra) ->
    send_code_change(State#state.inbox),
    send_code_change(State#state.outbox),
    {ok, State}.


% @private
% @spec (state(), Question, KVPairs) -> {Id, NewState}
% @doc Send a new question to the other side. Returns
% the Id of the new question and the new state of the server.
ask_question(State, Question, KVPairs) ->
    {_, _, Command} = Question,
    {Id, NextId} = make_id(State),
    Data = epryl_amp:encode_ask(Command, Id, KVPairs),
    send(Data, State),
    Questions = dict:store(Id, Question, State#state.questions),
    check_max_pending(Questions, State),
    NewState = State#state{nextid=NextId, questions=Questions},
    {Id, NewState}.

% @private
% @spec (Socket::socket()) -> Packet::binary()
% @doc Wait for a packet from the tcp socket and return it.
% If the socket is closed, exit with socket_closed. If there is a tcp
% error, exit with tcp_error. If it receives a code_change message, it
% loops with a module-qualified call. This function is only called by
% the inbox process.
get_packet(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Packet} ->
            collect_packet(Socket, Packet);
        {tcp_closed, Socket} ->
            exit(shutdown);
        {tcp_error, Socket, Reason} ->
            exit({socket_error, Reason});
        code_change ->
            ?MODULE:get_packet(Socket);
        Msg ->
            exit({unexpected_message, Msg})
    end.

% @private
% @spec (Socket::socket(), Packet::binary()) -> binary()
% @doc Gather pending data from the socket into a packet.
collect_packet(Socket, Packet) ->
    case gen_tcp:recv(Socket, 0, 0) of
        {error, timeout} ->
            Packet;
        {ok, Packet2} ->
            collect_packet(Socket, erlang:list_to_binary([Packet, Packet2]))
    end.

% @private
% @spec (Socket::socket()) -> void()
% @doc Start the inbox process and return its Pid.
start_inbox(Socket) ->
    Server = self(),
    Inbox = spawn_link(fun () -> inbox(Socket, Server) end),
    ok = gen_tcp:controlling_process(Socket, Inbox),
    Inbox ! go,
    Inbox.

% @private
% @spec (Socket::socket(), Server::pid()) -> void()
% @doc Top-level function for the inbox handling process.
% This process decodes boxes from the socket and sends them
% to the gen_server process.
inbox(Socket, Server) ->
    % wait for server to transfer socket ownership
    receive go -> ok end,
    get_header(Socket, Server, <<>>).

% @private
% @spec (Socket::socket(), Server::pid(), Accum::binary()) -> void()
% @doc Decode a box header then start decoding the box.
get_header(Socket, Server, Accum) ->
    case epryl_amp:decode_header(Accum) of
        not_enough ->
            Packet = get_packet(Socket),
            Whole = erlang:list_to_binary([Accum, Packet]),
            get_header(Socket, Server, Whole);
        {BoxType, Id, Remaining} ->
            start_decoding(BoxType, Id, Socket, Server, Remaining)
    end.

% @private
% @spec (Response::atom(), Id::string(), Socket::socket(),
%        Server::pid(), Remaining::binary()) -> void()
% @doc Start decoding a box given its header information.
start_decoding(Response, Id, Socket, Server, Accum)
  when Response =:= answer ; Response =:= error ->
    Command = gen_server:call(Server, {get_command_by_id, Id}),
    BoxTag = {Response, Id},
    Protocol = response_protocol(Response, Command),
    Decoder = epryl_amp:new_decoder(Protocol),
    decode_box(Decoder, BoxTag, Socket, Server, Accum);

start_decoding(ask, Id, Socket, Server, Accum) ->
    get_command_header(Id, Socket, Server, Accum).

% @private
% @spec (Id::string(), Socket::socket(), Server::pid(),
%        Accum::binary()) -> void()
% @doc Decode a box command header then start decoding the box.
get_command_header(Id, Socket, Server, Accum) ->
    case epryl_amp:decode_command_header(Accum) of
        not_enough ->
            Packet = get_packet(Socket),
            Whole = erlang:list_to_binary([Accum, Packet]),
            get_command_header(Id, Socket, Server, Whole);
        {CommandName, Remaining} ->
            Command = gen_server:call(Server,
                                      {get_command_by_name, CommandName}),
            BoxTag = {ask, CommandName, Id},
            Protocol = Command#amp_command.arguments,
            Decoder = epryl_amp:new_decoder(Protocol),
            decode_box(Decoder, BoxTag, Socket, Server, Remaining)
    end.

% @private
% @spec (Decoder::decoder(), BoxTag::tuple(), Socket::socket(),
%        Server::pid(), Accum::binary()) -> void()
% @doc Use the epryl_amp decoder to decode a box off the socket
% and then send the results to the main server, then processing
% the next box header.
decode_box(Decoder, BoxTag, Socket, Server, Accum) ->
    case epryl_amp:decode_box(Decoder, Accum) of
        {not_done, NewDecoder} ->
            Packet = get_packet(Socket),
            Whole = erlang:list_to_binary([Accum, Packet]),
            decode_box(NewDecoder, BoxTag, Socket, Server, Whole);
        {done, KVPairs, Remaining} ->
            gen_server:cast(Server, {box, BoxTag, KVPairs}),
            get_header(Socket, Server, Remaining)
    end.

% @private
% @spec ({Response::atom(), Id::string()}, KVPairs::list(),
%        State::state()) -> state()
%     BoxTag = QuestionTag | AnswerTag
%     QuestionTag = {ask, CommandName::string(), Id::string()}
%     AnswerTag = {answer | error, Id::string()}
% @doc Process a box from the inbox process for the main server,
% dispatching any results to the waiting client or handlers as
% appropriate. The new server state is returned.
process_box({Response, Id}, KVPairs, #state{questions=Questions} = State)
  when Response =:= answer ; Response =:= error ->
    {Question, Questions2} = dict_take(Id, Questions),
    process_response(Response, Id, KVPairs, Question),
    State#state{questions=Questions2};

process_box({ask, CommandName, Id}, KVPairs, State) ->
    {Command, Handler} = dict:fetch(CommandName, State#state.handlers),
    ExternalId = make_ref(),
    case Handler(self(), Command, ExternalId, KVPairs) of
        {respond, Response, Result} ->
            send_response(Response, Command, Id, Result, State),
            State;
        noreply ->
            Answers = dict:store(ExternalId, {Command, Id}, State#state.answers),
            check_max_pending(Answers, State),
            State#state{answers=Answers}
    end.


% @private
% @spec (Response::atom(), Id::string(), KVPairs::list(), Question) -> ok
% @doc Process an error or answer box in the form of decoded key/value pairs,
% sending any response to the caller.
process_response(Response, Id, KVPairs, Question) ->
    {CallType, From, Command} = Question,
    case Command#amp_command.requires_answer of
        true ->
            deliver_response(Response, CallType, Id, From, KVPairs, Command);
        false ->
            ok
    end,
    case Response of
        error ->
            process_error(Id, From, KVPairs, Command);
        answer ->
            ok
    end.

% @private
% @spec (Id::string(), From, KVPairs::list(), Command::amp_command()) -> ok
% @doc Process an error from the peer. For normal protocol errors, do nothing.
% Otherwise, log the error and crash.
process_error(Id, From, KVPairs, Command) ->
    {value, {_, ErrorCode}} = lists:keysearch(?AMP_KEY_ERROR_CODE, 1, KVPairs),
    {value, {_, ErrorCode, Opts}} = lists:keysearch(ErrorCode, 2,
                                                    Command#amp_command.errors),
    case lists:member(fatal, Opts) of
        false ->
            ok;
        true ->
            Msg = "Fatal amp error with Id ~p for ~p. Response: ~p",
            error_logger:error_msg(Msg, [Id, From, KVPairs]),
            exit(fatal_amp_error)
    end.

% @private
% @spec (Response::atom(), CallType::atom(), Id::string(),
%        From, KVPairs::list(), Command::amp_command()) -> ok
% @doc Deliver a response to the caller, unless the response
% is an error that is not listed in the command record.
deliver_response(answer, CallType, Id, From, KVPairs, _Command) ->
    Response = make_local_answer(CallType, Id, KVPairs),
    deliver_response(CallType, From, Response);
deliver_response(error, CallType, Id, From, KVPairs, Command) ->
    case make_local_error(CallType, Id, KVPairs, Command) of
        none -> ok;
        Response -> deliver_response(CallType, From, Response)
    end.

% @private
% @spec (CallType::atom(), From, Response::term()) -> void()
% @doc Deliver a response to the caller appropriate to the call type.
deliver_response(call, From, Response) ->
    gen_server:reply(From, Response);
deliver_response(ask, {Pid, _Tag}, Response) ->
    Pid ! Response.

% @private
% @spec (CallType::atom(), Id::string(), KVPairs::list()) -> term()
% @doc Given a call type (call | ask), an Id, and a list of key/value
% pairs, return the term that should be delivered to the caller in the
% case of a normal answer.
make_local_answer(call, _Id, KVPairs) ->
    {ok, KVPairs};
make_local_answer(ask, Id, KVPairs) ->
    {amp_answer, {self(), Id}, KVPairs}.

% @private
% @spec (CallType::atom(), Id::string(), KVPairs::list(),
%        Command::amp_command()) -> term()
% @doc Given a call type (call | ask), an Id, a list of key/value pairs,
% and an Amp command record, return the term that should be delivered to the
% caller in the case of an error. Alternatively, if the error is not a legal
% one, as specified by the errors members of the command record, 'none' is
% returned.
make_local_error(CallType, Id, KVPairs, Command)
  when is_record(Command, amp_command) ->
    {value, {_, ErrorCode}} = lists:keysearch(?AMP_KEY_ERROR_CODE, 1, KVPairs),
    case lists:keysearch(ErrorCode, 2, Command#amp_command.errors) of
        {value, {Error, ErrorCode, _Options}} ->
            make_local_error(CallType, Id, KVPairs, Error);
        false ->
            none
    end;
make_local_error(call, _Id, KVPairs, Error) when is_atom(Error) ->
    {error, Error, KVPairs};
make_local_error(ask, Id, KVPairs, Error) when is_atom(Error) ->
    {amp_error, {self(), Id}, Error, KVPairs}.

% @private
% @spec (Response::atom(), Command::amp_command()) -> Protocol::list()
% @doc Given a response code (answer | error), and an amp command,
% return protocol suitable for decoding the rest of the box.
response_protocol(answer, Command) ->
    Command#amp_command.response;
response_protocol(error, _Command) ->
    ?AMP_ERROR_PROTOCOL.

% @private
% @spec (Response::atom(), Command::amp_command(), Id::string(),
%        KVPairs::kvpairs(), State::state()) -> void()
% @doc Send an answer or error box to the remote client.
send_response(Response, Command, Id, KVPairs, State) ->
    Box = epryl_amp:encode_response(Response, Command, Id, KVPairs),
    send(Box, State).

% @private
% @spec (state()) -> {Id::string(), NextId::integer()}
% @doc Given a state, return a new Id string and a new nextid integer.
make_id(State) ->
    Id = State#state.nextid,
    {integer_to_list(State#state.nextid), Id + 1}.

% @private
% @spec (Key::term(), Dict::dict()) -> {Value, dict()}
% @doc Given a key and a dictionary, return a tuple with the
% value of that key and a new dictionary without the key.
dict_take(Key, Dict) ->
    Value = dict:fetch(Key, Dict),
    {Value, dict:erase(Key, Dict)}.

% @private
% @spec (Data::binary(), State::state()) -> void()
% @doc Send a data packet to the outbox process.
send(Data, State) ->
    State#state.outbox ! {data, Data}.

% @private
% @spec (Socket::socket()) -> pid()
% @doc Start the outbox process and return its Pid.
start_outbox(Socket) ->
    spawn_link(fun () -> outbox(Socket) end).

% @private
% @spec (Socket::socket()) -> void()
% @doc The outbox process main loop. Data packet messages are written
% to the socket and code_change messages cause a module-qualified
% call. Any other message will crash the process.
outbox(Socket) ->
    receive
        {data, Data} ->
            gen_tcp:send(Socket, collect_data(Data)),
            outbox(Socket);
        code_change ->
            ?MODULE:outbox(Socket);
        Msg ->
            exit({unexpected_message, Msg})
    end.

% @private
% @spec (Data::binary()) -> binary()
% @doc Gather outbound packets to send together.
collect_data(Data) ->
    receive
        {data, Data2} ->
            collect_data(erlang:list_to_binary([Data, Data2]))
    after 0 ->
            Data
    end.

% @private
% @spec (NoneOrPid) -> void()
%   NoneOrPid = none | pid()
% @doc Send a code change message to the given Pid, or do nothing
% if the argument is none.
send_code_change(none) ->
    ok;
send_code_change(Pid) when is_pid(Pid) ->
    Pid ! code_change.

% @private
% @spec (NoneOrPid) -> void()
% @doc Stop a child process, or do nothing if the argument is none.
stop_child(none) ->
    ok;
stop_child(Pid) when is_pid(Pid) ->
    unlink(Pid),
    exit(Pid, parent_exit).

% @private
% @spec (HandlerSpecs, State::state()) -> state()
% @doc Return a new state with all handlers added.
add_handlers([], State) ->
    State;
add_handlers([HSpec | HandlerSpecs], State) ->
    NewState = add_handler(HSpec, State),
    add_handlers(HandlerSpecs, NewState).

% @private
% @spec (HandlerSpec, State::state()) -> state()
% @doc Return a new state with all the handler added.
add_handler({Command, Handler}, State)
  when is_record(Command, amp_command), is_function(Handler, 4) ->
    Handlers = dict:store(Command#amp_command.name, {Command, Handler},
                          State#state.handlers),
    State#state{handlers=Handlers}.

check_max_pending(Dict, #state{max_pending=Max} = _State) ->
    case dict:size(Dict) =< Max of
        true ->
            ok;
        false ->
            exit(max_pending_exceeded)
    end.


% Tests

basic_test_() ->
    {setup,
     fun setup_test_server/0,
     fun shutdown_test_server/1,
     fun basic_generator/1}.

max_pending_test_() ->
    {setup,
     fun setup_test_server/0,
     fun shutdown_test_server/1,
     fun max_pending_generator/1}.

setup_test_server() ->
    {ok, LSock} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(LSock),
    ok = gen_tcp:close(LSock),
    Divide = #amp_command{name="Divide",
                          arguments=[{"denominator", float, []},
                                     {"numerator", float, []}],
                          response=[{"result", float, []}],
                          errors=[{divide_by_zero, "ZERO_DIVISION", []}]},
    Add = #amp_command{name="Add",
                       arguments=[{"a", integer, []},
                                  {"b", integer, []}],
                       response=[{"result", integer, []}]},
    NoReply = #amp_command{name="NoReply",
                           arguments=[{"a", integer, []}],
                           response=[{"a", integer, []}]},
    HandlerSpecs = [{Divide, fun divide/4}, {Add, fun add/4},
                    {NoReply, fun noreply/4}],
    Options = [{max_pending, 10}],
    Name = "amp" ++ integer_to_list(Port),
    RName = list_to_atom(Name ++ "_test"),
    {ok, Pid} = gen_socket_sup:start_link(RName, Name, [Port],
                                          epryl_amp_server,
                                          [HandlerSpecs, Options]),
    unlink(Pid),
    {ok, ClientPid} = connect(Name, "localhost", Port),
    {Pid, ClientPid, Port}.

shutdown_test_server({Pid, _ClientPid, _Port}) ->
    exit(Pid, shutdown).

basic_generator(Data) ->
    {with, Data, [fun test_call/1, fun test_ask/1, fun test_respond/1]}.

max_pending_generator(Data) ->
    {with, Data, [fun test_max_pending/1]}.

test_call({_Pid, ClientPid, _Port}) ->
    Result1 = {ok, [{"result", 4.0}]},
    ?assertEqual(call(ClientPid, "Divide",
                      [{"numerator", 8}, {"denominator", 2}]), Result1),
    Result2 = {error, divide_by_zero,
               [{?AMP_KEY_ERROR_CODE, "ZERO_DIVISION"},
                {?AMP_KEY_ERROR_DESCRIPTION, "Division by Zero"}]},
    ?assertEqual(call(ClientPid, "Divide",
                      [{"numerator", 4.5}, {"denominator", 0.0}]), Result2).

test_ask({_Pid, ClientPid, _Port}) ->
    {ok, QKey1} = ask(ClientPid, "Divide",
                      [{"numerator", 9}, {"denominator", 3}]),
    receive
        {amp_answer, QKey1, Res1} ->
            ok
    after 5000 ->
            Res1 = error
    end,
    ?assertEqual([{"result", 3.0}], Res1),
    {ok, QKey2} = ask(ClientPid, "Divide",
                      [{"numerator", 9}, {"denominator", 0}]),
    receive
        {amp_error, QKey2, divide_by_zero, Res2} ->
            ok
    after 5000 ->
            Res2 = error
    end,
    ?assertEqual([{?AMP_KEY_ERROR_CODE, "ZERO_DIVISION"},
                  {?AMP_KEY_ERROR_DESCRIPTION, "Division by Zero"}], Res2).

test_respond({_Pid, ClientPid, _Port}) ->
    Result1 = {ok, [{"result", 14}]},
    ?assertEqual(call(ClientPid, "Add", [{"a", 8}, {"b", 6}]), Result1),
    {ok, QKey1} = ask(ClientPid, "Add", [{"a", 9}, {"b", 3}]),
    receive
        {amp_answer, QKey1, Result2} ->
            ok
    after 5000 ->
            Result2 = error
    end,
    ?assertEqual([{"result", 12}], Result2).

test_max_pending({_Pid, ClientPid, _Port}) ->
    [{ok, _} = ask(ClientPid, "NoReply", [{"a", 0}]) || _ <- lists:seq(1, 10)],
    ?assertEqual(is_process_alive(ClientPid), true),
    ?assertExit({max_pending_exceeded, _},
                ask(ClientPid, "NoReply", [{"a", 0}])),
    ?assertEqual(is_process_alive(ClientPid), false).

divide(_Server, _Command, _Id, [{"denominator", 0.0}, {"numerator", _Numer}]) ->
    {respond, error, epryl_amp:make_error(divide_by_zero, "Division by Zero")};
divide(_Server, _Command, _Id, [{"denominator", Denom}, {"numerator", Numer}]) ->
    {respond, answer, [{"result", Numer / Denom}]}.

add(Server, _Command, Id, [{"a", A}, {"b", B}]) ->
    spawn(fun () -> add(Server, Id, A + B) end),
    noreply.

add(Server, Id, Res) ->
    respond(Server, answer, Id, [{"result", Res}]).

noreply(_Server, _Command, _Id, _KVPairs) ->
    noreply.
