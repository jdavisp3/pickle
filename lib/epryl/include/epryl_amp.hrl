%%%-------------------------------------------------------------------
%%% @author Dave Peticolas <dave@krondo.com>
%%% @copyright (C) 2008, Dave Peticolas
%%% @doc
%%% Definitions for the AMP protocol implementation. See:
%%%   [http://twistedmatrix.com/documents/current/api/twisted.protocols.amp.html]
%%% @end
%%% Created :  27 Apr 2008 by Dave Peticolas <dave@krondo.com>
%%%-------------------------------------------------------------------


%% This record represents an AMP call/response protocol. The fields
%% are as follows:
%%
%%   name = string()
%%   arguments = AmpList
%%   response = AmpList
%%   errors = AmpErrorList
%%   fatal_errors = AmpErrorList
%%   requires_answer = true | false
%%
%%   AmpList = [{AmpKey, AmpType, Options}, ...]
%%   AmpKey = string() (max 255 chars)
%%   AmpType = string | binary | integer | float | boolean | {amplist, AmpList}
%%   Options = [Option]
%%   Option = optional
%%
%%   AmpErrorList = [{atom(), string(), ErrorOptions}, ...]
%%   ErrorOptions = [ErrorOption]
%%   ErrorOption = fatal
%%
%% The atom names of the AmpType enumeration have been chosen
%% to match the names used in the Twisted Python implementation
%% of Amp, with one addition (see below). This means that the
%% 'string' AmpType does not correspond to an Erlang string, but
%% a Python string.
%%
%% Amp strings are equivalent to Python strings in the Twisted
%% implementation of Amp and Python strings are really just byte
%% arrays. Erlang strings are typically implemented as lists, while
%% binaries are used for arbitrary binary data. To deal with this
%% mismatch, this library provides two options.
%%
%% The 'string' AmpType will decode to an Erlang string, i.e., a list
%% of ascii character codes. The 'binary' AmpType will decode to an Erlang
%% binary. Both types accept Erlang strings and binaries for encoding.
%%
%% The 'binary' type is also suitable for accepting other Amp types
%% not currently supported by the library. For example, by using the
%% binary type, Amp unicode values will be received as utf-8 encoded
%% Erlang binaries.
%%
%% Each tuple in an error list specifies a mapping between an atom and
%% a string error code.
-record(amp_command, {name,
                      arguments,
                      response,
                      errors=[],
                      requires_answer=true}).


%% Pre-defined key names
-define(AMP_KEY_ASK, "_ask").
-define(AMP_KEY_COMMAND, "_command").
-define(AMP_KEY_ANSWER, "_answer").
-define(AMP_KEY_ERROR, "_error").
-define(AMP_KEY_ERROR_CODE, "_error_code").
-define(AMP_KEY_ERROR_DESCRIPTION, "_error_description").

%% Limits
-define(AMP_MAX_KEY_LEN, 255).
-define(AMP_MAX_VAL_LEN, 65535).

%% The protocol for error responses.
-define(AMP_ERROR_PROTOCOL, [{?AMP_KEY_ERROR_CODE, string, []},
                             {?AMP_KEY_ERROR_DESCRIPTION, string, []}]).
