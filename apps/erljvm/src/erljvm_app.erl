%%%-------------------------------------------------------------------
%% @doc erljvm public API
%% @end
%%%-------------------------------------------------------------------

-module(erljvm_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erljvm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
