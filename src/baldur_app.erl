%%%-------------------------------------------------------------------
%% @doc baldur public API
%% @end
%%%-------------------------------------------------------------------

-module(baldur_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    baldur_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
