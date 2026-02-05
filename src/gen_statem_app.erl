%%%-------------------------------------------------------------------
%% @doc gen_statem public API
%% @end
%%%-------------------------------------------------------------------

-module(gen_statem_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gen_statem_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
