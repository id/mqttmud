%%%-------------------------------------------------------------------
%% @doc mqttmud public API
%% @end
%%%-------------------------------------------------------------------

-module(mqttmud_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mqttmud_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
