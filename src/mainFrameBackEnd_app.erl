-module(mainFrameBackEnd_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> 
    %% crypto 
    application:start(crypto),
    ssl:start(),
    application:start(inets),
    application:start(jiffy),
    application:start(apns),
    application:start(mainFrameBackEnd),
    io:format("mainFrameBackEnd Started").

start(_StartType, _StartArgs) ->
    mainFrameBackEnd_sup:start_link().

stop(_State) ->
    ok.
