-module(mainFrameBackEnd_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> 
    %% start crypto 
    application:start(crypto),
    ssl:start(),
    %% start http client
    application:start(inets),
    %% json handling
    application:start(jiffy),
    %% apple push notification service
    application:start(apns),
    %% start spacestate backend
    application:start(mainFrameBackEnd).  
start(_StartType, _StartArgs) ->
    mainFrameBackEnd_sup:start_link().

stop(_State) ->
    ok.
