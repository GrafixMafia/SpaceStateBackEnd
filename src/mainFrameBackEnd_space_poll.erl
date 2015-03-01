-module(mainFrameBackEnd_space_poll).
-record(state, {name, url}).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, URL) ->  
    gen_server:start_link({local, Name}, ?MODULE, [Name, URL], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

interval_milliseconds()-> 40000.

init([Name, URL]) ->
    timer:send_interval(interval_milliseconds(), interval),
    {ok, #state{name=Name,url=URL}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(atom, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(interval, StateData)->
    URL = StateData#state.url,
    io:format("Name: ~p ~n", [StateData#state.name]),
    % call space api and recieve list
    {ok, {{_, HTTPFeedBackCode, _}, _, Body}} = httpc:request(get, {URL, []}, [], []),
    StateOfSpace = jiffy:decode(Body),
    {StateOf} = StateOfSpace,
    {noreply, StateData};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------