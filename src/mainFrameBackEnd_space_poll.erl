-module(mainFrameBackEnd_space_poll).
-record(space, {name, url, state}).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("../include/mainframe.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, URL, State) ->  
    gen_server:start_link({local, Name}, ?MODULE, [Name, URL, State], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

interval_milliseconds()-> 100000.

init([Name, URL, State]) ->
    % create new process storage that will hold details of the space
    ets:new(Name, [set, named_table]),
    % send request intervall periodicly
    timer:send_interval(interval_milliseconds(), interval),
    % pass state of process
    {ok, #space{name=Name,url=URL, state=State}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(atom, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(interval, StateData)->
    URL = StateData#space.url,
    io:format("Name: ~p   URL:  ~p ~n", [StateData#space.name,URL]),
    % call space api and recieve status
    {ok, {{_, HTTPFeedBackCode, _}, _, Body}} = httpc:request(get, {atom_to_list(URL), []}, [], []),
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