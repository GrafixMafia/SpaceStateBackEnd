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

interval_milliseconds()-> 60000.

init([Name, URL, State]) ->
    % create new process storage that will hold details of the space
    ets:new(Name, [set, named_table]),
    % send request intervall periodicly
    timer:send_interval(interval_milliseconds(), interval),
    % pass state of process
    State2 = #state{open=false},
    {ok, #space{name=Name,url=URL, state=State2}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(atom, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(interval, StateData)->
    URL = StateData#space.url,
    % call space api and recieve status
    {ok, [Part2]}  = get_status(URL),
    %compareState(Part2,StateData#space.state#state),
    
    io:format("OPEN? ~p ~n", [Part2]),
    
    NewState = #state{open=Part2},
    NewStateData = StateData#space{state=NewState},
    {noreply, NewStateData};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_status(URL) -> 

    try httpc:request(get, {atom_to_list(URL), []}, [], []) of
        {ok, {{_, HTTPFeedBackCode, _}, _, Body}} -> {ok, Response} = create_response(Body);
        _ -> {ok, Response} = create_defaultresponse()
    catch
        _:_ -> {ok, Response} = create_defaultresponse()
    end.

create_response(Body) -> 
    try jiffy:decode(Body) of
        StateOfSpace -> parse_response(StateOfSpace)
    catch
        _:_ ->  create_defaultresponse()
    end.

parse_response(StateOfSpace) -> 
    {ok, [Response]}  = mainFrame_SpaceStateUtils:convertSpaceState(StateOfSpace, <<"state">>),
    mainFrame_SpaceStateUtils:convertSpaceState(Response, <<"open">>).

create_defaultresponse() -> 
    {ok, [unknown]}.
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------