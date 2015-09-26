-module(mainFrameBackEnd_poll).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SPACEAPI, "http://spaceapi.net/directory.json?api=0.13").
-define(SPACENAMECHARS, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

% interval_milliseconds()-> 1200000.
interval_milliseconds()-> 60000.

init(Args) ->
    % create new process storage
    ets:new(spacelist, [set, named_table]),
    ets:new(timestamp, [set, named_table]),
    % set interval for checking the space api for changes and trigger 
    timer:send_interval(interval_milliseconds(), interval),
    % recieve list of spaces with meta information
    handle_spaces(),
    {ok, Args}.

handle_call(getSpaces, _From, State) ->
    {ok, SpaceListJSON} = space_listing(),
    {reply, SpaceListJSON, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(start_space_processes, State) ->
    start_space_processes(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
%handle the interval with spaceapi check    
handle_info(interval, StateData)->
    handle_spaces(),
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

handle_spaces() ->
    % recieve list of spaces with meta information
    {ok, SpaceList} = recieve_space_list(?SPACEAPI),
    % store meta information in process storage
    {ok, done} = store_space_list(SpaceList),
    %% trigger polling
    mainFrameBackEnd_serv ! startSpacePoll.

recieve_space_list(APIURL) -> 
    % call space api and recieve list 
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {APIURL, []}, [{ssl,[{verify,0}]}], []),
    % get list as erlang data structure
    Spaces = jiffy:decode(Body),
    {ListOfSpaces} = Spaces,
    {ok, ListOfSpaces}.

store_space_list(SpaceList) -> 
    %get timestemp from system
    TS = {_,_,_} = os:timestamp(),
    % store time stamp of request
    true = ets:insert(timestamp,{timestamp, TS}),
    % store space names and urls 
    [store_space(Name, URL) || {Name,URL} <- SpaceList],
    % return
    {ok, done}.

store_space(SpaceName, SpaceURL) ->
    {ok, ConvName} = space_name_convert(SpaceName),
    true = ets:insert(spacelist,{list_to_atom(ConvName), {[
            {name,  SpaceName},
            {url, SpaceURL}
        ]}}),
    {ok, true}.

start_space_processes() -> 
    % start another supervisor
    mainFrameBackEnd_space_sup:start_link(),
    {ok, done}.

space_name_convert(Name) ->
    ConvName = lists:filter(fun(X) -> lists:member(X,?SPACENAMECHARS) end,binary_to_list(Name)),
    {ok, ConvName}.

space_listing() -> 
    SpaceList = ets:tab2list(spacelist),
    SpaceListEJSON = {SpaceList},
    SpaceListJSON = jiffy:encode(SpaceListEJSON),
    {ok, SpaceListJSON}.



































