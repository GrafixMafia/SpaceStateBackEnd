-module(mainFrameBackEnd_poll).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SPACEAPI, "http://spaceapi.net/directory.json?api=0.13").

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

interval_milliseconds()-> 20000.

init(Args) ->
    % create new process storage
    ets:new(spacelist, [set, named_table]),
    % recieve list of spaces with meta information
    {ok, SpaceList} = recieveSpaceList(?SPACEAPI),
    % store meta information in process storage
    {ok, done} = storeSpaceList(SpaceList),
    % set interval for checking the space api for changes and trigger 
    timer:send_interval(interval_milliseconds(), interval),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(start_space_processes, State) ->
    start_space_processes(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
%handle the interval with spaceapi check    
handle_info(interval, StateData)->
    % recieve list of spaces with meta information
    {ok, SpaceList} = recieveSpaceList(?SPACEAPI),
    % store meta information in process storage
    {ok, done} = storeSpaceList(SpaceList),
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

storeSpace(SpaceName, SpaceURL) ->
    true = ets:insert(spacelist,{list_to_atom(SpaceName), list_to_atom(SpaceURL)}),
    {ok, true}.

recieveSpaceList(APIURL) -> 
    % call space api and recieve list 
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {APIURL, []}, [{ssl,[{verify,0}]}], []),
    % get list as erlang data structure
    Spaces = jiffy:decode(Body),
    {ListOfSpaces} = Spaces,
    {ok, ListOfSpaces}.

storeSpaceList(SpaceList) -> 
    %get timestemp from system
    TS = {_,_,Micro} = os:timestamp(),
    % store time stamp of request
    true = ets:insert(spacelist,{timestamp, TS}),
    % convert list from binary to estrings
    StringListOfSpaces = [{binary_to_list(Name), binary_to_list(URL)} || {Name,URL} <- SpaceList],
    % store space names and urls 
    [ storeSpace(Name, URL) || {Name,URL} <- StringListOfSpaces],
    % return
    {ok, done}.

start_space_processes() -> 
    % start another supervisor
    mainFrameBackEnd_space_sup:start_link(),
    
    Name = ets:first(spacelist),
    io:format("Name : ~s ~n", [Name]),
    [{Name,URL}] = ets:lookup(spacelist, Name),
    case is_atom(Name) of
        true  -> io:format("~s IS ATOM~n", [Name]);
        false -> io:format("~s IS NOT AN ATOM~n", [Name])
    end,
    io:format("URL : ~s ~n", [URL]),
    {ok, done}.

