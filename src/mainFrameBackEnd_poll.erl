-module(mainFrameBackEnd_poll).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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
    ets:new(spacelist, [set, named_table]),
    % call space api and recieve list 
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {"http://spaceapi.net/directory.json?api=0.13", []}, [{ssl,[{verify,0}]}], []),
    ListOfSpaces = jiffy:decode(Body),
    {TestList} = ListOfSpaces,
    % convert list
    StringListOfSpaces = [{binary_to_list(Name), binary_to_list(URL)} || {Name,URL} <- TestList],
    % store space names and urls 
    [{storeSpaceList(Name, URL)} || {Name,URL} <- StringListOfSpaces],
    %% 
    timer:send_interval(interval_milliseconds(), interval),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(start_space_processes, State) ->
    {noreply, State}.
    
handle_info(interval, StateData)->
    % call space api and recieve list 
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {"http://spaceapi.net/directory.json?api=0.13", []}, [{ssl,[{verify,0}]}], []),
    ListOfSpaces = jiffy:decode(Body),
    {TestList} = ListOfSpaces,
    % convert list
    StringListOfSpaces = [{binary_to_list(Name), binary_to_list(URL)} || {Name,URL} <- TestList],
    % store space names and urls 
    [{storeSpaceList(Name, URL)} || {Name,URL} <- StringListOfSpaces],
    {noreply, StateData};
handle_info(hallo, State) -> 
    % start another supervisor
    mainFrameBackEnd_space_sup:start_link(),
    % get spacelist
    Name = ets:first(spacelist),
    io:format("Name : ~s ~n", [Name]),
    [{Name,URL}] = ets:lookup(spacelist, Name),
    case is_atom(Name) of
        true  -> io:format("~s IS ATOM~n", [Name]);
        false -> io:format("~s IS NOT AN ATOM~n", [Name])
    end,
    io:format("URL : ~s ~n", [URL]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


storeSpaceList(SpaceName, SpaceURL) ->
    true = ets:insert(spacelist,{list_to_atom(SpaceName), list_to_atom(SpaceURL)}),
    {ok, true}.

% ToDo
% - check if changes happend
%    new Space ? member(Tab, Key) -> boolean()
%       add to list
%    Space lost
%       remove from list
% - send message to serv 


