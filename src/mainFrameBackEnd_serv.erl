-module(mainFrameBackEnd_serv).
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

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, URL,I , Type), {Name, {I, start_link, [Name, URL]}, permanent, 5000, Type, [I]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(startSpacePoll, State) -> 
    % create new processes for each space
    mainFrameBackEnd_space_sup:start_link(),
    startSpacePoll(),
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
startSpacePoll() -> 
    % get the space list fetched by mainFrameBackEnd_poll
    Name = ets:first(spacelist),
    % iterate through the space list and start a space process
    iterate(Name).

iterate('$end_of_table') -> 
    ok;
iterate(Name) ->
    io:format("Name: ~s", [Name]),
    % get space information
    [{Name,URL}] = ets:lookup(spacelist, Name),
    % start space poller
    {ok, _ } = startSpacePoller(Name, URL),
    % get next space information
    iterate(ets:next(spacelist, Name)).

startSpacePoller(Name, URL) -> 
    % create new child (space poller)
    SpacePoller = ?CHILD(Name, URL, mainFrameBackEnd_space_poll, worker),
    % add new space poller to supervisor
    {ok, _ } = supervisor:start_child(mainFrameBackEnd_space_sup, SpacePoller).
