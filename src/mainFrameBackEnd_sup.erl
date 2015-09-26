-module(mainFrameBackEnd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    mainFrameBackEnd_space_sup:start_link(),
    mainFrameBackEndApi_sup:start_link(),
    SpaceStateServ = ?CHILD(mainFrameBackEnd_serv, worker),
    SpaceStatePoll = ?CHILD(mainFrameBackEnd_poll, worker),
    {ok, { {one_for_one, 10, 60}, [SpaceStateServ, SpaceStatePoll]} }.

