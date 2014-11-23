-module(mainFrameBackEnd_space_sup).

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
    SpaceStateServ = ?CHILD(mainFrameBackEnd_space_serv, worker),
    % SpacePoller1 = {mainFrameBackEnd_space_poll1, {mainFrameBackEnd_space_poll, start_link, [mainFrameBackEnd_space_poll1]}, permanent, 5000, worker, [mainFrameBackEnd_space_poll]},
    % SpacePoller2 = {mainFrameBackEnd_space_poll2, {mainFrameBackEnd_space_poll, start_link, [mainFrameBackEnd_space_poll2]}, permanent, 5000, worker, [mainFrameBackEnd_space_poll]},
    % SpaceStatePoll = ?CHILD(mainFrameBackEnd_space_poll, worker),
    {ok, { {one_for_one, 5, 10}, [SpaceStateServ]}}.

