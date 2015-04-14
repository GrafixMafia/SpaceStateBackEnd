-module (mainFrame_SpaceStateUtils).

-include("../include/mainframe.hrl").

-export ([convertSpaceState/1]).


convertSpaceState(SpaceStateIn) ->
    StateOut = #spacestate{api = "1.3"},
    io:format(StateOut#spacestate.api),
    {ok, test}.