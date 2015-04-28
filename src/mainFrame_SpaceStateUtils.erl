-module (mainFrame_SpaceStateUtils).

-include("../include/mainframe.hrl").

-export ([convertSpaceState/1]).


convertSpaceState(SpaceStateIn) ->
    {SpaceState} = SpaceStateIn,
    iterateUntil(SpaceState, <<"name">>),
    {ok, ok}.

iterateUntil([H|T],Item) ->
    case is_list(H) of 
        true -> io:format("No list no tuple ~n");
        false -> selectFromTuple(H, Item)
    end.

selectFromTuple(Pair, Item) -> 
    case is_tuple(Pair) of
        true -> decompose(tuple_to_list(Pair), Item);
        false -> io:format("No tuple no List ~n")
    end.
decompose([H|T], Item) -> 
    case H == Item of
        true -> io:format("Bin kene Liste ~p ~n", [T]);
        false -> io:format("Nothing ~n")
    end.