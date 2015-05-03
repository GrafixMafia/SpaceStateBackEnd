-module (mainFrame_SpaceStateUtils).

-include("../include/mainframe.hrl").

-export ([convertSpaceState/1]).

convertSpaceState(SpaceStateIn) ->
    {SpaceState} = SpaceStateIn,
    iterateUntil(SpaceState, <<"space">>),
    {ok, ok}.

iterateUntil([], Item) ->
    {ok, nothing};
iterateUntil([H|T],Item) ->
    case is_list(H) of 
        true -> io:format("No list no tuple ~n");
        false -> selectFromTuple(H,T , Item)
    end.

selectFromTuple(Pair, T , Item) -> 
    case is_tuple(Pair) of
        true -> decompose(tuple_to_list(Pair), T ,Item);
        false -> iterateUntil(T, Item)
    end.
decompose([H|T], L, Item) -> 
    case H == Item of
        true -> io:format("Bin kene Liste ~p ~n", [T]);
        false -> iterateUntil(L, Item)
    end.