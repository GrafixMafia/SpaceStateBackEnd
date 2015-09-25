-module(mainFrameBackEndApi_serv).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
    {ok, [], <<"Hello World!">>};
%% Route METHOD & PATH to the appropriate clause
handle('GET',[<<"spacelist">>], _Req) ->
    SpaceList = gen_server:call(mainFrameBackEnd_poll, getSpaces),
    {ok, [], <<"TEST">>};
handle('GET', [<<"hello">>], Req) ->
    %% Fetch a GET argument from the URL.
    Name = elli_request:get_arg(<<"name">>, Req, <<"undefined">>),
    Status = gen_server:call(binary_to_atom(Name, utf8), getStates),
    SpaceS = atom_to_binary(Status,utf8),
    {ok, [], <<"Hello ", SpaceS/binary>>};

handle('GET',[<<"spacelist">>, <<"mainframe">>], _Req) ->
    Status = gen_server:call(list_to_atom("Mainframe"), getStates),
    {ok, [], atom_to_binary(Status,utf8)};
    
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.