#Commands

erl -pa deps/*/ebin ebin -s mainFrameBackEnd_app start


http client bauen und json abfragen.
documentieren.

Making HTTP Request with Erlang
http://no-fucking-idea.com/blog/2013/01/22/making-request-to-rest-resources-in-erlang/
    
    inets:start().
    {ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = httpc:request(get, {"http://spaceapi.net/directory.json?api=0.13", []}, [], []).
    SpaceListList = jiffy:decode(Body).

Nun haben wir einen Tuble der eine Liste mit Tublen enthält.

    -> SpaceListList -> 
    {[{space name, url}, {space name, url}, {space name, url} ..... ]}


    SpaceList element(1,SpaceListList).
    

Liste mit Tupeln 
    [{space name, url}, {space name, url}, {space name, url} ..... ]
    


Parsing JSON


spawn multiple worker 
http://www.erlang.org/documentation/doc-4.9.1/doc/design_principles/sup_princ.html

http://www.erlang.org/doc/man/supervisor.html#start_child-2

spawn new supervised prozesses 
http://stackoverflow.com/questions/4837196/erlang-supervisor3-adding-a-child-process

