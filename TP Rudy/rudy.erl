-module(rudy).
-import(http, [parse_request/1]).
-export([init/1]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} ->
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->
            spawn(fun() -> request(Client) end),
            handler(Listen);
        {error, Error} ->
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Response = reply(http:parse_request(Str)),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    serve_file(URI).

serve_file(URI) ->
    FilePath = "public" ++ URI,  % Busca en la carpeta "public"
    case file:read_file(FilePath) of
        {ok, Content} ->
            Response = "HTTP/1.1 200 OK\r\n" ++
                       "Content-Type: text/html\r\n\r\n" ++
                       binary_to_list(Content),
            Response;
        {error, _} ->
            "HTTP/1.1 404 Not Found\r\n\r\n404 Not Found"
    end.