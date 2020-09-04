-module(amoveo_explorer_app).
-behaviour(application).
-export([start/2, stop/1]).
start(_StartType, _StartArgs) ->
    inets:start(),
    start_http(),
    scan:cron(130000),
    amoveo_explorer_sup:start_link().
stop(_State) ->
    ok.
start_http() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [
		  {"/:file", file_handler, []},
		  {"/", http_handler, []}
		 ]}]),
    Port = 8091,
    {ok, _} = cowboy:start_http(
                http, 100,
                [{ip, {0, 0, 0, 0}}, {port, Port}],
                [{env, [{dispatch, Dispatch}]}]),
    ok.
    
