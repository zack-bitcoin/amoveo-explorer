-module(amoveo_explorer_app).
-behaviour(application).
-export([start/2, stop/1]).
start(_StartType, _StartArgs) ->
    inets:start(),
    start_http(),
    TM = utils:test_mode(),
    N = if
            TM -> 0;
            true -> 130000
        end,
    spawn(fun() ->
                  scan:doit(),
                  scan:cron(N)
          end),
    spawn(fun() ->
                  accounts:clean_cron()
          end),
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
    
