-module(http_handler).
-export([init/3, handle/2, terminate/3, doit/1]).
-include("records.hrl").
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
handle(Req, State) ->
    {ok, Data0, Req2} = cowboy_req:body(Req),
    {{IP, _}, Req3} = cowboy_req:peer(Req2),
    %io:fwrite("http handler got message: "),
    %io:fwrite(Data0),
    %io:fwrite("\n"),
    Data1 = jiffy:decode(Data0),
    Data = packer:unpack_helper(Data1),
    Result = doit(Data),
    D = packer:pack(Result),
    Headers=[{<<"content-type">>,<<"application/octet-stream">>},
	     {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req4} = cowboy_req:reply(200, Headers, D, Req3),
    {ok, Req4, State}.
doit({test}) -> {ok, "success"};
doit({account, Pub}) ->
    {ok, accounts:read(Pub)};
doit({tx, ID}) ->
    {ok, txs:read(ID)};
doit({market, MID}) ->
    {ok, markets:read(MID)};
doit({r, CID1, CID2}) ->
    {ok, paths:doit(CID1, CID2)};
doit({markets}) ->
    {ok, markets:large_ones()};
doit({contract, CID}) ->
    %io:fwrite("received api contract request"),
    %io:fwrite("\n"),
    X = contracts:read(CID),
    %io:fwrite(packer:pack(X)),
    %io:fwrite("\nsending response from gen server\n"),
    {ok, X};
doit({contracts}) ->
    {ok, contracts:large_ones()};

doit(X) ->
    io:fwrite("http handler doit fail"),
    io:fwrite(X).
   
is_in(X, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) ->
    is_in(X, T).
