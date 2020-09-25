-module(accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/1, add_tx/2, add_sub/2, add_shares/2,
         clean/0,
         add/4,
         test/0]).

-record(acc, {pub, txs = [], sub_accs = [], liquidity_shares = []}).

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Pub, Txids, Subs, Shares}, X) -> 
    Acc = case dict:find(Pub, X) of
              error -> #acc{pub = Pub};
              {ok, Y} -> Y
          end,
    #acc{txs = Txs,
         sub_accs = SAs,
         liquidity_shares = LSs} = Acc,
    Acc2 = Acc#acc{txs = merge(Txids, Txs),
                   sub_accs = merge(SAs, Subs),
                   liquidity_shares = merge(LSs, Shares)},
    X2 = dict:store(Pub, Acc2, X),
    {noreply, X2};
handle_cast({replace, X}, _) -> 
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Pub}, _From, X) -> 
    {reply, dict:find(Pub, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

clean() ->
    X = gen_server:call(?MODULE, all),
    X2 = clean_accounts(dict:fetch_keys(X), X),
    gen_server:cast(?MODULE, {replace, X2}).
clean_accounts([], X) -> X;
clean_accounts([H|T], X) -> 
    A = dict:fetch(H, X),
    V2 = clean_account(A, X),
    X2 = dict:store(H, V2, X),
    clean_accounts(T, X2).
clean_account(Acc, Dict) ->
    #acc{
          pub = Pub,
          sub_accs = SA,
          liquidity_shares = LS
        } = Acc,
    Acc#acc{
      sub_accs = clean_sub_dust(SA, Pub),
      liquidity_shares = clean_liquidity_dust(LS, Pub)
     }.
clean_sub_dust([], _) -> [];
clean_sub_dust([H|T], Pub) -> 
    {ok, Contract} = utils:talk({contract, H}),
    io:fwrite("clean sub dust\n"),
    io:fwrite(packer:pack([H, Contract])),
    io:fwrite("\n"),
    MT = element(3, Contract),
    B = csb_check(MT, H, Pub),
    X = if
            B  -> [];
            true -> [H]
        end,
    X ++ clean_sub_dust(T, Pub).
csb_check(0, _, _) -> true;
csb_check(T, CID, Pub) ->
    {ok, SA} = utils:talk({sub_account, Pub, CID, T}),
    B = element(2, SA) > 0.00000001,
    if
        B -> false;
        true -> csb_check(T-1, CID, Pub)
    end.
clean_liquidity_dust([], _) -> [];
clean_liquidity_dust([H|T], Pub) -> 
    {ok, SA} = utils:talk({sub_account, Pub, H, 0}),
    B = element(2, SA) > 0.00000001,
    if
        B -> clean_liquidity_dust(T, Pub);
        true -> [H|clean_liquidity_dust(T, Pub)]
    end.
             

    
merge([], X) -> X;
merge([H|T], L) -> 
    B = is_in(H, L),
    if
        B -> merge(T, L);
        true -> merge(T, [H|L])
    end.
is_in(H, []) -> false;
is_in(H, [H|_]) -> true;
is_in(H, [_|T]) -> 
    is_in(H, T).

read(Pub) ->
    gen_server:call(?MODULE, {read, Pub}).
add_tx(0, _) -> ok;
add_tx(Pub, TxID) ->
    %ok.
    gen_server:cast(?MODULE, {add, Pub, [TxID], [], []}).
add_sub(0, _) -> ok;
add_sub(Pub, <<0:256>>) -> ok;
add_sub(Pub, ID) ->
    gen_server:cast(?MODULE, {add, Pub, [], [ID], []}).
add_shares(0, _) -> ok;
add_shares(Pub, ID) ->
    gen_server:cast(?MODULE, {add, Pub, [], [], [ID]}).

add(0, _, _, _) -> ok;
add(_, [], [], []) -> ok;
add(Pub, Txs, Subs, Shares) ->
    gen_server:cast(?MODULE, {add, Pub, Txs, Subs, Shares}).
    
   

test() ->
    Pub = <<1:520>>,
    add_tx(Pub, hash:doit(1)),
    add_sub(Pub, hash:doit(2)),
    add_shares(Pub, hash:doit(3)),
    read(Pub).
