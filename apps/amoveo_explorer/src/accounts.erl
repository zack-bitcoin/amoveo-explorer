-module(accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/1, add_tx/2, add_sub/2, add_shares/2,
         clean/0,
         add/4,
         clean_cron/0,

         is_worker/2, is_boss/2, not_boss/2,

         test/0]).
-define(LOC, "accounts.db").

-record(acc, {pub, txs = [], sub_accs = [], liquidity_shares = [], worker_for = [], boss_of = []}).

init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> dict:new();
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("accounts died!"), ok.
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
handle_cast({worker, Pub, ID}, X) -> 
    Acc = case dict:find(Pub, X) of
              error -> #acc{pub = Pub};
              {ok, Y} -> Y
          end,
    WF = Acc#acc.worker_for,
    WF2 = merge([ID], WF),
    Acc2 = Acc#acc{worker_for = WF2},
    X2 = dict:store(Pub, Acc2, X),
    {noreply, X2};
handle_cast({boss, Pub, ID}, X) -> 
    Acc = case dict:find(Pub, X) of
              error -> #acc{pub = Pub};
              {ok, Y} -> Y
          end,
    BO = Acc#acc.boss_of,
    BO2 = merge([ID], BO),
    Acc2 = Acc#acc{boss_of = BO2},
    X2 = dict:store(Pub, Acc2, X),
    {noreply, X2};
handle_cast({not_boss, Pub, ID}, X) -> 
    Acc = case dict:find(Pub, X) of
              error -> #acc{pub = Pub};
              {ok, Y} -> Y
          end,
    BO = Acc#acc.boss_of,
    BO2 = remove_from_list(ID, BO),
    Acc2 = Acc#acc{boss_of = BO2},
    X2 = dict:store(Pub, Acc2, X),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Pub}, _From, X) -> 
    {reply, dict:find(Pub, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

clean() ->
    X = gen_server:call(?MODULE, all),
    X2 = clean_accounts(dict:fetch_keys(X), X),
    gen_server:cast(?MODULE, {replace, X2}).

clean_cron() ->
    timer:sleep(120000),
    spawn(clean),
    clean_cron().



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
    X = case Contract of
        0 -> [];
        _ ->
            %io:fwrite("clean sub dust\n"),
            %io:fwrite(packer:pack([H, Contract])),
            %io:fwrite("\n"),
            MT = element(3, Contract),
            B = csb_check(MT, H, Pub),
            if
                B  -> [];
                true -> [H]
            end
        end,
    X ++ clean_sub_dust(T, Pub).
csb_check(0, _, _) -> true;
csb_check(T, CID, Pub) ->
    {ok, SA} = utils:talk({sub_account, Pub, CID, T}),
    B = element(2, SA) > 0.0001,
    if
        B -> false;
        true -> csb_check(T-1, CID, Pub)
    end.
clean_liquidity_dust([], _) -> [];
clean_liquidity_dust([H|T], Pub) -> 
    {ok, SA} = utils:talk({sub_account, Pub, H, 0}),
    B = element(2, SA) > 0.0001,
    if
        B -> [H|clean_liquidity_dust(T, Pub)];
        true -> clean_liquidity_dust(T, Pub)
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

remove_from_list(ID, []) -> [];
remove_from_list(ID, [ID|R]) -> R;
remove_from_list(ID, [H|T]) -> 
    [H|remove_from_list(ID, T)].

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
   
is_worker(Pub, ID) -> 
    io:fwrite("account is_worker\n"),
    gen_server:cast(?MODULE, {worker, Pub, ID}).
is_boss(Pub, ID) ->
    io:fwrite("account is_boss\n"),
    gen_server:cast(?MODULE, {boss, Pub, ID}).
not_boss(Pub, ID) ->
    gen_server:cast(?MODULE, {not_boss, Pub, ID}).
    
   

test() ->
    Pub = <<1:520>>,
    add_tx(Pub, hash:doit(1)),
    add_sub(Pub, hash:doit(2)),
    add_shares(Pub, hash:doit(3)),
    read(Pub).
