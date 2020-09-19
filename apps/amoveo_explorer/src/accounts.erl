-module(accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read/1, add_tx/2, add_sub/2, add_shares/2,
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
    Acc2 = Acc#acc{txs = [],%merge(Txids, Txs),
                   sub_accs = merge(SAs, Subs),
                   liquidity_shares = merge(LSs, Shares)},
    X2 = dict:store(Pub, Acc2, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Pub}, _From, X) -> 
    {reply, dict:find(Pub, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

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
