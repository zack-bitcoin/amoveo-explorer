-module(blocks).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        add/1, read/1]).
-define(LOC, "blocks.db").
-record(block, {height, hash, txs = []}).

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
    io:fwrite("blocks died!"),
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Block}, X) -> 
    Hash = element(3, Block),
    Height = element(2, Block),
    Txs = element(11, Block),
    B2 = #block{height = Height, hash=Hash,
                txs = txid_maker(Txs)},
    X2 = dict:store(Hash, B2, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Hash}, _From, X) -> 
    {reply, dict:find(Hash, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

txid_maker([]) ->
    [];
txid_maker([H|T]) ->
    TXID = crypto:hash(sha256, sign:serialize(H)),
    [TXID|txid_maker(T)].
    
read(Hash) ->
    gen_server:call(?MODULE, {read, Hash}).
add(Block) ->
    gen_server:cast(?MODULE, {add, Block}).
