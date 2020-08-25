-module(txs).
-behaviour(gen_server).

%this module stores any tx we have seen, as long as it was included in a block with enough work done.

-record(tx, {txid, raw, block}).

-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        add/3]).
init(ok) -> {ok, []}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Tx, ID, Block}, X) -> 
    T = #tx{txid = ID, raw = Tx, block = Block},
    X2 = dict:store(ID, T, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, X) -> 
    Z = dict:find(ID, X),
    {reply, Z, X};
handle_call(_, _From, X) -> {reply, X, X}.


add(Tx, ID, BlockHash) ->
    <<_:256>> = ID,
    <<_:256>> = BlockHash,
    gen_server:cast(?MODULE, {add, Tx, ID, BlockHash}).

read(ID) ->
    gen_server:call(?MODULE, {read, ID}).
    
