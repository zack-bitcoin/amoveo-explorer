-module(txs).
-behaviour(gen_server).

%this module stores any tx we have seen, as long as it was included in a block with enough work done.

-record(tx, {txid, block, raw, height}).

-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         scan_history/0, add/3, read/1, test/0]).
-define(LOC, "txs.db").
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
    io:format("txs died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Tx, ID, Block, Height}, X) -> 
    T = #tx{txid = ID, raw = Tx, block = Block, height = Height},
    X2 = dict:store(ID, T, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID}, _From, X) -> 
    Z = dict:find(ID, X),
    {reply, Z, X};
handle_call(_, _From, X) -> {reply, X, X}.


add(Tx, BlockHash, Height) when element(1, Tx) == coinbase ->
    ID = crypto:hash(sha256, sign:serialize(Tx)),
    gen_server:cast(?MODULE, {add, Tx, ID, BlockHash, Height}),
    ID;
add(Stx, BlockHash, Height) ->
    Tx = element(2, Stx),
    ID = crypto:hash(sha256, sign:serialize(Tx)),
    <<_:256>> = BlockHash,
    gen_server:cast(?MODULE, {add, Stx, ID, BlockHash, Height}),
    ID.

read(ID) ->
    gen_server:call(?MODULE, {read, ID}).


scan_history() ->
    {ok, Height} = utils:talk({height}),
    scan_history(0, Height).
scan_history(N, M) when N >= M -> ok;
scan_history(Start, End) -> 
    io:fwrite("scanning at \n"),
    io:fwrite(integer_to_list(Start)),
    io:fwrite("\n"),
    {ok, Blocks} = utils:talk({blocks, Start, End}),
    if
        Start > 11000 ->
            io:fwrite(packer:pack(length(Blocks))),
            io:fwrite("\n");
        true -> ok
    end,
    case length(Blocks) of
        1 -> ok;
        _ ->
            load_txs(Blocks),
            LastBlock = lists:nth(length(Blocks), Blocks),
            LastHeight = element(2, LastBlock),
            scan_history(LastHeight + 1, End)
    end.
load_txs([]) -> ok;
load_txs([Block|[NB|T]]) -> 
    Height = element(2, Block),
    Hash = element(3, NB),
    %{ok, Hash} = utils:talk({block_hash, Height}),
    Txs = element(11, Block),
    load_txs2(Txs, Hash, Height),
    load_txs(T);
load_txs([Block]) -> 
    Height = element(2, Block),
    {ok, Hash} = utils:talk({block_hash, Height}),
    Txs = element(11, Block),
    load_txs2(Txs, Hash, Height),
    ok;
load_txs([]) -> ok.
load_txs2([], _, _) -> ok;
load_txs2([Tx|T], Hash, Height) -> 
    add(Tx, Hash, Height),
    load_txs2(T, Hash, Height).
test() ->
%blocks(Start, End) ->
    {ok, Height} = utils:talk({height}),
    {ok, Blocks} = utils:talk({blocks, 1900, 2200}),
    %Block = hd(Blocks),
    %BH = element(2, Block),
    %{ok, Hash} = utils:talk({block_hash, BH}),
    %Txs = element(11, Block),
    load_txs(Blocks).
    %Stx = hd(tl(Txs)),
    %ID = add(Stx, Hash),
    %{ok, TxData} = read(ID),
    %TxData.
    
