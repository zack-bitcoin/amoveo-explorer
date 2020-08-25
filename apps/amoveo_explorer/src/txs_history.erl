-module(txs_history).
-behaviour(gen_server).

%unused

%this module allows you to look up a version of the set of existing txs, based on a given block height.

-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        add/3, read/2, test/0]).

-record(acc, {id = <<0:520>>, balance = 0, sub_accs = []}).
-record(node, {id = <<0:520>>, balance = 0, sub_accs = [], left = error, right = error}).
-record(db, {counter = 0, 
             blockhash2node = dict:new(), 
             nodes = dict:new()}).

init(ok) -> 
    D = dict:store(0, #node{}, dict:new()), 
    X = #db{nodes = D},
    {ok, X}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, IDs, PrevHash, NextHash}, X) -> 
    NID = case dict:find(PrevHash, X#db.blockhash2node) of
            error -> 0;
            {ok, F} -> F
        end,
    Counter = X#db.counter,
    {Counter2, NodesDict2, NID2} = 
        add_ids(Counter, IDs, NID, X#db.nodes),
    D2 = dict:store(NextHash, NID2, X#db.blockhash2node),
    X2 = X#db{
           blockhash2node = D2,
           counter = Counter2,
           nodes = NodesDict2
          },
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, ID, BlockHash}, _From, X) -> 
    R = case dict:find(BlockHash, X#db.blockhash2node) of
            error -> "unknown block";
            {ok, NodeRoot} ->
                %ok
                read_tree(ID, NodeRoot, X#db.nodes)
        end,
    {reply, R, X};
handle_call(_, _From, X) -> {reply, X, X}.

add(Txids, PrevBlockHash, NewBlockHash) ->
    gen_server:cast(?MODULE, {add, Txids, PrevBlockHash, NewBlockHash}).
read(Txid, BlockHash) ->
    gen_server:call(?MODULE, {read, Txid, BlockHash}).

add_ids(Counter, [], NID, Dict) ->
    {Counter, Dict, NID};
add_ids(Counter, [ID|IDS], NID, Dict) ->
    {Dict2, Counter2} = add_id(Counter, ID, NID, Dict),
    add_ids(Counter2+1, IDS, Counter2, Dict2).

add_id(Counter, <<H:256>>, error, Dict) ->
    Node = #node{id = <<H:256>>},
    Dict2 = dict:store(Counter+1, Node, Dict),
    {Dict2, Counter + 1};
add_id(Counter, <<H:256>>, RootIDNumber, Dict) ->
    Node = dict:fetch(RootIDNumber, Dict),
    #node{
           id = <<ID:256>>,
           right = RNID,
           left = LNID
         } = Node,
    if
        H > ID ->
            {Dict2, Counter2} = 
                add_id(Counter, <<H:256>>, RNID, Dict),
            Node2 = Node#node{right = Counter2},
            Dict3 = dict:store(Counter2+1, Node2, Dict2),
            {Dict3, Counter2+1};
        H < ID ->
            {Dict2, Counter2} = 
                add_id(Counter, <<H:256>>, LNID, Dict),
            Node2 = Node#node{left = Counter2},
            Dict3 = dict:store(Counter2+1, Node2, Dict2),
            {Dict3, Counter2+1}
    end.

read_tree(_, error, _) -> false;
read_tree(<<ID:256>>, Root, Dict) ->
    Node = dict:fetch(Root, Dict),
    #node{
           id = <<ID2:256>>,
           right = RNID,
           left = LNID
         } = Node,
    if
        (ID == ID2) -> true;
        ID < ID2 -> read_tree(<<ID:256>>, LNID, Dict);
        true -> read_tree(<<ID:256>>, RNID, Dict)
    end.


test() ->
    B1 = hash:doit(1),
    B2 = hash:doit(2),
    Txs1 = [hash:doit(3),
            hash:doit(4)],
    add(Txs1, B1, B2),
    true = read(hd(Txs1), B2),
    true = read(hd(tl(Txs1)), B2),
    false = read(hash:doit(1), B2),
    "unknown block" = read(hd(Txs1), B1),
    success.
   
