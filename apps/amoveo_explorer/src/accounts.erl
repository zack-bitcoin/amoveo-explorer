-module(accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         add/3, read/2, test/0
        ]).

%this module allows you to look up a version of the set of accounts, based on a given block hash.
%This way we can easily revert for orphaned blocks.

-record(acc, {pub, balance, sub_accs}).
-record(node, {id = <<0:520>>, balance, sub_accs, left = error, right = error}).
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
handle_cast({add, Accs, PrevHash, NextHash}, X) -> 
    NID = case dict:find(PrevHash, X#db.blockhash2node) of
            error -> 0;
            {ok, F} -> F
        end,
    Counter = X#db.counter,
    {Counter2, NodesDict2, NID2} = 
        add_accs(Counter, Accs, NID, X#db.nodes),
    D2 = dict:store(NextHash, NID2, X#db.blockhash2node),
    X2 = X#db{
           blockhash2node = D2,
           counter = Counter2,
           nodes = NodesDict2
          },
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, Pub, BlockHash}, _From, X) -> 
    R = case dict:find(BlockHash, X#db.blockhash2node) of
            error -> "unknown block";
            {ok, NodeRoot} ->
                %ok
                read_tree(Pub, NodeRoot, X#db.nodes)
        end,
    {reply, R, X};
handle_call(_, _From, X) -> {reply, X, X}.

add(Accs, PrevBlockHash, NewBlockHash) ->
    gen_server:cast(?MODULE, {add, Accs, PrevBlockHash, NewBlockHash}).
read(Pub, BlockHash) ->
    case gen_server:call(?MODULE, {read, Pub, BlockHash}) of
        empty -> empty;
        "unknown block" -> "unknown block";
        Node ->
    #node{
           id = ID,
           balance = Balance,
           sub_accs = SA
         } = Node,
    #acc{pub = ID,
           balance = Balance,
           sub_accs = SA}
    end.


add_accs(Counter, [], NID, Dict) ->
    {Counter, Dict, NID};
add_accs(Counter, [Acc|IDS], NID, Dict) ->
    {Dict2, Counter2} = add_acc(Counter, Acc, NID, Dict),
    add_accs(Counter2+1, IDS, Counter2, Dict2).
    
add_acc(Counter, Acc, error, Dict) ->
    #acc{ 
         pub = <<H:520>>,
         balance = Balance,
         sub_accs = SubAccs
        } = Acc,
    Node = #node{id = <<H:520>>, 
                 balance = Balance, 
                 sub_accs = SubAccs},
    Dict2 = dict:store(Counter+1, Node, Dict),
    {Dict2, Counter + 1};
add_acc(Counter, Acc, RootIDNumber, Dict) ->
    #acc{
         pub = <<H:520>>,
         balance = Balance,
         sub_accs = SubAccs
        } = Acc,
    Node = dict:fetch(RootIDNumber, Dict),
    #node{
           id = <<ID:520>>,
           right = RNID,
           left = LNID
         } = Node,
    if
        H == ID ->
            Node2 = Node#node{
                      balance = Balance,
                      sub_accs = SubAccs
                     },
            Dict3 = dict:store(Counter+1, Node2, Dict),
            {Dict3, Counter+1};
        H > ID ->
            {Dict2, Counter2} = 
                add_acc(Counter, Acc, RNID, Dict),
            Node2 = Node#node{right = Counter2},
            Dict3 = dict:store(Counter2+1, Node2, Dict2),
            {Dict3, Counter2+1};
        H < ID ->
            {Dict2, Counter2} = 
                add_acc(Counter, Acc, LNID, Dict),
            Node2 = Node#node{left = Counter2},
            Dict3 = dict:store(Counter2+1, Node2, Dict2),
            {Dict3, Counter2+1}
    end.

read_tree(_, error, _) -> empty;
read_tree(<<ID:520>>, Root, Dict) ->
    Node = dict:fetch(Root, Dict),
    #node{
           id = <<ID2:520>>,
           right = RNID,
           left = LNID
         } = Node,
    if
        (ID == ID2) -> Node;
        ID < ID2 -> read_tree(<<ID:520>>, LNID, Dict);
        true -> read_tree(<<ID:520>>, RNID, Dict)
    end.


test() ->
    ID1 = <<1:520>>,
    ID2 = <<2:520>>,
    Acc1 = #acc{pub = ID1, balance = 27, sub_accs = [3]},
    Acc2 = #acc{pub = ID2, balance = 55, sub_accs = [2]},
    Accs = [Acc1, Acc2],
    B1 = hash:doit(1),
    B2 = hash:doit(2),
    add(Accs, B1, B2),
    Acc1 = read(ID1, B2),
    Acc2 = read(ID2, B2),
    empty = read(<<3:520>>, B2),
    "unknown block" = read(ID1, B1),
    success.
   
