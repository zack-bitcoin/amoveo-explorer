-module(oracles).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
         read_oid/1,
         read_question_hash/1,
         recent/0,
         by_stake/0,
         add_oracle/4,
         oracle_bet/5,
         oracle_close/3
]).
-define(LOC, "oracles.db").
-define(Recents, 50).
-define(ByStakes, 50).

-record(oracle, {oid, height, txs = [], 
                 question, stake = 0, 
                 type, %1 = true, 2 = false, 3 = bad_question
                 closed = 0}).
-record(db, {by_oid = dict:new(), 
             question_hash_to_oid = dict:new(), 
             recent = [], 
             by_stake = []}).

init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> #db{};
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("oracles died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add_oracle, OID, Height, TXID, Question}, X) -> 
    Oracle = #oracle{oid = OID, height = Height, txs = [TXID],
                     question = Question},
    QH = question_to_hash(Question),
    QLDB = X#db.question_hash_to_oid,
    QL = case dict:find(QH, QLDB) of
             error -> [OID];
             {ok, L} -> [OID|L]
         end,
    X2 = X#db{by_oid = dict:store(OID, Oracle, X#db.by_oid),
              question_hash_to_oid = 
                  dict:store(QH, QL, QLDB),
              recent = [{OID, Height}|X#db.recent]},
    {noreply, X2};
handle_cast({oracle_bet, OID, Height, TXID, Stake, Type}, X) -> 
    case dict:find(OID, X#db.by_oid) of
        error ->
            %io:fwrite("ERROR oracle bet for non-existant oracle!\n"),
            {noreply, X};
        {ok, Oracle} ->
            Stake1 = Oracle#oracle.stake,
            Type1 = Oracle#oracle.type,
            {Stake2, Type2}
                = if
                      (Type == Type1) ->
                          {Stake + Stake1, Type};
                      (Stake1 < Stake) ->
                          {Stake - Stake1, Type};
                      (Stake1 >= Stake) ->
                          {Stake1 - Stake, Type1}
                  end,
            Oracle2 = Oracle#oracle{
                        txs = insert_without_repeats(TXID, Oracle#oracle.txs),
                        height = Height,
                        stake = Stake2,
                        type = Type2
                       },
            X2 = X#db{
                   by_oid = dict:store(OID, Oracle2, X#db.by_oid),
                   recent = merge_recent(
                              OID, Oracle#oracle.height,
                              Height, X#db.recent),
                   by_stake = merge_by_stake(
                                OID, Stake1, Stake2,
                               X#db.by_stake)
                  },
            {noreply, X2}
    end;
handle_cast({oracle_close, OID, Height, TXID}, X) -> 
    case dict:find(OID, X#db.by_oid) of
        error ->
            %io:fwrite("ERROR oracle close for non-existant oracle!\n"),
            {noreply, X};
        {ok, Oracle} ->
            Oracle2 = Oracle#oracle{
                        height = Height,
                        txs = insert_without_repeats(TXID, Oracle#oracle.txs),
                        closed = 1
                       },
            X2 = X#db{
                   by_oid = dict:store(OID, Oracle2, X#db.by_oid),
                   %recent = remove_recent(
                  %            OID, Oracle#oracle.height,
                  %           X#db.recent),
                   by_stake = remove_by_stake(
                                OID, Oracle#oracle.stake,
                               X#db.by_stake)
                  },
            {noreply, X2}
    end;
handle_cast(_, X) -> {noreply, X}.
handle_call({read_oid, OID}, _From, X) -> 
    {reply, dict:find(OID, X#db.by_oid), X};
handle_call({read_question_hash, Hash}, _From, X) -> 
    {reply, dict:find(Hash, X#db.question_hash_to_oid), X};
handle_call(recent, _From, X) -> 
    {reply, X#db.recent, X};
handle_call(by_stake, _From, X) -> 
    {reply, X#db.by_stake, X};
handle_call(_, _From, X) -> {reply, X, X}.


read_oid(OID) ->
    gen_server:call(?MODULE, {read_oid, OID}).
read_question_hash(Hash) ->
    gen_server:call(?MODULE, {read_question_hash, Hash}).
recent() ->
    Recent = gen_server:call(?MODULE, recent),
    recent2(Recent).
recent2([]) -> [];
recent2([{H, _}|T]) -> 
    {ok, X} = read_oid(H),
    [X|recent2(T)].
    
by_stake() ->
    S = gen_server:call(?MODULE, by_stake),
    by_stake2(S).
by_stake2([]) -> [];
by_stake2([{H, _}|T]) -> 
    {ok, X} = read_oid(H),
    [X|by_stake2(T)].

add_oracle(OID, Height, TXID, Question) ->
    gen_server:cast(
      ?MODULE, 
      {add_oracle, OID, Height, TXID, Question}).
oracle_bet(OID, Height, TXID, Stake, Type) ->
    case Type of
        1 -> ok;%true
        2 -> ok;%false
        3 -> ok%bad_question
    end,
    gen_server:cast(
      ?MODULE, 
      {oracle_bet, OID, Height, TXID, Stake, Type}).
oracle_close(OID, Height, TXID) ->
    gen_server:cast(
      ?MODULE,
      {oracle_close, OID, Height, TXID}).
    

%internal functions
question_to_hash(Question) ->
    hash:doit(Question).

merge_recent(OID, Height1, Height2, L) ->
    L2 = remove(OID, Height1, L),
    L3 = add(OID, Height2, L2),
    if
        (length(L3) > ?Recents) -> 
            {L4, _} = lists:split(?Recents, L3),
            L4;
        true -> L3
    end.
merge_by_stake(OID, Stake1, Stake2, L) ->
    L2 = remove(OID, Stake1, L),
    L3 = add(OID, Stake2, L2),
    if
        (length(L3) > ?ByStakes) ->
            {L4, _} = lists:split(?ByStakes, L3),
            L4;
        true -> L3
    end.
remove_recent(OID, Height, L) ->
    remove(OID, Height, L).
remove_by_stake(OID, Stake, L) ->
    remove(OID, Stake, L).

remove(_, _, []) -> [];
remove(OID, N, [{OID, N}|L]) -> L;
remove(OID, N, [X|L]) -> 
    [X|remove(OID, N, L)].
add(OID, N, []) -> [{OID, N}];
add(OID, N, [{OID2, M}|T]) when N > M ->
    [{OID, N}|[{OID2, M}|T]];
add(OID, N, [F|L]) ->
    [F|add(OID, N, L)].
    
insert_without_repeats(X, []) -> [X];
insert_without_repeats(X, [X|T]) -> [X|T];
insert_without_repeats(X, [A|T]) -> 
    [A|insert_without_repeats(X, T)].
    
