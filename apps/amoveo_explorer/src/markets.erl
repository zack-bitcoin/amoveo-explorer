-module(markets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        read/1, add/8, large_ones/0, test/0,
        cid1/1, cid2/1]).

-record(market, {mid, height, volume = 0, txs = [], cid1, type1, cid2, type2}).

cid1(M) ->
    M#market.cid1.
cid2(M) ->
    M#market.cid2.

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, MID, Volume, Txs, Height, CID1, Type1, CID2, Type2}, X) -> 
    DF = dict:find(MID, X),
    if
        {DF, CID1} == {error, 0} ->
            {noreply, X};
        true ->
            M = case DF of
                    {ok, Market} -> Market;
                    error -> 
                        #market{
                      mid = MID, height = Height, 
                      cid1 = CID1, type1 = Type1, 
                      cid2 = CID2, type2 = Type2}
                end,
            OldHeight = M#market.height,
            DH = max(0, Height - OldHeight),
            V1 = round(M#market.volume * 
                           math:pow(129 / 130, DH)),
            M2 = M#market{
                   height = OldHeight + DH, 
                   volume = V1 + Volume,
                   txs = merge(Txs, M#market.txs)
                  },
            X2 = dict:store(MID, M2, X),
            {noreply, X2}
    end;
handle_cast(_, X) -> {noreply, X}.
handle_call(large, _From, X) -> 
    Keys = dict:fetch_keys(X),
    Markets = 
        lists:map(
          fun(Key) ->
                  dict:fetch(Key, X)
          end, Keys),
    Markets2 = 
        lists:sort(
          fun(A, B) -> 
                  % TODO: volume should decrease depending on how long it has been since it was updated.
                  A#market.volume <
                      B#market.volume
          end, Markets),
    Markets3 = 
        lists:map(
          fun(Market) ->
                  Market#market.mid
          end, Markets2),
    {reply, Markets3, X};
handle_call({read, MID}, _From, X) -> 
    {reply, dict:find(MID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

read(MID) ->
    gen_server:call(?MODULE, {read, MID}).
large_ones() ->
    gen_server:call(?MODULE, large).
add(MID, Volume, Txs, Height, CID1, Type1, CID2, Type2) ->
    gen_server:cast(?MODULE, {add, MID, Volume, Txs, Height, CID1, Type1, CID2, Type2}).



merge([], X) -> X;
merge([H|T], L) -> 
    B = is_in(H, L),
    if
        B -> merge(T, L);
        true -> merge(T, [H|L])
    end.
is_in(_, []) -> false;
is_in(H, [H|_]) -> true;
is_in(H, [_|T]) -> 
    is_in(H, T).


test() ->
    MID = hash:doit(1),
    CID = hash:doit(2),

    add(MID, 100, [], 1, CID, 1, <<0:256>>, 0),
    add(MID, 100, [], 5, CID, 1, <<0:256>>, 0),
    add(MID, 100, [], 4, CID, 1, <<0:256>>, 0),
    {ok, Market} = read(MID),
    [MID] = large_ones(),
    success.
    
