-module(markets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        read/1, add/9, large_ones/0, test/0,
         make_id/4, swap/6, liquidity/4,
        cid1/1, cid2/1]).
-define(LOC, "markets.db").

-record(market, {mid, height, txs = [], cid1, type1, cid2, type2, amount1, amount2, prices = [], liquidities = []}).

make_id(CID1, Type1, CID2, Type2) ->
    %this is a copy of markets:make_id from amoveo.
    <<N1:256>> = CID1,
    <<N2:256>> = CID2,
    if
        ((N1+Type1) =< (N2+Type2)) ->
            X = <<CID1/binary,
                  CID2/binary,
                  Type1:16,
                  Type2:16>>,
            hash:doit(X);
        true ->
            make_id(CID2, Type2, CID1, Type1)
    end.

cid1(M) ->
    M#market.cid1.
cid2(M) ->
    M#market.cid2.

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
    io:format("markets died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, MID, TxID, Height, CID1, 
             Type1, CID2, Type2, Amount1, 
             Amount2}, X) -> 
    M = #market{
      mid = MID, height = Height, 
      cid1 = CID1, type1 = Type1, 
      cid2 = CID2, type2 = Type2,
      txs = [TxID], amount1 = Amount1,
      amount2 = Amount2},
    X2 = dict:store(MID, M, X),
    {noreply, X2};
handle_cast({liquidity, MID, Height, TxID, Amount}, X) ->
    M = dict:fetch(MID, X),
    #market{amount1 = A1,
            amount2 = A2} = M,
    LS1 = max(1, round(math:sqrt(A1*A2))),
    LS2 = LS1 + Amount,
    B1 = A1 * LS2 / LS1,
    B2 = A2 * LS2 / LS1,
    M2 = M#market{
           amount1 = B1,
           amount2 = B2,
           liquidities = 
               [{Height, LS2}|
                M#market.liquidities]
          },
    X2 = dict:store(MID, M2, X), 
    {noreply, X2};
handle_cast({swap, MID, TxID, Height, 
             Give, Take, Direction}, X) -> 
    M = dict:fetch(MID, X),
    #market{amount1 = B1,
            amount2 = B2} = M,
    K = B1*B2,
    %(Give+B1) * (B2 - Lose) = K
    %K/(Give+B1)
    {A1, A2} = 
        case Direction of
            1 -> {B1 + Give, 
                  K/(Give+B1)};
            2 -> {K/(Give+B2),
                  B2 + Give}
        end,
    M2 = M#market{
           height = Height, 
           txs = [TxID|M#market.txs],
           amount1 = round(A1),
           amount2 = round(A2),
           prices = 
               [{Height, A2/(A1+A2)}|
                M#market.prices]},
    X2 = dict:store(MID, M2, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
%handle_call({large, Height}, _From, X) -> 
%    Keys = dict:fetch_keys(X),
%    Markets = 
%        lists:map(
%          fun(Key) ->
%                  dict:fetch(Key, X)
%          end, Keys),
%    Markets2 = 
%        lists:sort(
%          fun(A, B) -> 
                  % TODO: volume should decrease depending on how long it has been since it was updated.
%                  VA = current_volume(A, Height),
%                  VB = current_volume(B, Height),
%                  VA < VB
%          end, Markets),
%    Markets3 = 
%        lists:map(
%          fun(Market) ->
%                  Market#market.mid
%          end, Markets2),
%    {reply, Markets3, X};
handle_call({read, MID}, _From, X) -> 
    {reply, dict:find(MID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

%current_volume(M, Height) ->
%    OldHeight = M#market.height,
%    DH = max(0, Height - OldHeight),
%    V1 = round(M#market.volume * 
%                   math:pow(129 / 130, DH)),
%    V1.

    

read(MID) ->
    gen_server:call(?MODULE, {read, MID}).
large_ones() ->
    {ok, Height} = utils:talk({height}),
    X = gen_server:call(?MODULE, dict),
    Keys = dict:fetch_keys(X),
    Markets = 
        lists:map(
          fun(Key) ->
                  dict:fetch(Key, X)
          end, Keys),
    Markets2 = Markets,
%    Markets2 = 
%        lists:sort(
%          fun(A, B) -> 
                  % TODO: volume should decrease depending on how long it has been since it was updated.
%                  VA = current_volume(A, Height),
%                  VB = current_volume(B, Height),
%                  VA > VB
%          end, Markets),
%    Markets3 = 
%        lists:map(
%          fun(Market) ->
%                  Market#market.mid
%          end, Markets2),
    {Markets4, _} = 
        lists:split(min(10, length(Markets2)),
                    Markets2),
    Markets4.
%    Markets4 = 
%        lists:map(
%          fun(MID) ->
%                  {ok, M} = utils:talk({markets, MID}),
%                  M
%          end, Markets3),
                          
                                                     


add(MID, TxID, Height, CID1, 
    Type1, CID2, Type2, Amount1, 
    Amount2) ->
    gen_server:cast(
      ?MODULE, 
      {add, MID, TxID, Height, CID1, 
       Type1, CID2, Type2, Amount1, 
       Amount2}).
swap(MID, TxID, Height, Give, 
     Take, Direction) ->
    gen_server:cast(
      ?MODULE,
      {swap, MID, TxID, Height, Give,
       Take, Direction}).
liquidity(MID, Height, TxID, Amount) ->
    gen_server:cast(
      ?MODULE,
      {liquidity, MID, Height, TxID, Amount}).



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
    Txid1 = hash:doit(3),
    Txid2 = hash:doit(4),
    Txid3 = hash:doit(5),

    add(MID, Txid1, 1, CID, 1, <<0:256>>, 0, 1, 1),
    add(MID, Txid3, 4, CID, 1, <<0:256>>, 0, 1, 1),
    add(MID, Txid2, 5, CID, 1, <<0:256>>, 0, 1, 1),
    {ok, Market} = read(MID),
    [MID] = large_ones(),
    success.
    
