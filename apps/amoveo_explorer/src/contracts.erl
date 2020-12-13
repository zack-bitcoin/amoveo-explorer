-module(contracts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        read/1, add/6, test/0,
         large_ones/0,
        source/1, types/1]).

-record(contract, {cid, source = <<0:256>>, types, markets = [], txs = [], source_type = 0}).
source(X) ->
    X#contract.source.
types(X) ->
    X#contract.types.
    
   

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, CID, Source, Types, Markets, Txs, SourceType}, X) -> 
    DF = dict:find(CID, X),
    if
        {DF, Types} == {error, 0} ->
            {noreply, X};
        true ->
            C = case DF of
                    error ->
                        #contract{
                      source = Source,
                      cid = CID,
                      types = Types,
                      source_type = SourceType};
                    {ok, Z} -> Z
                end,
            C2 = C#contract{
                   markets = merge(
                               C#contract.markets, 
                               Markets),
                   txs = merge(
                           C#contract.txs,
                           Txs)
                  },
            X2 = dict:store(CID, C2, X),
            {noreply, X2}
    end;
handle_cast(_, X) -> {noreply, X}.
handle_call({read, CID}, _From, X) -> 
    %io:fwrite("in genserver\n"),
    {reply, dict:find(CID, X), X};
handle_call(_, _From, X) -> {reply, X, X}.

large_ones() ->
    {ok, Height} = utils:talk({height}),
    %gen_server:call(?MODULE, {large, Height}).
    X = gen_server:call(?MODULE, dict),
    Keys = dict:fetch_keys(X),
    Contracts = 
        lists:map(
          fun(Key) ->
                  dict:fetch(Key, X)
          end, Keys),
    CIDS =
        lists:map(
          fun(Contract) ->
                  Contract#contract.cid
          end, Contracts),
    Contracts2 = 
        lists:map(
          fun(CID) ->
                  {ok, C} = talker:talk({contracts, CID},
                                        utils:server_url(external)),
                  {CID, C}
          end, CIDS),
    Ls = lists:map(
           fun({CID, C}) ->
                   Source = element(9, C),
                   SourceType = element(10, C),
                   MID1 = markets:make_id(Source, SourceType, CID, 1),
                   MID2 = markets:make_id(Source, SourceType, CID, 2),
                   MID3 = markets:make_id(CID, 1, CID, 2),
                   [K1, K2, K3] = 
                       lists:map(fun(MID) ->
                                         {ok, M} = 
                                             talker:talk({markets, MID},
                                                         utils:server_url(external)),
                                         if
                                             M == 0 -> 0;
                                             true ->
                                                 math:sqrt(element(5, M) * element(8, M))
                                         end
                                 end,
                                 [MID1, MID2, MID3]),
                   Liquidity = K1 + K2 + K3,
                   {Liquidity, C}
           end, Contracts2),
    
    Contracts3 =
        lists:sort(
          fun({L1, C1}, {L2, C2}) ->
                  L1 > L2
          end, Ls),
    Contracts4 = lists:map(
                  fun({_, C}) ->
                          C
                  end, Contracts3),
    %Contracts3 =
    %    lists:sort(
    %      fun(C1, C2) ->
    %              (element(12, C1)) >
    %                  (element(12, C2))
    %      end, Contracts2),
    {Contracts5, _} = 
        lists:split(min(10, length(Contracts4)),
                    Contracts4),
    Contracts5.

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

add(CID, Source, Types, Markets, Txs, SourceType) ->
    gen_server:cast(?MODULE, {add, CID, Source, Types, Markets, Txs, SourceType}).
read(CID) ->
    %io:fwrite("about to call gen server\n"),
    Y = gen_server:call(?MODULE, {read, CID}),
    %io:fwrite("returned from gen server\n"),
    case Y of
        error -> 0;
        X -> X
    end.
            

test() ->
    CID = hash:doit(1),
    Types = 2,
    Markets = [hash:doit(2), hash:doit(3)],
    add(CID, <<0:256>>, Types, Markets, [], 0),
    {ok, #contract{
       cid = CID, 
       types = Types,
       markets = Markets
      }} 
        = read(CID),
    success.
