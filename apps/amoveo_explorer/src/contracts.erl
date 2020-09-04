-module(contracts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        read/1, add/5, test/0,
        source/1, types/1]).

-record(contract, {cid, source = <<0:256>>, types, markets = [], txs = []}).
source(X) ->
    X#contract.source.
types(X) ->
    X#contract.types.
    
   

init(ok) -> {ok, dict:new()}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, CID, Source, Types, Markets, Txs}, X) -> 
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
                      types = Types};
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
    {reply, dict:find(CID, X), X};
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

add(CID, Source, Types, Markets, Txs) ->
    gen_server:cast(?MODULE, {add, CID, Source, Types, Markets, Txs}).
read(CID) ->
    gen_server:call(?MODULE, {read, CID}).

test() ->
    CID = hash:doit(1),
    Types = 2,
    Markets = [hash:doit(2), hash:doit(3)],
    add(CID, <<0:256>>, Types, Markets, []),
    {ok, #contract{
       cid = CID, 
       types = Types,
       markets = Markets
      }} 
        = read(CID),
    success.
