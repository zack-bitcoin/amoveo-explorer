-module(utils).
-export([cron_job/2, off/0, server_url/1, talk/1,
         trade_id/2, trade_id/1, market_id/1]).

-define(TestMode, false).
-include("records.hrl").

server_url(T) ->
    L = case T of
          internal -> "1";
          external -> "0"
      end,
    if 
        ?TestMode -> "http://127.0.0.1:301"++L;
        true -> "http://127.0.0.1:808"++L
    end.
talk(X) ->
    Port = 
        if
            ?TestMode -> 3011;
            true -> 8081
        end,
    talker:talk(X, {{127,0,0,1}, Port}).
            

cron_job(Period, F) ->
    spawn(fun() -> 
                  timer:sleep(1000),
                  cron2(F, Period) end).
cron2(F, P) ->
    spawn(F),
    timer:sleep(P * 1000),
    cron2(F, P).

off() ->
    amoveo_explorer_sup:stop(),
    ok = application:stop(amoveo_explorer).
    
trade_id(Salt, Pub) ->
    hash:doit(<<Pub/binary,
                Salt/binary>>).
trade_id(SO) ->
    #swap_offer{
          salt = Salt,
          acc1 = Acc
         } = SO,
    trade_id(Salt, Acc).
             
market_id(S) ->
    #swap_offer{
    cid1 = CID1,
    type1 = T1,
    cid2 = CID2,
    type2 = T2
   } = S,
    hash:doit(<<CID1/binary,
                T1:32,
                CID2/binary,
                T2:32>>).

