-module(pair_buy).
-export([]).

-record(pair_buy_offer, 
        {from, nonce, start_limit,
         end_limit, source_id, 
         source_type, contract_hash,
         new_id, amount1, fee1,
         amount2, fee2, subs1, subs2}).

    
