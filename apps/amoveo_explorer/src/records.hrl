-record(swap_tx, {from, offer, fee}).
-record(swap_offer, {
          acc1, start_limit, end_limit, salt,
          amount1, cid1, type1, %this is what acc1 gives.
          amount2, cid2, type2, %this is what acc2 gives.
          fee1, %what acc1 pays in fees
          nonce}).
-record(acc, {balance = 0, %amount of money you have
	      nonce = 0, %increments with every tx you put on the chain. 
	      pubkey = <<>>,
	      bets = 1,%This is a pointer to the merkel tree that stores how many bets you have made in each oracle.
              bets_hash = <<>>}).
-record(sub_acc, {balance = 0,
                 nonce = 0,
                 pubkey,
                 contract_id,
                 type}).