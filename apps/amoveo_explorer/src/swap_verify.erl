-module(swap_verify).
-export([doit/2, keep_longer/3]).

-include("records.hrl").
-define(verbose, true).

doit(TID, S) ->
    FN = utils:server_url(external),
    {ok, Height} = talker:talk({height}, FN),
    doit(TID, S, Height).
doit(TID, S, Height) ->
    FN = utils:server_url(external),
    FNL = utils:server_url(internal),

    Offer = element(2, S),
    #swap_offer{
                 start_limit = SL,
                 fee1 = Fee1,
                 amount1 = Amount1,
                 cid1 = CID1,
                 cid2 = CID2,
                 acc1 = Acc1
               } = Offer,
    B1 = sign:verify_sig(Offer, element(3, S), Acc1),

    %check acc1 is putting some minimum amount into it.
    B2 = Fee1 + Amount1 > 1000000,

    %check that we aren't already storing a trade with this id
    B4 = error == swap_full:read(TID),

    %check that the start height limit has already occured.
    B5 = (Height >= SL),

    %TODO verify that all the contract ids are in the binary_contracts database. `binary_contracts:read(CID)`
    B6 = ((not(error == binary_contracts:read_contract(CID1))) or
                                                                 (not(error == scalar_contracts:read_contract(CID1)))),
    B7 = ((not(error == binary_contracts:read_contract(CID2))) or
                                                                 (not(error == scalar_contracts:read_contract(CID2)))),
    B8 = B1 and B2 and B4 and B5 and B6 and B7,
    if
        (?verbose and (not B8))-> 
            io:fwrite("swap verify, bad offer.\n"),
            io:fwrite(packer:pack([B1, B2, B4, B5, B6, B7])),
            io:fwrite("\n"),
            false;
        (not B8) -> false;
        true -> keep_longer(Offer, Height, TID)
    end.

keep_longer(Offer, Height, TID) ->
    FNL = utils:server_url(internal),
    FN = utils:server_url(external),
    #swap_offer{
             end_limit = EL,
             amount1 = Amount1,
             amount2 = Amount2,
             salt  = Salt,
             acc1 = Acc1,
             cid1 = CID1,
             type1 = Type1,
             nonce = Nonce
          } = Offer,

    %check that it isn't expired.
    B2 = (Height =< EL),

    %check the nonce.
    {ok, [_, BlockHash]} = talker:talk({top, 1}, FNL),
    B3 = case CID1 of
             <<0:256>> ->
                 {ok, Acc} = talker:talk({account, Acc1, BlockHash}, FNL),
                 {ok, Acc0} = talker:talk({account, Acc1}, FNL),
                 if
                     Acc == 0 -> false;
                     true ->
                         (Acc0#acc.nonce =< Nonce)
                             and (Acc#acc.balance >= Amount1)
                 end;
             _ ->
                 %Key = sub_accounts:make_key(Acc1, CID1, Type1),
                 {ok, SubAcc} = talker:talk({sub_account, Acc1, CID1, Type1, BlockHash}, FNL),
                 {ok, SubAcc0} = talker:talk({sub_account, Acc1, CID1, Type1}, FNL),
                 if
                     SubAcc == 0 -> false;
                     true ->
                         (SubAcc0#sub_acc.nonce =< Nonce)
                             and (SubAcc#sub_acc.balance >= Amount1)
                 end
         end,

    %check that the trade id isn't already consumed
    B4 = {ok, 0} == talker:talk({trade, TID}, FNL),

    %TODO, check that the swap isn't already in the tx pool.
    {ok, Txs} = talker:talk({txs}, FN),
    B5 = no_repeats(Acc1, Salt, Txs),

    B6 = B2 and B3 and B4 and B5,
    if
        (?verbose and not(B6)) ->
            io:fwrite(packer:pack(swap_full:read(TID))),
            io:fwrite("\n"),
            io:fwrite(packer:pack([B2, B3, B4, B5])),
            io:fwrite("\n"),
            false;
        not(B6) ->
            false;
        true -> 
            true
    end.
    
no_repeats(_, _, []) -> true;
no_repeats(Acc, Salt, 
           [{signed, 
             #swap_tx{
               offer = 
                   {signed,
                    #swap_offer{
                      acc1 = Acc,
                      salt = Salt
                     },
                    _, _}
              }, _, _}|_]) -> 
    false;
no_repeats(Acc, Salt, [_|T]) -> 
    no_repeats(Acc, Salt, T).
    
