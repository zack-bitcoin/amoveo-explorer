-module(scan).
-export([doit/0, make_market_id/4, cron/1]).


cron(N) ->
    spawn(fun() ->
                  timer:sleep(1000),
                  case utils:talk({height}) of
                      {ok, Height} ->
                          if
                              Height > N -> 
                                  spawn(fun() ->
                                                timer:sleep(5000),
                                                scan_history(Height-5, Height+1)
                                        end),
                                  cron(Height);
                              true -> cron(N)
                          end;
                      _ -> cron(N)
                  end
          end).
                  


doit() ->
    {ok, Height} = utils:talk({height}),
    Start = case utils:test_mode() of
                true -> 0;
                false -> 130000
            end,
%    spawn(fun() ->
              scan_history(Start, Height+1).
%          end).
    %scan_sub_accounts(),
    %scan_markets().
scan_history(N, M) when N >= M -> ok;
scan_history(Start, End) -> 
    io:fwrite("scanning blocks at \n"),
    io:fwrite(integer_to_list(Start)),
    io:fwrite(" - "),
    io:fwrite(integer_to_list(End)),
    io:fwrite("\n"),
    E2 = min(End, Start+50),
    {ok, Blocks} = utils:talk({blocks, Start, E2}),
    %{ok, Blocks} = utils:talk({blocks, 50, Start}),
    case length(Blocks) of
        1 -> 
            io:fwrite("done scanning tx history\n"),
            ok;
        _ ->
            load_txs(Blocks),
            LastBlock = lists:nth(length(Blocks), Blocks),
            LastHeight = element(2, LastBlock),
            scan_history(LastHeight + 1, End)
    end.
load_txs([]) -> ok;
load_txs([Block|[NB|T]]) -> 
    %io:fwrite("scanning txs at \n"),
    %io:fwrite(integer_to_list(Height)),
    %io:fwrite("\n"),
    Hash = element(3, NB),
    Height = element(2, NB),
    %{ok, Hash} = utils:talk({block_hash, Height}),
    Txs = element(11, Block),
    load_txs2(Txs, Hash, Height),
    load_txs([NB|T]);
load_txs([Block]) -> 
    Height = element(2, Block),
    %io:fwrite("scanning txs at \n"),
    %io:fwrite(integer_to_list(Height)),
    %io:fwrite("\n"),
    {ok, Hash} = utils:talk({block_hash, Height}),
    Txs = element(11, Block),
    load_txs2(Txs, Hash, Height),
    ok.
load_txs2([], _, _) -> ok;
load_txs2([Tx|T], Hash, Height) -> 
    Txid = txs:add(Tx, Hash),
    %Txid = crypto:hash(sha256, sign:serialize(Tx)),
    accounts_txids(Tx, Txid),
    accounts_subs(Tx),
    contracts(Tx, Txid),
    markets(Tx, Height, Txid),
    load_txs2(T, Hash, Height).
contracts({signed, Tx, _, _}, Txid) ->
    if
        (element(1, Tx) == multi_tx) ->
            lists:map(fun(Z) ->
                              Z2 = setelement(2, Z, element(2, Tx)),
                              contracts2(Z2, Txid)
                      end, element(5, Tx));
        true ->
            contracts2(Tx, Txid)
    end;
contracts(_, _) -> ok.
contracts2(Tx, Txid) ->
    case element(1, Tx) of
        contract_new_tx ->
%-record(contract_new_tx, {from, contract_hash, fee, many_types, source, source_type}).
            Code = element(3, Tx),
            Types = element(5, Tx),
            Source = element(6, Tx),
            ST = element(7, Tx),
            ID = hash:doit(<<Code/binary,
                             Source/binary,
                             Types:16,
                             ST:16>>),
            contracts:add(ID, Source, Types, [], [Txid], ST);
        _ -> ok
    end.
            
markets({signed, Tx, _, _}, Height, Txid) ->
    if
        (element(1, Tx) == multi_tx) ->
            lists:map(fun(Z) ->
                              Z2 = setelement(2, Z, element(2, Tx)),
                              markets2(Z2, Height, Txid)
                      end, element(5, Tx));
        true ->
            markets2(Tx, Height, Txid)
    end;
markets(_, _, _) -> ok.
markets2(Tx, Height, Txid) ->
    case element(1, Tx) of
        market_new_tx ->
%-record(market_new_tx, {from, nonce = 0, fee, cid1, cid2, type1, type2, amount1, amount2}).
            CID1 = element(5, Tx),
            CID2 = element(6, Tx),
            Type1 = element(7, Tx),
            Type2 = element(8, Tx),
            %Amount1 = element(9, Tx),
            %Amount2 = element(10, Tx),
            MID = make_market_id(CID1, Type1, CID2, Type2),
            contracts:add(CID1, 0, 0, [MID], [Txid], 0),
            contracts:add(CID2, 0, 0, [MID], [Txid], 0),
            markets:add(MID, 0, [Txid], Height, CID1, Type1, CID2, Type2);
        market_swap_tx ->
%-record(market_swap_tx, {from, nonce, fee, mid, give, take, direction, cid1, type1, cid2, type2}).
            MID = element(5, Tx),
            Give = element(6, Tx),
            Direction = element(8, Tx),
            V = case Direction of
                    1 -> %buying type 2.
                        Give;
                    2 -> 0
                end,
            markets:add(MID, V, [Txid], Height, 0,0,0,0);
        market_liquidity_tx ->
%-record(market_liquidity_tx, {from, nonce, fee, mid, amount, cid1, type1, cid2, type2}).
            MID = element(5, Tx),
            %To = element(2, Tx),
            markets:add(MID, 0, [Txid], Height, 0,0,0,0);
        _ -> ok
    end.
accounts_subs({signed, Tx, _, _}) ->
    if
        (element(1, Tx) == multi_tx) ->
            lists:map(fun(Z) ->
                              Z2 = setelement(2, Z, element(2, Tx)),
                              accounts_subs2(Z2)
                      end, element(5, Tx));
        true ->
            accounts_subs2(Tx)
    end;
accounts_subs(_) -> ok.
accounts_subs2(Tx) ->
    %accounts:add_sub(Pub, CID)
    case element(1, Tx) of
        sub_spend_tx ->
%-record(sub_spend_tx, {from, nonce, fee, to, amount, contract, type}).
            To = element(5, Tx),
            CID = element(7, Tx),
            accounts:add_sub(To, CID);
        contract_use_tx ->
%-record(contract_use_tx, {from, nonce, fee, contract_id, amount, many, source, source_type}).
            From = element(2, Tx),
            CID = element(5, Tx),
            accounts:add_sub(From, CID);
        contract_winnings_tx ->
%-record(contract_winnings_tx, {from, nonce, fee, contract_id, amount, sub_account, winner, proof, row}).
            To = element(8, Tx),
            CID = element(5, Tx),
            accounts:add_sub(To, CID);
        market_new_tx ->
%-record(market_new_tx, {from, nonce = 0, fee, cid1, cid2, type1, type2, amount1, amount2}).
            To = element(2, Tx),
            CID1 = element(5, Tx),
            CID2 = element(6, Tx),
            Type1 = element(7, Tx),
            Type2 = element(8, Tx),
            MID = make_market_id(CID1, Type1, CID2, Type2),
            accounts:add_shares(To, MID),
            accounts:add_sub(To, CID1),
            accounts:add_sub(To, CID2);
        market_liquidity_tx ->
%-record(market_liquidity_tx, {from, nonce, fee, mid, amount, cid1, type1, cid2, type2}).
            To = element(2, Tx),
            MID = element(5, Tx),
            CID1 = element(7, Tx),
            CID2 = element(9, Tx),
            accounts:add_shares(To, MID),
            accounts:add_sub(To, CID1),
            accounts:add_sub(To, CID2);
        market_swap_tx ->
%-record(market_swap_tx, {from, nonce, fee, mid, give, take, direction, cid1, type1, cid2, type2}).
            To = element(2, Tx),
            CID1 = element(9, Tx),
            CID2 = element(11, Tx),
            accounts:add_sub(To, CID1),
            accounts:add_sub(To, CID2);
        _ -> ok
    end.
    

accounts_txids({coinbase, Pub, _, _}, Txid) ->
    accounts:add_tx(Pub, Txid);
accounts_txids({signed, Tx, _, _}, Txid) ->
    if
        (element(1, Tx) == multi_tx) ->
            accounts:add_tx(element(2, Tx), Txid),
            lists:map(fun(Z) ->
                              Z2 = setelement(2, Z, element(2, Tx)),
                              accounts_txids2(Z2, Txid)
                      end, element(5, Tx));
        true ->
            accounts_txids2(Tx, Txid)
    end.

accounts_txids2(Tx, ID) ->
    Ls = 
        case element(1, Tx) of
            spend -> [2, 5];
            create_acc_tx -> [2, 5];
            sub_spend_tx -> [2, 5];
            contract_evidence_tx -> [2];
            contract_timeout_tx -> [2];
            contract_winnings_tx -> [2];
            contract_simplify_tx -> [2];
            oracle_new -> [2];
            oracle_close -> [2];
            oracle_winnings -> [2];
            oracle_bet -> [2];
            market_new_tx -> [2];%TODO add market to markets
            market_liquidity_tx -> [2];%TODO update market liquidity
            market_swap_tx -> [2];%TODO update market volume
            _ -> []
        end,
    lists:map(fun(N) ->
                      accounts:add_tx(element(N, Tx),
                                      ID)
              end, Ls).

scan_sub_accounts() ->
    {ok, SAs} = utils:talk({accounts, 2}),
    %{sub_acc, balance, nonce, pubkey, cid, type}
    lists:map(fun(SA) ->
                      Pub = element(4, SA),
                      CID = element(5, SA),
                      Type = element(6, SA),
                      case Type of
                          0 -> accounts:add_shares(Pub, CID);
                          _ -> accounts:add_sub(Pub, CID)
                      end
              end, SAs),
    ok.
scan_markets() ->
    {ok, Markets} = utils:talk({market, 2}),
    lists:map(fun(Market) ->
                      %markets:add(MID, Volume, Liquidity, Txs, Height)
                      MID = element(2, Market), 
                      Liquidity = element(9, Market),
                      markets:add(MID, 0, 0, [], 0)
              end, Markets),
    ok.

make_market_id(CID1, Type1, CID2, Type2) ->
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
            make_market_id(CID2, Type2, CID1, Type1)
    end.
    









unused(Tx, ID) ->
    case element(1, Tx) of
        sub_spend_tx -> 
            CID = element(7, Tx),
            accounts:add_sub(element(2, Tx),
                             CID),
            accounts:add_sub(element(5, Tx),
                             CID);
        swap_tx -> 
            Acc2 = element(2, Tx),
            Offer = element(3, Tx),
            Acc1 = element(2, Offer),
            CID1 = element(7, Offer),
            CID2 = element(10, Offer),
            accounts:add_sub(Acc1, CID1),
            accounts:add_sub(Acc1, CID2),
            accounts:add_sub(Acc2, CID1),
            accounts:add_sub(Acc2, CID2),
            accounts:add_tx(Acc1, ID),
            accounts:add_tx(Acc2, ID);
        market_new_tx ->
            Acc = element(2, Tx),
            CID1 = element(5, Tx),
            CID2 = element(6, Tx),
            Type1 = element(7, Tx),
            Type2 = element(8, Tx),
            MID = make_market_id(CID1, Type1, CID2, Type2),
            accounts:add_shares(Acc, MID),
            accounts:add_sub(Acc, CID1),
            accounts:add_sub(Acc, CID2);
        market_liquidity_tx ->
            Acc = element(2, Tx),
            MID = element(5, Tx),
            CID1 = element(7, Tx),
            CID2 = element(9, Tx),
            accounts:add_shares(Acc, MID),
            accounts:add_sub(Acc, CID1),
            accounts:add_sub(Acc, CID2);
        market_swap_tx ->
            Acc = element(2, Tx),
            MID = element(5, Tx),
            CID1 = element(9, Tx),
            CID2 = element(11, Tx),
            accounts:add_shares(Acc, MID),
            accounts:add_sub(Acc, CID1),
            accounts:add_sub(Acc, CID2);
        _ -> ok
    end.
               
            
                
            
    
test() ->
%blocks(Start, End) ->
    {ok, Height} = utils:talk({height}),
    {ok, Blocks} = utils:talk({blocks, 1900, 2200}),
    %Block = hd(Blocks),
    %BH = element(2, Block),
    %{ok, Hash} = utils:talk({block_hash, BH}),
    %Txs = element(11, Block),
    load_txs(Blocks).
    %Stx = hd(tl(Txs)),
    %ID = add(Stx, Hash),
    %{ok, TxData} = read(ID),
    %TxData.
    
