-module(scan).
-export([doit/0]).

doit() ->
    {ok, Height} = utils:talk({height}),
    scan_history(0, Height+1),
    scan_sub_accounts().
scan_history(N, M) when N >= M -> ok;
scan_history(Start, End) -> 
    io:fwrite("scanning blocks at \n"),
    io:fwrite(integer_to_list(Start)),
    io:fwrite(" - "),
    io:fwrite(integer_to_list(End)),
    io:fwrite("\n"),
    {ok, Blocks} = utils:talk({blocks, Start, End}),
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
    Height = element(2, Block),
    %io:fwrite("scanning txs at \n"),
    %io:fwrite(integer_to_list(Height)),
    %io:fwrite("\n"),
    Hash = element(3, NB),
    %{ok, Hash} = utils:talk({block_hash, Height}),
    Txs = element(11, Block),
    load_txs2(Txs, Hash),
    load_txs([NB|T]);
load_txs([Block]) -> 
    Height = element(2, Block),
    %io:fwrite("scanning txs at \n"),
    %io:fwrite(integer_to_list(Height)),
    %io:fwrite("\n"),
    {ok, Hash} = utils:talk({block_hash, Height}),
    Txs = element(11, Block),
    load_txs2(Txs, Hash),
    ok;
load_txs([]) -> ok.
load_txs2([], _) -> ok;
load_txs2([Tx|T], Hash) -> 
    Txid = txs:add(Tx, Hash),
    %io:fwrite("processing a tx \n"),
    %io:fwrite(packer:pack(Tx)),
    %io:fwrite("\n"),
    account_process(Tx, Txid),
    load_txs2(T, Hash).
account_process({coinbase, Pub, _, _}, Txid) ->
    accounts:add_tx(Pub, Txid);
account_process({signed, Tx, _, _}, Txid) ->
    if
        (element(1, Tx) == multi_tx) ->
            accounts:add_tx(element(2, Tx), Txid),
            lists:map(fun(Z) ->
                              ap2(Z, Txid)
                      end, element(5, Tx));
        true ->
            ap2(Tx, Txid)
    end.

ap2(Tx, ID) ->
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
            market_new_tx -> [2];
            market_liquidity_tx -> [2];
            market_swap_tx -> [2];
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
               
            
                
make_market_id(CID1, Type1, CID2, Type2) ->
    <<N1:256>> = CID1,
    <<N2:256>> = CID2,
    if
        ((N1+Type1) =< (N2+Type2)) ->
            X = <<CID1/binary,
                  CID2/binary,
                  Type1:16,
                  Type2:16>>,
            crypto:hash(sha256, sign:serialize(X));
        true ->
            make_market_id(CID2, Type2, CID1, Type1)
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
    
