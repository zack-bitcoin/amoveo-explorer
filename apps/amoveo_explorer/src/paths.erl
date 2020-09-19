-module(paths).
-export([doit/2, test/0]).

mmi(A,B,C,D) ->
    scan:make_market_id(A, B, C, D).

doit(CID1, CID2) ->
    L1 = to_source(CID1),
    L2 = to_source(CID2),
    L3 = L1 ++ L2,
    L4 = remove_repeats(L3),
    M = all_mids(L4),
    M2 = remove_repeats(M),
    M3 = lists:map(fun(CID) -> 
                           markets:read(CID)
                   end,
                   M2),
    M4 = lists:filter(fun(X) -> not(X == error) end, M3),
    M5 = lists:map(fun({ok, Mar}) -> Mar end, M4),
    M6 = lists:map(fun(Mar) -> 
                           element(2, Mar) 
                   end, M5),
    C1 = remove_repeats(all_contracts(M5)),
    C2 = remove_ele(<<0:256>>, C1),
    {M6, C2}.
remove_ele(_, []) -> [];
remove_ele(X, [X|T]) -> T;
remove_ele(X, [Z|T]) -> 
    [Z|remove_ele(X, T)].
all_contracts([]) -> [];
all_contracts([M|T]) -> 
    [markets:cid1(M),
     markets:cid2(M)] ++
        all_contracts(T).
all_mids([]) -> [];
all_mids([H|T]) -> 
    L1 = all_mids2(H, [H|T]),
    L1 ++ all_mids(T).
all_mids2(_, []) -> [];
all_mids2(X, [H|T]) ->
    all_mids3(X, H) ++
        all_mids2(X, T).
all_mids3({CID1, Types1}, {CID2, Types2}) ->
    all_mids4(CID1, 1, Types1, CID2, 1, Types2).
all_mids4(CID1, T1, T1, <<0:256>>, _, _) ->
    [mmi(CID1, T1, <<0:256>>, 0)];
all_mids4(<<0:256>>, _, _, CID1, T1, T1) ->
    [mmi(CID1, T1, <<0:256>>, 0)];
all_mids4(CID1, T1, T1, CID2, T2, T2) ->
    [mmi(CID1, T1, CID2, T2)];
all_mids4(CID1, T1, L1, <<0:256>>, _, _) ->
    [mmi(CID1, T1, <<0:256>>, 0)|
     all_mids4(CID1, T1+1, L1, <<0:256>>, 0, 0)];
all_mids4(CID1, T1, L1, CID2, T2, T2) ->
    [mmi(CID1, T1, CID2, T2)|
     all_mids4(CID1, T1+1, L1, CID2, min(1, T2), T2)];
all_mids4(<<0:256>>, 0, 0, CID2, T2, L2) ->
    [mmi(<<0:256>>, 0, CID2, T2)|
     all_mids4(<<0:256>>, 0, 0, CID2, T2+1, L2)];
all_mids4(CID1, T1, L1, CID2, T2, L2) ->
    [mmi(CID1, T1, CID2, T2)|
     all_mids4(CID1, T1, L1, CID2, T2+1, L2)].


remove_repeats([]) -> [];
remove_repeats([H|T]) -> 
    B = is_in(H, T),
    if
        B -> remove_repeats(T);
        true -> [H|remove_repeats(T)]
    end.
is_in(H, []) -> false;
is_in(H, [H|_]) -> true;
is_in(H, [_|T]) -> 
    is_in(H, T).

to_source(<<0:256>>) -> [{<<0:256>>, 1}];
to_source(CID) -> 
    case contracts:read(CID) of
        {ok, C} ->
            %{ok, C} = contracts:read(CID),
            S = contracts:source(C),
            T = contracts:types(C),
            [{CID, T}|to_source(S)];
        error ->
            case markets:read(CID) of
                {ok, M} -> 
                    CID1 = markets:cid1(M),
                    CID2 = markets:cid2(M),
                    to_source(CID1)++
                        to_source(CID2);
                error ->
                    io:fwrite("unhandled paths error \n"),
                    1=2
            end
    end.
    


test() ->
    scan:doit(),
    CID1 = base64:decode("1qSyvrk/L3uBxN6uSsaX3oVypSzLpL260/CJl6e4Dq8="),
    CID2 = base64:decode("95bMsSPuGa2s3M6gADqmbdBjuCVTIYc2Nf6KMw4xl48="),
    %doit(CID1, CID2).
    doit(CID1, CID2).
