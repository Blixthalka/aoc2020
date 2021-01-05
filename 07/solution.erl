-module(solution).
-import(lists, [map/2, sum/1, filter/2]).
-export([part1/0, part2/0]).

%% Part 1
part1() ->
    Bags = read_input(),
    sum(map(fun(X) -> fits_gold(X, Bags) end, Bags)).

fits_gold({_, CanContain}, Bags) -> 
    Result = search(CanContain, Bags),
    case Result >= 1 of 
        true -> 1;
        false -> 0
    end.

search([{Key, _}|_], _) when Key =:= "shiny gold" -> 1;
search([], _) -> 0;
search([{Key, _}|T], Bags) -> 
    search(find_bag_by_key(Key, Bags), Bags) + search(T, Bags).

find_bag_by_key(Key, [{BagKey, CanContain}|_]) when Key == BagKey -> CanContain;
find_bag_by_key(_, []) -> error;
find_bag_by_key(Key, [_|T]) -> find_bag_by_key(Key, T).

%% Part 2
part2() -> 
    Bags = read_input(),
    find_gold(Bags).

find_gold(Bags) -> 
    [{_, CanContain}] = lists:filter(fun ({Key,_}) -> Key =:= "shiny gold" end, Bags),
    search2(CanContain, Bags) - 1.

search2([], _) -> 1;
search2([{Key, Num}|T], Bags) ->
    Num * search2(find_bag_by_key(Key, Bags), Bags) + search2(T, Bags).

%% Common 
read_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Bags = string:split(Data, "\n", all),
    map(fun parse_bag/1, Bags).

parse_bag(Bag) -> 
    {match, Captured} = re:run(Bag, "^([a-z]+\s[a-z]+) bags contain ([a-z,0-9,\,,\s]+).$", [multiline, global]),
    Matched = tl(hd(Captured)),
    Key = binary_to_list(string:trim(first_match(Matched, Bag))),
    Contain = parse_contain_str(binary_to_list(second_match(Matched, Bag))),
    {Key, Contain}.

first_match(Matched, Value) -> 
    {Start, Length} = hd(Matched),
    string:slice(Value, Start, Length).

second_match(Matched, Value) -> 
    {Start, Length} = hd(tl(Matched)),
    string:slice(Value, Start, Length).

parse_contain_str(ContainStr) -> 
    case string:find(ContainStr, "no") =:= ContainStr of 
        true -> [];
        false -> parse_contain_str2(ContainStr)
    end.

parse_contain_str2(ContainStr) -> 
    ContainBags = string:split(ContainStr, ",", all),
    map(fun parse_contain_bag/1, ContainBags).

parse_contain_bag(Bag) -> 
    {match, Captured} = re:run(Bag, "([0-9]+)\s([a-z]+\s[a-z]+) bag[s]{0,1}", [multiline, global]),
    Matched = tl(hd(Captured)),
    {Num, _} = string:to_integer(first_match(Matched, Bag)),
    Key = string:trim(second_match(Matched, Bag)),
    {Key, Num}.