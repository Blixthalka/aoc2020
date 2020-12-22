-module(solution).
-export([part1/0, part2/0]).

%% Part 1
part1() -> search_part1(read_input()).

search_part1([]) -> 0;
search_part1([H | T]) -> search_part1(H, T) + search_part1(T).

search_part1(_, []) -> 0;
search_part1(V, [H | _]) when V + H =:= 2020 -> 
    V * H;
search_part1(V, [_ | T]) -> 
    search_part1(V, T).

%% Part 2
part2() -> search_part2(read_input()).

search_part2([]) -> 0;
search_part2([H | T]) -> search_part2(H, T) + search_part2(T).

search_part2(_, []) -> 0;
search_part2(V, [H | T]) -> search_part2(V, H, T) + search_part2(V, T).

search_part2(_, _, []) -> 0;
search_part2(V1, V2, [H | T]) ->
    if V1 + V2 + H =:= 2020 -> V1 * V2 * H;
       true -> search_part2(V1, V2, T)
    end.

%% Common
read_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Strings = string:split(Data, "\n", all),
    Pairs = [string:to_integer(X) || X <- Strings],
    {Result, _} = lists:unzip(Pairs),   
    Result.
