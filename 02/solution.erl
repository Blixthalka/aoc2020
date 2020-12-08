-module(solution).

-export([part1/0, part2/0]).

%% Part 1
part1() -> [resolve(Line) || Line <- read_input()].

%% Part 2
part2() -> 1.

resolve({_, _, Letter, Sequence}) ->
    count = count(Letter, Sequence, 0).

count(_, [], _) -> 0;
count(Letter, [H | T], Sum) ->
    case Letter == H of
        true -> count(Letter, T, Sum + 1);
        false -> count(Letter, T, Sum)
    end.

%% Common
read_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Lines = string:split(Data, "\n", all),
    [regex(Line) || Line <- Lines].

regex(Line) ->
    Regex = "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$",
    case re:run(Line, Regex) of
        {match, Captured} -> extract_from_regex(Captured, Line);
        nomatch -> throw({"No match for ", Line})
    end.

extract_from_regex(Captured, Line) ->
    {extract_int(Captured, Line, 2),
     extract_int(Captured, Line, 3),
     extract_string(Captured, Line, 4),
     extract_string(Captured, Line, 5)}.

extract_string(Captured, Line, Index) ->
    {Start, Length} = lists:nth(Index, Captured),
    Sliced = string:slice(Line, Start, Length),
    binary:bin_to_list(Sliced).

extract_int(Captured, Line, Index) ->
    {Start, Length} = lists:nth(Index, Captured),
    Sliced = string:slice(Line, Start, Length),
    {Result, _} = string:to_integer(Sliced),
    Result.
