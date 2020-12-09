-module(solution).

-export([part1/0, part2/0]).

%% Part 1
part1() ->
    sum([currect_sequence(Line) || Line <- read_input()]).

currect_sequence({Min, Max, Letter, Sequence}) ->
    Letters = occurance_of_letter(Letter, Sequence, 0),
    case Letters of
        L when L >= Min, L =< Max -> 1;
        _ -> 0
    end.

occurance_of_letter(_, [], Sum) -> Sum;
occurance_of_letter(Letter, [H | T], Sum) ->
    case Letter =:= H of
        true -> occurance_of_letter(Letter, T, Sum + 1);
        false -> occurance_of_letter(Letter, T, Sum)
    end.

%% Part 2
part2() -> sum([right_index(Line) || Line <- read_input()]).

right_index({First, Second, Letter, Sequence}) ->
    Found = right_index(First, Second, Letter, Sequence, 1),
    case Found =:= 1 of
        true -> 1;
        false -> 0
    end.

right_index(_, _, _, [], _) -> 0;
right_index(First, Second, Letter, [H | T], Index) ->
    check_index(First, Index, Letter, H) +
        check_index(Second, Index, Letter, H)
        + right_index(First, Second, Letter, T, Index + 1).

check_index(Num, Index, Letter, H) ->
    case Index =:= Num of
        true -> compare_letter(Letter, H);
        false -> 0
    end.

compare_letter(Letter, H) ->
    case Letter =:= H of
        true -> 1;
        false -> 0
    end.

%% Common
sum([]) -> 0;
sum([H | T]) -> H + sum(T).

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
    Num1 = extract_int(Captured, Line, 2),
    Num2 = extract_int(Captured, Line, 3),
    [Letter | _] = extract_string(Captured, Line, 4),
    Sequence = extract_string(Captured, Line, 5),
    {Num1, Num2, Letter, Sequence}.

extract_string(Captured, Line, Index) ->
    {Start, Length} = lists:nth(Index, Captured),
    Sliced = string:slice(Line, Start, Length),
    binary:bin_to_list(Sliced).

extract_int(Captured, Line, Index) ->
    {Start, Length} = lists:nth(Index, Captured),
    Sliced = string:slice(Line, Start, Length),
    {Result, _} = string:to_integer(Sliced),
    Result.
