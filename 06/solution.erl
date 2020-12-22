-module(solution).

-export([part1/0, part2/0]).

%% Part 1
part1() ->
    Groups = read_input(),
    sum([count_unique_answers(Group) || Group <- Groups]).

count_unique_answers(Group) -> 
    length(unique_answers(Group, [])).

unique_answers([], Counted) -> Counted;
unique_answers([H | T], Counted) ->
    case lists:member(H, Counted) of 
        true -> unique_answers(T, Counted);
        false -> unique_answers(T, [H] ++ Counted)
    end.

%% Part 2
part2() -> 1.


%% Common 
sum(L) -> sum(L, 0).
sum([], Sum) -> Sum;
sum([H | T], Sum) -> sum(T, H + Sum).

read_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Groups = re:split(Data, "^\s*$", [multiline, {return, list}]),
    [string:join(string:replace(Group, "\n", "", all), "") || Group <- Groups].