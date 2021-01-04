-module(solution).
-import(lists, [map/2, sum/1, filter/2]).
-export([part1/0, part2/0]).

%% Part 1
part1() ->
    sum(map(fun count_unique_answers/1, read_input())).

count_unique_answers(Group) -> 
    length(unique_answers(lists:flatten(Group), [])).

unique_answers([], Counted) -> Counted;
unique_answers([H | T], Counted) ->
    case lists:member(H, Counted) of 
        true -> unique_answers(T, Counted);
        false -> unique_answers(T, [H] ++ Counted)
    end.

%% Part 2
part2() -> sum(map(fun count_same_answer/1, read_input())).

count_same_answer([Person|Persons]) -> 
    length(same_answers(Persons, Person)).

same_answers([], Answers) -> Answers;
same_answers(_, []) -> [];
same_answers([H|T], Answers) -> 
   same_answers(T, filter(fun(X) -> lists:member(X, H) end, Answers)).


%% Common 
read_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Groups = re:split(Data, "^\s*$", [multiline, {return, list}]),
    map(fun parse_group/1, Groups).

parse_group(Group) -> 
    Persons = string:split(Group, "\n", all),
    filter(fun(Person) -> Person =/= [] end, Persons).