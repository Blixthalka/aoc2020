-module(solution).

-export([part1/0, part2/0]).

%% Part 1
part1() -> 
    Valid_passports = [ valid(Passport) || Passport <- read_input()],
    sum(Valid_passports).


valid(Passport) -> 
    convert(contains_required_fields(Passport)).

convert(true) -> 1;
convert(false) -> 0.

contains_required_fields(Passport) -> 
    lists:member("byr", Passport) and 
    lists:member("eyr", Passport) and
    lists:member("hgt", Passport) and
    lists:member("hcl", Passport) and  
    lists:member("ecl", Passport) and 
    lists:member("pid", Passport) and   
    lists:member("iyr", Passport). 

%% Part 2
part2() -> 2.

%% Common
sum(L) -> sum(L, 0).
sum([], Sum) -> Sum;
sum([H | T], Sum) -> sum(T, H + Sum).


read_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Passports = re:split(Data, "^\s*$", [multiline, {return, list}]),
    [ parse_passport(Passport) || Passport <- Passports].

parse_passport(Passport) ->
    case re:run(Passport, "([a-z]{3}:)", [multiline, global])of
        {match, Captured} -> extract_from_regex(Captured, Passport, []);
        nomatch -> throw({"No match for ", Passport})
    end.

extract_from_regex([], _, Extracted) -> Extracted;
extract_from_regex([H|T], Passport, Extracted) -> 
    extract_from_regex(T, Passport, [extract_string(H, Passport) | Extracted]).

extract_string([{Start, Length} | _], Passport) ->
    string:slice(Passport, Start, Length - 1).

