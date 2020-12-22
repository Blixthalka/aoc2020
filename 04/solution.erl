-module(solution).

-export([part1/0, part2/0]).

%% Part 1
part1() -> 
    Valid_passports = [ valid1(Passport) || Passport <- read_input()],
    sum(Valid_passports).


valid1(Passport) -> 
    convert(contains_required_fields(Passport)).

convert(true) -> 1;
convert(false) -> 0.

contains_required_fields(Passport) -> 
    Keys = [ Key || {Key, _} <- Passport],
    lists:member("byr", Keys) andalso 
    lists:member("eyr", Keys) andalso
    lists:member("hgt", Keys) andalso
    lists:member("hcl", Keys) andalso
    lists:member("ecl", Keys) andalso
    lists:member("pid", Keys) andalso
    lists:member("iyr", Keys).

%% Part 2
part2() ->
    Valid_passports = [ valid2(Passport) || Passport <- read_input()],
    sum(Valid_passports).


valid2(Passport) -> 
    Result = [ valid_data(PassportFields) || PassportFields <- Passport] ++ [contains_required_fields(Passport)],
    convert(not lists:member(false, Result)).

valid_data({Key, Value}) when Key =:= "hgt" -> valid_height(Value);
valid_data({Key, Value}) when Key =:= "eyr" -> in_interval(Value, 2020, 2030);
valid_data({Key, Value}) when Key =:= "iyr" -> in_interval(Value, 2010, 2020);
valid_data({Key, Value}) when Key =:= "byr" -> in_interval(Value, 1920, 2002);
valid_data({Key, Value}) when Key =:= "hcl" -> matches_regex(Value, "^#[a-f,0-9]{6}$");
valid_data({Key, Value}) when Key =:= "ecl" -> matches_regex(Value, "^amb|blu|brn|gry|grn|hzl|oth$");
valid_data({Key, Value}) when Key =:= "pid" -> matches_regex(Value, "^[0-9]{9}$");
valid_data({Key, _____}) when Key =:= "cid" -> true;
valid_data(_) -> false.

matches_regex(Value, Regex) -> 
   case re:run(Value, Regex) of 
        {match, _} -> true;
        nomatch -> false
    end.

valid_height(Value) -> 
    Length = string:length(Value),
    Unit = string:slice(Value, Length - 2, 2),
    Number = string:slice(Value, 0, Length - 2),
    case Unit of 
        "cm" -> in_interval(Number, 150, 193);
        "in" -> in_interval(Number, 59, 76);
        _Else -> false
    end.

in_interval(Value, Min, Max) -> 
    {IntValue, _} = string:to_integer(Value),
    (IntValue =< Max andalso IntValue >= Min).

%% Common
sum(L) -> sum(L, 0).
sum([], Sum) -> Sum;
sum([H | T], Sum) -> sum(T, H + Sum).


read_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Passports = re:split(Data, "^\s*$", [multiline, {return, list}]),
    [ parse_passport(Passport) || Passport <- Passports].

parse_passport(Passport) ->
    case re:run(Passport, "([a-z]{3}:[a-z,#,0-9]*)", [multiline, global]) of
        {match, Captured} -> extract_from_regex(Captured, Passport, []);
        nomatch -> throw({"No match for ", Passport})
    end.

extract_from_regex([], _, Extracted) -> Extracted;
extract_from_regex([H|T], Passport, Extracted) -> 
    extract_from_regex(T, Passport, [extract_string(H, Passport) | Extracted]).

extract_string([{Start, Length} | _], Passport) ->
    Key = string:slice(Passport, Start, 3),
    Value = string:slice(Passport, Start + 4, Length - 4),
    {Key, Value}.

