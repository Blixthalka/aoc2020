-module(solution).

-export([part1/0, part2/0]).

%% Part 1
part1() -> 
    lists:max([ seat_id(Seating) || Seating <- read_input() ]).

seat_id(Seating) -> row(Seating) * 8 + column(Seating).

%% Part 2
part2() -> 
    AllIds = all_seat_ids(),
    PresentIds =  [ seat_id(Seating) || Seating <- read_input() ],
    EmptySeats = [ X || X <- AllIds, not lists:member(X, PresentIds) ],
    find_middle(EmptySeats).

find_middle(EmptySeats) -> find_middle(EmptySeats, -1).
find_middle([H|T], LastNum) when (H - 1) =:= LastNum -> find_middle(T, H);
find_middle([H|_], _) -> H.

all_seat_ids() -> 
    all_seat_ids(0).

all_seat_ids(Num) when Num =:= 8 * 127 -> [];
all_seat_ids(Num) ->  [Num] ++ all_seat_ids(Num + 1).

%% Common
column(Seating) -> 
    search(string:slice(Seating, 7, 4), 0, 7, {"L", "R"}).

row(Seating) ->
     search(string:slice(Seating, 0, 7), 0, 127, {"F", "B"}).

search(_, Min, Max, _) when Min =:= Max -> Min;
search([H|T], Min, Max, Settings = {MinLetter, MaxLetter}) -> 
    Increment = round((Max - Min + 1) / 2),
    case [H] of 
        MinLetter -> search(T, Min, Max - Increment, Settings);
        MaxLetter -> search(T, Min + Increment, Max, Settings)
    end.

read_input() ->
    {ok, Data} = file:read_file("input.txt"),
    Splitted = binary:split(Data, <<"\n">>, [global]),
    [binary:bin_to_list(X) ||  X <- Splitted].
