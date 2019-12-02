-module(day1).
-export([run/0, calcFuel/1, part1/1, part2/1, getInput/1, calcTotalFuelWeight/1]).

getInput(FileName) ->
    {ok, Data} = file:read_file(FileName),
    lists:map(fun erlang:binary_to_integer/1, binary:split(Data, [<<"\n">>], [global, trim_all])).

calcFuel(X) -> floor(X / 3) - 2.

part1(FileName) -> 
  Input = getInput(FileName),
  lists:sum(lists:map(fun day1:calcFuel/1, Input)).

calcFuel_acc(Acc, X) when X > 0 -> calcFuel_acc(X + Acc, calcFuel(X)) ;
calcFuel_acc(Acc, _X) -> Acc.

calcTotalFuelWeight(X) -> calcFuel_acc(0, calcFuel(X)).

part2(FileName) ->
  Input = getInput(FileName),
  lists:sum(lists:map(fun day1:calcTotalFuelWeight/1, Input)).

run() -> 
  File = "inputs/day1.txt",
  {day1, [{part1, part1(File)}, {part2, part2(File)}]}.
