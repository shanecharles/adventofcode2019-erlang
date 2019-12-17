-module(day2).
-export([part1/1, part2/2, get_program/1, to_program/1, run_program/1]).

get_program(FileName) -> 
  {ok, Data} = file:read_file(FileName),
  to_program(lists:map(fun (X) -> erlang:binary_to_integer(string:trim(X)) end, binary:split(Data, [<<",">>], [global, trim_all]))).

to_program(Input) -> to_program_acc(0, Input, []).

to_program_acc(_Id, [], Acc) -> maps:from_list(Acc) ;
to_program_acc(Id, [H | T], Acc) -> 
  to_program_acc(Id + 1, T, [{Id, H} | Acc]).

run_program(Program) -> run_code_acc(0, Program).

process_command(Fn, Idx1, Idx2, Dest, Program) ->
  #{Idx1:=R1, Idx2:=R2, Dest:=S} = Program,
  #{R1:=V1, R2:=V2} = Program,
  maps:update(S, Fn(V1,V2), Program).

run_code_acc(Id, Program) -> 
  case maps:get(Id, Program) of
    99 -> Program ;
     1 -> Add = fun(X,Y) -> X+Y end,
          run_code_acc(Id+4, process_command(Add, Id+1, Id+2, Id+3, Program)) ;
     2 -> Mul = fun(X,Y) -> X*Y end,
          run_code_acc(Id+4, process_command(Mul, Id+1, Id+2, Id+3, Program)) 
  end.

set_program_code(Hi, Low, Program) -> 
  maps:update(2, Low, maps:update(1, Hi, Program)).

part1(Input) -> 
  Program = get_program(Input),
  run_program(set_program_code(12,2,Program)).

get_middle(Max, Min) -> (Max - Min) div 2 + Min.

search(Max, Min, Expected, Program) ->
  Middle = get_middle(Max,Min),
  Hi = Middle div 100,
  Low = Middle rem 100,
  #{0:=Result} = run_program(set_program_code(Hi,Low,Program)),
  if 
    Result =:= Expected -> Middle ;
    Result < Expected   -> search(Max, Middle, Expected, Program) ;
    Expected < Result   -> search(Middle, Min, Expected, Program)
  end.

part2(Input, Expected) -> 
  Program = get_program(Input),
  search(9999, 0, Expected, Program). 
