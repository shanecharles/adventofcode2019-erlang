-module(day5).
-export([part1/0, get_opcode/1, run_program/1]).

run_program(Program) when is_list(Program) -> 
  P = intcode:get_program("inputs/day5.txt"), 
  run_program(P) ;
run_program(Program) -> run_code_acc(0, Program, []).

get_parameter_mode(X,Div) -> 
  X div Div rem 10.

get_opcode(X) ->
  { X rem 100, 
    [get_parameter_mode(X, 100), 
     get_parameter_mode(X, 1000),
     get_parameter_mode(X, 10000)] }.

get_value({0, Id}, Program) -> 
  maps:get(maps:get(Id, Program), Program) ;
get_value({1, Id}, Program) -> 
  maps:get(Id, Program).

op_add([A1, A2, {_, Si} | _], Program) -> 
  V1 = get_value(A1, Program), 
  V2 = get_value(A2, Program),
  S  = maps:get(Si, Program),                          
  maps:update(S, V1 + V2, Program).

op_multiply([A1, A2, {_, Si} | _], Program) -> 
  V1 = get_value(A1, Program), 
  V2 = get_value(A2, Program),
  S  = maps:get(Si, Program),                          
  maps:update(S, V1 * V2, Program).

op_store(V1,[{_,Si} | _], Program) -> 
  S = maps:get(Si, Program),
  maps:update(S, V1, Program).


run_code_acc(Id, Program, Output) -> 
  { Op, Modes } = get_opcode(maps:get(Id, Program)),
  [{_, Si} | _] = Args = lists:zip(Modes, lists:map(fun (X) -> X + Id end, [1,2,3])),
  case Op of
    99 -> {Program, lists:reverse(Output) } ;
     1 -> run_code_acc(Id+4, op_add(Args, Program),Output) ;
     2 -> run_code_acc(Id+4, op_multiply(Args, Program), Output) ;
     3 -> run_code_acc(Id+2, op_store(1, Args, Program), Output) ;
     4 -> run_code_acc(Id+2, Program, [maps:get(maps:get(Si, Program), Program) | Output])
  end.

part1() ->
  Program = intcode:get_program("inputs/day5.txt"), 
  run_program(Program).
