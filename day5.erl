-module(day5).
-export([part1/0, part2/0, get_opcode/1, run_program/2]).

run_program(Program, Input) when is_list(Program) -> 
  P = intcode:get_program("inputs/day5.txt"), 
  run_program(P, Input) ;
run_program(Program, Input) -> run_code_acc({0, Program}, Input, []).

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

op_add(Id, [A1, A2, {_, Si} | _], Program) -> 
  V1 = get_value(A1, Program), 
  V2 = get_value(A2, Program),
  S  = maps:get(Si, Program),                          
  {Id+4, maps:update(S, V1 + V2, Program)}.

op_multiply(Id, [A1, A2, {_, Si} | _], Program) -> 
  V1 = get_value(A1, Program), 
  V2 = get_value(A2, Program),
  S  = maps:get(Si, Program),                          
  {Id+4, maps:update(S, V1 * V2, Program)}.

op_store(Id, V1,[{_,Si} | _], Program) -> 
  S = maps:get(Si, Program),
  {Id+2, maps:update(S, V1, Program)}.

op_jump_true(Id, [A1, S2 | _], Program) ->
  V1 = get_value(A1, Program), 
  Next_id = if
              V1 =/= 0 -> get_value(S2, Program) ;
              true     -> Id + 3
            end,
  {Next_id, Program}.

op_jump_false(Id, [A1, S2 | _], Program) ->
  V1 = get_value(A1, Program), 
  Next_id = if
              V1 =:= 0 -> get_value(S2, Program) ;
              true     -> Id + 3
            end,
  {Next_id, Program}.

op_less_than(Id, [A1, A2, {_, S1} | _], Program) ->
  V1 = get_value(A1, Program), 
  V2 = get_value(A2, Program),
  S = maps:get(S1, Program),
  Store =
    if
      V1 < V2 -> 1 ;
      true    -> 0
    end,
  {Id+4, maps:update(S, Store, Program)}.

op_equals(Id, [A1, A2, {_, S1} | _], Program) ->
  V1 = get_value(A1, Program), 
  V2 = get_value(A2, Program),
  S = maps:get(S1, Program),
  Store =
    if
      V1 =:= V2 -> 1 ;
      true      -> 0
    end,
  {Id+4, maps:update(S, Store, Program)}.



run_code_acc({Id, Program}, Input, Output) -> 
  { Op, Modes } = get_opcode(maps:get(Id, Program)),
  [{_, Si} | _] = Args = lists:zip(Modes, lists:map(fun (X) -> X + Id end, [1,2,3])),
  case Op of
    99 -> {Program, lists:reverse(Output) } ;
     1 -> run_code_acc(op_add(Id, Args, Program), Input, Output) ;
     2 -> run_code_acc(op_multiply(Id, Args, Program), Input, Output) ;
     3 -> run_code_acc(op_store(Id, Input, Args, Program), Input, Output) ;
     4 -> run_code_acc({Id+2, Program}, Input, [maps:get(maps:get(Si, Program), Program) | Output]) ;
     5 -> run_code_acc(op_jump_true(Id, Args, Program), Input, Output) ;
     6 -> run_code_acc(op_jump_false(Id, Args, Program), Input, Output) ;
     7 -> run_code_acc(op_less_than(Id, Args, Program), Input, Output) ;
     8 -> run_code_acc(op_equals(Id, Args, Program), Input, Output) 
  end.

part1() ->
  Program = intcode:get_program("inputs/day5.txt"), 
  run_program(Program, 1).

part2() ->
  Program = intcode:get_program("inputs/day5.txt"),
  run_program(Program, 5).
