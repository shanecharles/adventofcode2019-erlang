-module(intcode).
-export([get_program/1]).

get_program(FileName) -> 
  {ok, Data} = file:read_file(FileName),
  to_program(lists:map(fun (X) -> erlang:binary_to_integer(string:trim(X)) end, binary:split(Data, [<<",">>], [global, trim_all]))).

to_program(Input) -> to_program_acc(0, Input, []).

to_program_acc(_Id, [], Acc) -> maps:from_list(Acc) ;
to_program_acc(Id, [H | T], Acc) -> 
  to_program_acc(Id + 1, T, [{Id, H} | Acc]).


