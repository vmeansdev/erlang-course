-module(lambdas).
-compile(export_all).

%% Напишите lambda-функцию, которая осуществляет произвольную операцию Operation(A, B) -> C (где A, B, C - числа),
%% над двумя числовыми списками попарно, возвращая список результатов операции также в виде списка.
%% Проверьте вашу функцию на разных операциях (erlang:'+', erlang:'xor', erlang:'rem', erlang:'/' и собственной фунции,
%% которая возвращает среднее гармоническое двух чисел H = 2/(1/A + 1/B)).
l_apply(_, [], []) -> [];
l_apply(Fun, ListOne, ListTwo) ->
  Op = fun Operate(Acc, F, [A|TailOne], [B|TailTwo]) ->
             Operate([F(A,B) | Acc], F, TailOne, TailTwo);
           Operate(Acc, _, [], []) -> Acc
       end,
  Operation = fun(F, List1, List2) -> Op([], F, List1, List2) end,
  lists:reverse(Operation(Fun, ListOne, ListTwo)).

%% Напишите lambda-функцию, которая для каждой точки точки из списка dotsA вычисляет расстояние до всех точек 
%% из списка точек dotsB в пространстве размерности N. Напишите функцию, которая читает следующую нотацию:
%% [
%%   {dimension, 5},
%%   {dotsA, [{1, 2, 3, 4 5}, {7, 8, 9, 10, 11}]},
%%   {dotsB, [{0, 0, 0, 0, 0}, {-1, -2, -3, -4, -5}]}
%% ]
dots([{dimension, N}, {dotsA, DotsAList}, {dotsB, DotsBList}]) ->
  ToLists = fun(Tuples) -> [tuple_to_list(Tuple) || Tuple <- Tuples] end,
  ListA = ToLists(DotsAList),
  ListB = ToLists(DotsBList),

  Sq = fun(X, Y) -> (Y - X) * (Y - X) end,

  Distance = fun Dist(Acc, El, [H|T]) -> 
                  Squares = l_apply(Sq, El, H),
                  SqRoot  = math:sqrt(reduce(fun erlang:'+'/2, 0, Squares)),
                  Dist([SqRoot | Acc], El, T);
                 Dist(Acc, _, []) -> Acc
             end,

  Op = fun Operate(Acc, [Dot|T], List) ->
              case length(Dot) =:= N of
                true ->
                  DotDist = Distance(Acc, Dot, List),
                  Operate(DotDist, T, List);
                _ -> Operate(Acc, T, List)
              end;
            Operate(Acc, [], _) -> lists:reverse(Acc)
       end,
  Op([], ListA, ListB).

reduce(_, Value, []) -> Value;
reduce(Fun, Init, [H|T]) ->
  reduce(Fun,Fun(Init, H),T).
