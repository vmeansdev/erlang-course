%% - task 1.7
-module(mlist).
-export([create/1,      reverse_create/1, print/1,
         print_odd/1,   filter/2,         reverse/1,
         concatenate/1, flatten/1,        dna_to_rna/1,
         cut_rdna/2]).


%% - Напишите функцию create/1, которая на вход принимает число N
%% - и возвращает список вида [1, 2,..., N -1, N].
create(0) -> [];
create(N) when N > 0 ->
  create(N - 1) ++ [N].

%% - Напишите функцию, которая также принимает число N, но возвращает список вида [N, N-1, ..., 2,1].
reverse_create(0) -> create(0);
reverse_create(N) when N > 0 ->
  [N | create(N - 1)].

%% - Напишите функцию, которая распечатывает все числа от 1 до N.
print(To, From) when To > From ->
  io:format("~p~n", [From]),
  Iter = From + 1,
  print(To, Iter);
print(To,From) when To == From ->
  io:format("~p~n", [From]).

print(N) when N < 0 -> false;
print(0) -> 0;
print(N) ->
  print(N, 1).

%% - Напишите функцию, которая распечатывает все нечётные числа от 1 до N.
print_odd(To,From) when To > From, From rem 2 == 0 ->
  print_odd(To, From + 1);
print_odd(To,From) when To == From, From rem 2 == 0 ->
  ok;

print_odd(To, From) when To > From, From rem 2 =/= 0 ->
  io:format("~p~n", [From]),
  Iter = From + 1,
  print_odd(To, Iter);
print_odd(To, From) when To == From, From rem 2 =/= 0 ->
  io:format("~p~n", [From]).

print_odd(N) when N < 0 -> false;
print_odd(0) -> 0;
print_odd(N) ->
  print_odd(N, 1).

%% - Напишите функцию, которая принимает на вход список целых чисел и одно целое число,
%% - а возвращает список всех элементов списка, которые меньше либо равны числу, переданному вторым аргументом. Пример: filter([1, 2, 3, 4, 5], 3) => [1,2,3].
filter(Acc, [H|T], Num) when H =< Num ->
  filter(Acc ++ [H], T, Num);
filter(Acc, [H|T], Num) when H >= Num ->
  filter(Acc, T, Num);
filter(Acc, [], _Num) -> Acc.

filter(List, Num) ->
  filter([], List, Num).

%% - Напишите функцию, которая переворачивает список. Пример: reverse([1,2,3]) => [3,2,1].
reverse(Acc, [H | T]) -> reverse([H | Acc], T);
reverse(Acc, []) -> Acc.
reverse([]) -> [];
reverse(List) ->
  reverse([], List).

%% - Напишите функцию, которая преобразует список списков в один список,
%% - соединяя все списки-элементы. Пример: concatenate([[1,2,3], [], [4, five]]) => [1,2,3,4,five].
concatenate(Acc, []) -> Acc;
concatenate(Acc, [H | T]) when is_list(H) ->
  concatenate(Acc, H ++ T);
concatenate(Acc, [H | T]) ->
  concatenate(Acc ++ [H], T).

concatenate(List) ->
  concatenate([], List).

%% - Напишите функцию, которая по списку вложенных списков
%% - строит линейный список. Пример: flatten([[1, [2 , [3] , [] ], [[[4]]], [5,6]]) => [1,2,3,4,5,6].
flatten(List) -> concatenate(List).

%% - Напишите функцию, которая принимает на вход последовательность нуклеотидов ДНК и выдает
%% - комплементарную ей цепочку нуклеотидов РНК. Траскрипция происходит путем замены нуклеотидов
%% - в исходной цепочке ДНК на парные им нуклеотиды, входящие в цепочку РНК. ДНК содержит четыре нуклеотида:
%% - A - аденин, С - цитозин, G - гуанин, T - тимин. В РНК содержатся следующие нуклеотиды:
%% - A - аденин, С - цитозин, G - гуанин, U - урацил.
dna_to_rna(Acc, []) -> Acc;
dna_to_rna(Acc, [H|T]) when H == $G; H == $g; H == g ->
  dna_to_rna(Acc ++ [$c], T);
dna_to_rna(Acc, [H|T]) when H == $C; H == $c; H == c ->
  dna_to_rna(Acc ++ [$g], T);
dna_to_rna(Acc, [H|T]) when H == $T; H == $t; H == t ->
  dna_to_rna(Acc ++ [$a], T);
dna_to_rna(Acc, [H|T]) when H == $A; H == $a; H == a ->
  dna_to_rna(Acc ++ [$u], T).

dna_to_rna([]) -> [];

dna_to_rna(List) when is_list(List) ->
  dna_to_rna([], List).

%% - Напишите функцию, которая из заданной цепочки РНК/ДНК вырезает
%% - заданную последовательность из трех нуклеотидов.
%% - Пример: cut_rdna("AAGGTT", "AGG") => "ATT".
cut_rdna([], _) -> [];
cut_rdna(List, []) -> List;
cut_rdna(List, Seq) when length(Seq) == 3 -> 
  List -- Seq.