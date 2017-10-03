%% Exercises:
%% 1.1 
B = 1. %% - связывает B со значением 1
1 = B. %% - сопоставляет значение связанное с B и 1
1 = C. %% - сопоставляет значение связанное с С и 1, но выводит ошибку, т.к. C 'unbound' (ни с чем не связана)
C = 1. %% - связывает C со значением 1
C = B. %% - сопоставляет значение в С со значением в В
A = F = D = B. %% - сначала связывает unbound переменные F и D со значением в B, затем сопоставляет все значения с В

%% 1.2
Path = "/bar/foo",
Bar ++ "/foo" = Path, %% - не может быть вычислено, т.к. слева не матчится
"/bar/" ++ Foo = Path. %% - справа - матчится

Person = #{name => "Mike", surname => "Williams", phone  => [1,2,3,4]}.
#{
    name := Name,
    surname := Name,
    phone := Phone
} = Person.
%% - ошибка матчинга, 2 раза используем Name. сыпется на матче surname := Name, т.к "Mike" =/= "Williams"

%% 1.3
atom1 > atom2. %% - false
atom10 < atom2. %% - false (кол-во символов в atom10 больше чем в atom2, значит он больше)
#{a => 0} < #{a => 1}. %% - true
#{a => 1} < #{a => 0, b => 0}. %% - true (больше значений)
{a, 1, 0} > {a, 0, 0}. %% - true

%% 1.4
  %% - Зарезервированные создателями языка: after and andalso band begin bnot bor 
  %% bsl bsr bxor case catch cond div end fun if let not of or orelse query receive rem try when xor
  %% - строгое равенство: 1 =:= 1 (true) и 1 =:= 1.0 (false), нестрогое 1 == 1 (true), 1 =:= 1.0 (true)
  %% - строгое проверяет значение и тип, нестрогое - только значение
  %% - f(X). - забыть значение X, f(). - забыть все связывания в интерпретаторе


%% 1.5
[{X, Y} || X <- [1,2,3], Y <- [4,5,6]]. %% - декартово произведение

List = [[1,2,3], [4,5,6], [7,8,9]].

[Elem || SubList <- List, Elem <- SubList]. %% - плоский список

Dicts = [
    #{
        tags => [awesome, erlang],
    },
    #{
        tags => [simple_tag]
    },
    #{
        tags => [just_atom, 'I am ok']
    }
],

[Value || Dict <- Dicts, Value <- maps:get(tags, Dict)]. %% - [awesome, erlang, simple_tag, justom_atom, 'I am ok'].

MixedList = [
    john,
    doe,
    {age, 19},
    {height, 182},
    {weight, 72},
    london,
    britain
],

TuplesList = [Tuple || Tuple <- MixedList, is_tuple(Tuple)], %% - [{age, 19}, {height, 182},{weight, 72}]

AtomsList  = [Atom  || Atom  <- MixedList, is_atom(Atom)]. %% - [john, doe, london, britain]

Shapes = [
    {{0, 0}, {10, 10}},
    {{0, 1}, {2, 30}},
    {{30, 31}, {40, 41}},
    {{32, 56}, {5, 9}}
],

N = 100,

%% - Написать LC, результатом которого будет список прямоугольников с площадью меньше N
LessThanNSquares = [{{X1, Y1},{X2, Y2}} || {{X1,Y1},{X2,Y2}} <- Shapes, ((X2 - X1) * (Y2 - Y1)) < N].

%% - Написать binary comprehension, который сериализует список прямоугольникаов в бинарное представление.
BinShapes = << <<X1:8, Y1:8, X2:8, Y2:8>> || {{X1, Y1},{X2, Y2}} <- Shapes>>.

%% - Напишите list comprehension, распакует бинарную строку в список, эквивалентный списку Shapes.
[{{X1,Y1},{X2,Y2}} || <<X1:8, Y1:8, X2:8, Y2:8>> <= BinShapes ].