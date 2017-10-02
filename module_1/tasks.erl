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


