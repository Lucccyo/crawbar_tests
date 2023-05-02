## change 1:
    relop can work with value.

## change 2:
    Fix of:
        (input)             Relop [Bool [false] >= Relop [Bool [false] < Relop [Bool [true] = Relop [Bool [false] >= Bool [false]]]]]
                                !=
        (parse of print)    Relop [Relop [Relop [Relop [Bool [false] >= Bool [false]] < Bool [true]] = Bool [false]] >= Bool [false]]

    relop put parenthesis in right.

## change 3


Relop [Bool [false] != Relop [Bool [false] > Relop [Bool [true] >= Relop [Bool [true] >= Relop [Bool [true] < Relop [Bool [false] = Relop [Relop [Relop [Bool [false] = Bool [true]] >= Bool [false]] != Bool [false]]]]]]]]
    !=
Relop [Bool [false] != Relop [Bool [false] > Relop [Bool [true] >= Relop [Bool [true] >= Relop [Bool [true] < Relop [Bool [false] = Relop [Bool [false] = Relop [Bool [true] >= Relop [Bool [false] != Bool [false]]]]]]]]]]





Relop [Bool [false] != Relop [Bool [false] > Relop [Bool [true] >= Relop [Bool [true] >= Relop [Bool [true] < Relop [Bool [false] = Relop [Relop [Relop [Bool [false] = Bool [true]] >= Bool [false]] != Bool [false]]]]]]]]
    !=
4 [Bool [false] != Relop [Bool [false] > Relop [Bool [true] >= Relop [Bool [true] >= Relop [Bool [true] < Relop [Bool [false] = Relop [Bool [false] = Relop [Bool [true] >= Relop [Bool [false] != Bool [false]]]]]]]]]]













    [ [ true |  [ false |  [ true & true ] ] ] & true ]
 !=
    [ [ true |  false ] |  [ [ true & true ] & true ] ]


Logop [Bool [false] | Logop [Bool [false] & Bool [false]]]
    !=
Logop [Logop [Bool [false] | Bool [false]] & Bool [false]]



Logop [Logop [Bool [true] | Logop [Bool [false] | Logop [Bool [true] & Bool [true]]]] & Bool [true]]
    !=
Logop [Logop [Bool [true] | Bool [false]] | Logop [Logop [Bool [true] & Bool [true]] & Bool [true]]]


Logop [Logop [Bool [true] | Logop [Bool [false] | Logop [Bool [true] & Bool [true]]]] & Bool [true]]
    !=
Logop [Bool [true] | Logop [Bool [false] | Logop [Logop [Bool [true] & Bool [true]] & Bool [true]]]]







[[a | [b | [c & d]]] & e]
    !=
[[a | b] | [[c & d] & e]]  <-
((a | (b | (c & d))) & e)


[[a | [b | [c & d]]] & e]
    !=
[a | [b | [[c & d] & e]]]


[input]                 a | b | c & d & e
[output]                ((a | b) | ((c & d) & e))
[expected]              ((a | (b | (c & d))) & e)





sans parenthèses dans le print des logop
((a | (a | (a & a))) & a) !=
((a | a) | ((a & a) & a))

avec les parenthèses dnas le print logop
 (a | a) != Group [  (a | a)]