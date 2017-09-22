:- use_module(library(plunit)).

:- begin_tests(regx).

test(digit) :-
    findall(R, regx([1], R), Rs),
        Rs == [
            [1],
            [\d]
        ].

:- end_tests(regx).