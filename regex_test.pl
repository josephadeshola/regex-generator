:- use_module(library(plunit)).

:- begin_tests(regx).

:- set_prolog_flag('double_quotes', 'chars').

test(digit) :-
    findall(R, regx("1", R), Rs),
        Rs == [
            ['1'],
            [\d]
        ].

test(escaped) :-
    findall(R, regx(['.'], R), Rs),
        Rs == [
            ['\\.']
        ].

:- end_tests(regx).