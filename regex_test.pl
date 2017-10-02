:- use_module(library(plunit)).

:- set_prolog_flag('double_quotes', 'chars').

:- begin_tests(regx).

single_char_regx(S, R) :- regx(S, R), length(R, 1).
multi_input_regx([S|Ss], R) :- regx(S, R), multi_input_regx(Ss, R).
multi_input_regx([], R).

test(digit) :-
    findall(R, single("1", R), Rs),
        Rs == [
            ['1'],
            ['\\d'],
            ['\\w']
        ].

test(escaped) :-
    findall(R, single_char_regx(".", R), Rs),
        Rs == [
            ['\\.']
        ].

test(lower) :-
    findall(R, single_char_regx("a", R), Rs),
        Rs == [
            ['a'],
            ['[a-z]'],
            ['[a-zA-Z]'],
            ['\\w']
        ].

test(repetition) :-
    findall(R, multi_input_regx(["a", "aa", "aaa"], R), Rs),
        Rs == [
            ['a',+],
            ['[a-z]',+],
            ['[a-zA-Z]',+],
            ['\\w',+]
        ].

:- end_tests(regx).