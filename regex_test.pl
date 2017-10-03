:- use_module(library(plunit)).
:- consult('regex.pl').


:- begin_tests(regex).

test(digit) :-
    findall(R, regex("1", R), Rs),
        Rs == [
            '1',
            '\\d',
            '\\w',
            '1+',
            '\\d+',
            '\\w+'
        ].

test(escaped) :-
    findall(R, regex(".", R), Rs),
        Rs == [
            '\\.',
            '\\.+'
        ].

test(lower) :-
    findall(R, regex("a", R), Rs),
        Rs == [
            'a',
            '[a-z]',
            '[a-zA-Z]',
            '\\w',
            'a+',
            '[a-z]+',
            '[a-zA-Z]+',
            '\\w+'
        ].

test(repetition) :-
    findall(R, regex_multi(["a", "aa", "aaa"], R), Rs),
        Rs == [
            'a+',
            '[a-z]+',
            '[a-zA-Z]+',
            '\\w+'
        ].

test(negation) :-
    findall(R, regex_multi(["12", "345"], ["6"], R), Rs),
        Rs == [
            '\\d\\d+',
            '\\d\\w+',
            '\\w\\d+',
            '\\w\\w+'
        ].

:- end_tests(regex).