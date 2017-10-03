:- use_module(library(plunit)).
:- consult('regex.pl').

% Removing this rule to more easily test repetition etc.
:- retract((word(C) :- digit(C))).

:- begin_tests(regex).

test(digit) :-
    findall(R, regex("1", R), Rs),
        Rs == [
            '1',
            '\\d',
            '1+',
            '\\d+'
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
    findall(R, regex_multi(["1", "11", "111"], R), Rs),
        Rs == [
            '1+',
            '\\d+'
        ].

test(negation) :-
    findall(R, regex_multi(["12", "345"], ["6"], R), Rs),
        Rs == [
            '\\d\\d+',
            '\\d+\\d',
            '\\d+\\d+'
        ].

:- end_tests(regex).