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
            '\\d+\\d',
            '(\\d+\\d)+'
        ].

test(group_repetition) :-
    findall(R, regex_multi(["12", "1212"], ["13", "3232"], R), Rs),
        Rs == [
            '(12)+',
            '(12+)+',
            '(1+2)+',
            '(1+2+)+'
        ].

test(back_reference) :-
    findall(R, regex_multi(["121", "343"], ["112", "344"], R), Rs),
        Rs == [
            '(\\d)\\d$1',
            '(\\d+)\\d$1'
        ].

:- end_tests(regex).