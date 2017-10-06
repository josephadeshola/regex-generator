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

test(repetition_only) :-
    findall(R, regex_multi(["1", "11", "111"], R), Rs),
        Rs == [
            '1+',
            '\\d+'
        ].

test(repetition_suffix) :-
    findall(R, regex_multi(["1a", "22B", "333C"], ["1a1a"], R), Rs),
        Rs == [
            '\\d+[a-zA-Z]',
            '\\d+\\w',
            '\\d+[a-zA-Z]+',
            '\\d+\\w+'
        ].

test(negation) :-
    findall(R, regex_multi(["12", "345"], ["6"], R), Rs),
        Rs == [
            '\\d\\d+',
            '(\\d\\d+)+'
        ].

test(group_repetition) :-
    findall(R, regex_multi(["12", "1212"], ["13", "3232"], R), Rs),
        Rs == [
            '(12)+',
            '(12+)+',
            '(1+2)+',
            '(1+2+)+'
        ].

test(back_references, nondet) :-
    regex("12321", '(\\d)(\\d)\\d$2$1').

% This test is slow.  Uncomment at your own risk.
%test(back_reference) :-
%    findall(R, regex_multi(["121", "33433"], ["112", "1121"], R), Rs),
%        Rs == [
%            '(\\d+)\\d$1'
%        ].

:- end_tests(regex).