:- use_module(library(lists)).
:- consult('rules.pl').
:- consult('validate.pl').
:- consult('encode.pl').

% Strings should be interpreted as a list of character atoms.
:- set_prolog_flag('double_quotes', 'chars').


% procedure regex(+Cs, ?Rs):
%   +Cs - string or list of characters
%   ?Rs - list of elements in a regular expression
regex(Cs, R) :-
    regex_multi([Cs], R).

% procedure regex_multi(+Ss, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches all strings in Ss.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match all strings in Ss. No duplicates
%   will be returned.
regex_multi([S|Ss], R) :-
    regex_tokens(S, [], Rs, flags('', [], Ss)),
    validate(Rs),
    regex_encode(Rs, R).

% procedure regex_multi(+Ss, +Xs, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   +Xs - list of strings or list of lists of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches all strings in Ss.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match all strings in Ss, and do not match
%   and strings in Xs. No duplicates will be returned.
regex_multi([S|Ss], Xs, R) :-
    regex_tokens(S, [], Rs, flags('', [], Ss)),
    validate(Rs),
    \+ (member(E, Xs), forward_match(E, Rs, [])),
    regex_encode(Rs, R).
