:- use_module(library(lists)).
:- consult('rules.pl').
:- consult('validate.pl').
:- consult('encode.pl').

% Strings should be interpreted as a list of character atoms.
:- set_prolog_flag('double_quotes', 'chars').


% procedure regex(+Cs, ?Rs):
%   +Cs - string or list of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches Cs.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match Cs. No duplicates will be returned.
regex(Cs, R) :- regex_multi([Cs], R).

% procedure regex_multi(+Ss, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches all strings in Ss.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match all strings in Ss. No duplicates
%   will be returned.
% regex_multi([S1, S2, ..., Sn], Rs) is equivalent to regex(S1, Rs), regex(S2, Rs), ..., regex(Sn, Rs).
%regex_multi([S,S2|Ss], R) :- regex(S, R), regex_multi([S2|Ss], R).
%regex_multi([S], R) :- regex(S, R).
regex_multi([S|Ss], R) :- regex_tokens(S, [], Rs, flags('', [], Ss)), validate(Rs), regex_encode(Rs, R).

% procedure regex_multi(+Ss, +Xs, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   +Xs - list of strings or list of lists of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches all strings in Ss.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match all strings in Ss, and do not match
%   and strings in Xs. No duplicates will be returned.
% regex_multi([S1, S2, ..., Sn], [X1, X2, ..., Xn], Rs) is equivalent to regex(S1, Rs), regex(S2, Rs), ...,
%   regex(Sn, Rs), \+ regex(X1, Rs), \+ regex(X2, Rs), ..., \+ regex(Xn, Rs).
regex_multi([S|Ss], Xs, R) :- regex(S, R), regex_multi(Ss, Xs, R).
regex_multi([], [X], R) :- \+ regex(X, R).
regex_multi([], [X,X2|Xs], R) :- \+ regex(X, R), regex_multi([], [X2|Xs], R).
