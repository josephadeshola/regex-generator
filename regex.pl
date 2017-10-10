:- use_module(library(lists)).
:- compile('rules.pl').
:- compile('validate.pl').
:- compile('encode.pl').

% Strings should be interpreted as a list of character atoms.
:- set_prolog_flag('double_quotes', 'chars').


% procedure regex(+Cs, ?Rs):
%   +Cs - string or list of characters
%   ?R - atom  containing a regular expression
regex(Cs, R) :-
    regex_multi([Cs], R).

% procedure regex_multi(+Ss, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   ?R - atom  containing a regular expression
% If R is bound, checks if the regular expression represented by R matches all strings in Ss.
% If R is not bound, returns (as R) an implemented regular expressions that matches all strings in Ss. No duplicates
%   will be returned.
regex_multi([S|Ss], R) :-
    regex_tokens(S, [], Rs, flags('', [], Ss)),
    validate(Rs),
    regex_encode(Rs, R).

% procedure regex_multi(+Ss, +Xs, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   +Xs - list of strings or list of lists of characters
%   ?R - atom  containing a regular expression
% If R is bound, checks if the regular expression represented by R matches all strings in Ss and none in Xs.
% If Rs is not bound, returns (as R) an implemented regular expressions that matches all strings in Ss, and does not
%   match and of the strings in Xs. No duplicates will be returned.
regex_multi([S|Ss], Xs, R) :-
    regex_tokens(S, [], Rs, flags('', [], Ss)),
    validate(Rs),
    \+ (member(E, Xs), forward_match(E, Rs, [])),
    regex_encode(Rs, R).
