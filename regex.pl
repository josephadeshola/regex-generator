:- consult('characters.pl').

% Strings should be interpreted as a list of character atoms.
:- set_prolog_flag('double_quotes', 'chars').


% procedure regex(+Cs, ?Rs):
%   +Cs - string or list of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches Cs.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match Cs. No duplicates will be returned.
regex(Cs, [R|Rs]) :- regex([], Cs, [R|Rs]).
regex([], [], []).

% procedure regex_multi(+Ss, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches all strings in Ss.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match all strings in Ss. No duplicates
%   will be returned.
% regex_multi([S1, S2, ..., Sn], Rs) is equivalent to regex(S1, Rs), regex(S2, Rs), ..., regex(Sn, Rs).
regex_multi([S|Ss], R) :- regex(S, R), regex_multi(Ss, R).
regex_multi([], R).

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
regex_multi([], [X|Xs], R) :- \+regex(X, R), regex_multi([], Xs, R).
regex_multi([], [], R).


% Regular expression rules.

% Recognize unescaped characters as themselves.
regex(Cs, [C|Ps], [C|Rs]) :- unescaped(C), regex(Cs, Ps, Rs).

% Recognize escaped characters as themselves if they should be escaped.
regex(Cs, [C|Ps], [E|Rs]) :- escaped(C, E), regex(Cs, Ps, Rs).

% Recognize character classes.
regex(Cs, [C|Ps], ['\\d'|Rs]) :- digit(C), regex(Cs, Ps, Rs).
regex(Cs, [C|Ps], ['[A-Z]'|Rs]) :- upper(C), regex(Cs, Ps, Rs).
regex(Cs, [C|Ps], ['[a-z]'|Rs]) :- lower(C), regex(Cs, Ps, Rs).
regex(Cs, [C|Ps], ['[a-zA-Z]'|Rs]) :- letter(C), regex(Cs, Ps, Rs).
regex(Cs, [C|Ps], ['\\w'|Rs]) :- word(C), regex(Cs, Ps, Rs).

% Recognize repetition.
regex([], [C], [R,+]) :- regex([], [C], [R]).
regex(Cs, [C|Ps], [R,+|Rs]) :- regex([], [C], [R]), regex(Cs, Ps, [R,+|Rs]).
