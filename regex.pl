:- consult('characters.pl').

% Strings should be interpreted as a list of character atoms.
:- set_prolog_flag('double_quotes', 'chars').


% procedure regex(+Cs, ?Rs):
%   +Cs - string or list of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches Cs.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match Cs. No duplicates will be returned.
regex(Cs, R) :- regex_tokens([], Cs, Rs), regex_encode(Rs, R).

% procedure regex_multi(+Ss, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches all strings in Ss.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match all strings in Ss. No duplicates
%   will be returned.
% regex_multi([S1, S2, ..., Sn], Rs) is equivalent to regex(S1, Rs), regex(S2, Rs), ..., regex(Sn, Rs).
regex_multi([S], R) :- regex(S, R), !.
regex_multi([S|Ss], R) :- regex(S, R), regex_multi(Ss, R).

% procedure regex_multi(+Ss, +Xs, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   +Xs - list of strings or list of lists of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches all strings in Ss.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match all strings in Ss, and do not match
%   and strings in Xs. No duplicates will be returned.
% regex_multi([S1, S2, ..., Sn], [X1, X2, ..., Xn], Rs) is equivalent to regex(S1, Rs), regex(S2, Rs), ...,
%   regex(Sn, Rs), \+ regex(X1, Rs), \+ regex(X2, Rs), ..., \+ regex(Xn, Rs).
regex_multi([S], Xs, R) :- regex(S, R), regex_multi([], Xs, R), !.
regex_multi([S|Ss], Xs, R) :- regex(S, R), regex_multi(Ss, Xs, R).
regex_multi([], [X], R) :- \+ regex(X, R), !.
regex_multi([], [X|Xs], R) :- \+ regex(X, R), regex_multi([], Xs, R).


% Convert regular expression tokens to regular expression strings.
regex_encode([R|Rs], S) :- regex_encode(R, Sr), regex_encode(Rs, Ss), atom_concat(Sr, Ss, S).
regex_encode([], '').
regex_encode(class(C), C).
regex_encode(plus(E), P) :- regex_encode(E, C), atom_concat(C, +, P).


% Regular expression rules.

% Recognize unescaped characters as themselves.
regex_tokens(Cs, [C|Ps], [class(C)|Rs]) :- unescaped(C), regex_tokens(Cs, Ps, Rs).

% Recognize escaped characters as themselves if they should be escaped.
regex_tokens(Cs, [C|Ps], [class(E)|Rs]) :- escaped(C, E), regex_tokens(Cs, Ps, Rs).

% Recognize character classes.
regex_tokens(Cs, [C|Ps], [class('\\d')|Rs]) :- digit(C), regex_tokens(Cs, Ps, Rs).
regex_tokens(Cs, [C|Ps], [class('[A-Z]')|Rs]) :- upper(C), regex_tokens(Cs, Ps, Rs).
regex_tokens(Cs, [C|Ps], [class('[a-z]')|Rs]) :- lower(C), regex_tokens(Cs, Ps, Rs).
regex_tokens(Cs, [C|Ps], [class('[a-zA-Z]')|Rs]) :- letter(C), regex_tokens(Cs, Ps, Rs).
regex_tokens(Cs, [C|Ps], [class('\\w')|Rs]) :- word(C), regex_tokens(Cs, Ps, Rs).

% Recognize repetition.
regex_tokens(Cs, [C|Ps], [plus(R)|Rs]) :- iterable(R), regex_tokens([], [C], [R]), regex_tokens(Cs, Ps, [plus(R)|Rs]).
regex_tokens(Cs, [C|Ps], [plus(R)|Rs]) :- iterable(R), regex_tokens([], [C], [R]), regex_tokens(Cs, Ps, Rs).

% Base case.
regex_tokens([], [], []).

% Allow iteration only on certain types of regex tokens.
iterable(class(_)).
