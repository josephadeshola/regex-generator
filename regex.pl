:- use_module(library(lists)).
:- consult('characters.pl').

% Strings should be interpreted as a list of character atoms.
:- set_prolog_flag('double_quotes', 'chars').


% procedure regex(+Cs, ?Rs):
%   +Cs - string or list of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches Cs.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match Cs. No duplicates will be returned.
regex(Cs, R) :- regex_tokens(Cs, [], Rs), validate(Rs), regex_encode(Rs, R).

% procedure regex_multi(+Ss, ?Rs):
%   +Ss - list of strings or list of lists of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches all strings in Ss.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match all strings in Ss. No duplicates
%   will be returned.
% regex_multi([S1, S2, ..., Sn], Rs) is equivalent to regex(S1, Rs), regex(S2, Rs), ..., regex(Sn, Rs).
regex_multi([S,S2|Ss], R) :- regex(S, R), regex_multi([S2|Ss], R).
regex_multi([S], R) :- regex(S, R).

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


% Convert regular expression tokens to regular expression strings.
regex_encode([R|Rs], S) :- regex_encode(R, Sr), regex_encode(Rs, Ss), atom_concat(Sr, Ss, S).
regex_encode([], '').
regex_encode(class(C), C).
regex_encode(plus(E), P) :- regex_encode(E, C), atom_concat(C, +, P).
regex_encode(group(E), G) :- regex_encode(E, R), atom_concat('(', R, O), atom_concat(O, ')', G).
regex_encode(back(N), B) :- atom_concat('$', N, B).


% Don't allow groups on their own.
validate([R|Rs]) :- \+ functor(R, group, 1), \+ functor(R, plus, 1), validate(Rs).
validate([plus(R)|Rs]) :- \+ functor(R, group, 1), validate(Rs).
validate([plus(group(R))|Rs]) :- length(R, L), L >= 2, validate(R), validate(Rs).
validate([]).


% Regular expression rules.

regex_tokens(Cs, Acc, Rs) :- regex_tokens(Cs, Acc, Rs, '').

% Recognize unescaped characters as themselves.
regex_tokens([C|Cs], Acc, Rs, _) :- unescaped(C), regex_tokens(Cs, [class(C)|Acc], Rs).

% Recognize escaped characters as themselves if they should be escaped.
regex_tokens([C|Cs], Acc, Rs, _) :- escaped(C, E), regex_tokens(Cs, [class(E)|Acc], Rs).

% Recognize character classes.
regex_tokens([C|Cs], Acc, Rs, _) :- digit(C), regex_tokens(Cs, [class('\\d')|Acc], Rs).
regex_tokens([C|Cs], Acc, Rs, _) :- upper(C), regex_tokens(Cs, [class('[A-Z]')|Acc], Rs).
regex_tokens([C|Cs], Acc, Rs, _) :- lower(C), regex_tokens(Cs, [class('[a-z]')|Acc], Rs).
regex_tokens([C|Cs], Acc, Rs, _) :- letter(C), regex_tokens(Cs, [class('[a-zA-Z]')|Acc], Rs).
regex_tokens([C|Cs], Acc, Rs, _) :- word(C), regex_tokens(Cs, [class('\\w')|Acc], Rs).

% Recognize repetition.
regex_tokens([C|Cs], Acc, Rs, F) :- \+ F = plus, regex_tokens([C], [], [R], plus), regex_tokens(Cs, [plus(R)|Acc], Rs), \+ Acc = [R|_], \+ Acc = [plus(R)|_].
regex_tokens([C|Cs], [plus(R)|Acc], Rs, F) :- \+ F = plus, regex_tokens([C], [], [R], plus), regex_tokens(Cs, [plus(R)|Acc], Rs).

% Allow groups.
regex_tokens(Cs, Acc, Rs, F) :- \+ F = group, create_group(Cs, Gs, Os), regex_tokens(Gs, [], R, group), regex_tokens(Os, [group(R)|Acc], Rs).

% Allow group repetition.
regex_tokens(Cs, Acc, Rs, F) :- \+ F = group, create_group(Cs, Gs, Os), regex_tokens(Gs, [], R, group), regex_tokens(Os, [plus(group(R))|Acc], Rs).
regex_tokens(Cs,[plus(group(R))|Acc], Rs, F) :- \+ F = group, create_group(Cs, R, _, Os), regex_tokens(Os, [plus(group(R))|Acc], Rs).

% Base case.
regex_tokens("", Acc, Rs, _) :- reverse(Acc, Rs).

% Split into groups.
create_group([T1,T2|Ts], G, O) :- append(G, O, [T1,T2|Ts]), length(G, Lg), Lg >= 1.
create_group(T, R, G, O) :- append(G, O, T), length(G, L), L >= 2, regex_tokens(G, [], R).
