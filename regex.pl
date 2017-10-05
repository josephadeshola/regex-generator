:- use_module(library(lists)).
:- consult('characters.pl').

% Strings should be interpreted as a list of character atoms.
:- set_prolog_flag('double_quotes', 'chars').


% procedure regex(+Cs, ?Rs):
%   +Cs - string or list of characters
%   ?Rs - list of elements in a regular expression
% If Rs is bound, checks if the regular expression represented by Rs matches Cs.
% If Rs is not bound, returns (as Rs) all implemented regular expressions that match Cs. No duplicates will be returned.
regex(Cs, R) :- regex_tokens(Cs, [], Rs, flags('', [])), validate(Rs), regex_encode(Rs, R).

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
% This number -> atom conversion means we only support up to 9 groups.  Okay for now.
regex_encode(back(N), B) :- number_chars(N, [C]), atom_concat('$', C, B).


% Don't allow nested groups, or single element groups that aren't backreferenced.
validate(Rs) :- back_count(Rs, [], N), validate(Rs, N).
validate([R|Rs], G) :- \+ functor(R, group, 1), \+ functor(R, plus, 1), validate(Rs, G).
validate([plus(R)|Rs], G) :- \+ contains_group(R), validate(Rs, G).
validate([plus(group(R))|Rs], G) :- length(R, L), L >= 2, group_validate(R), validate(Rs, G).
validate([group(R)|Rs], G) :- length(R, 1), G > 0, group_validate(R), Gn is G - 1, validate(Rs, Gn).
validate([plus(group(R))|Rs], G) :- length(R, 1), G > 0, group_validate(R), Gn is G - 1, validate(Rs, Gn).
validate([], _).

group_validate(R) :-  \+ contains_group(R), validate(R).

contains_group(group(_)).
contains_group([group(_)|_]).
contains_group([plus(R)|_]) :- contains_group(R).
contains_group([_|Rs]) :- contains_group(Rs).

back_count([], _, 0).
back_count([R|Rs], Bs, N) :- \+ functor(R, back, 1), back_count(Rs, Bs, N).
back_count([back(B)|Rs], Bs, N) :- member(B, Bs), back_count(Rs, Bs, N).
back_count([back(B)|Rs], Bs, N) :- \+ member(B, Bs), back_count(Rs, [B|Bs], Bn), N is Bn + 1.



% Regular expression rules.

regex_tokens(Cs, Acc, Rs) :- regex_tokens(Cs, Acc, Rs, '').

% Recognize backreferences in single-character groups.
regex_tokens(Cs, Acc, Rs, flags(F, [G|Gs])) :- matches(Cs, [G|Gs], N, Os), regex_tokens(Os, [back(N)|Acc], Rs, flags(F, [G|Gs])).

% Recognize unescaped characters as themselves.
regex_tokens([C|Cs], Acc, Rs, F) :- unescaped(C), regex_tokens(Cs, [class(C)|Acc], Rs, F).

% Recognize escaped characters as themselves if they should be escaped.
regex_tokens([C|Cs], Acc, Rs, F) :- escaped(C, E), regex_tokens(Cs, [class(E)|Acc], Rs, F).

% Recognize character classes.
regex_tokens([C|Cs], Acc, Rs, F) :- digit(C), regex_tokens(Cs, [class('\\d')|Acc], Rs, F).
regex_tokens([C|Cs], Acc, Rs, F) :- upper(C), regex_tokens(Cs, [class('[A-Z]')|Acc], Rs, F).
regex_tokens([C|Cs], Acc, Rs, F) :- lower(C), regex_tokens(Cs, [class('[a-z]')|Acc], Rs, F).
regex_tokens([C|Cs], Acc, Rs, F) :- letter(C), regex_tokens(Cs, [class('[a-zA-Z]')|Acc], Rs, F).
regex_tokens([C|Cs], Acc, Rs, F) :- word(C), regex_tokens(Cs, [class('\\w')|Acc], Rs, F).

% Recognize repetition.
regex_tokens([C|Cs], Acc, Rs, F) :- \+ F = plus, regex_tokens([C], [], [R], plus), regex_tokens(Cs, [plus(R)|Acc], Rs), \+ Acc = [R|_], \+ Acc = [plus(R)|_].
regex_tokens([C|Cs], [plus(R)|Acc], Rs, F) :- \+ F = plus, regex_tokens([C], [], [R], plus), regex_tokens(Cs, [plus(R)|Acc], Rs).

% Allow groups.
regex_tokens(Cs, Acc, Rs, flags(F, G)) :- \+ F = group, create_group(Cs, Gs, Os), regex_tokens(Gs, [], R, group), regex_tokens(Os, [group(R)|Acc], Rs, flags(F, [Gs|G])).

% Allow group repetition.
regex_tokens(Cs, Acc, Rs, F) :- \+ F = group, create_group(Cs, Gs, Os), regex_tokens(Gs, [], R, group), regex_tokens(Os, [plus(group(R))|Acc], Rs).
regex_tokens(Cs,[plus(group(R))|Acc], Rs, F) :- \+ F = group, create_group(Cs, R, _, Os), regex_tokens(Os, [plus(group(R))|Acc], Rs).

% Base case.
regex_tokens("", Acc, Rs, _) :- reverse(Acc, Rs).

% Split into groups. Don't allow groups of 1 where that character does not appear later in the string (for efficiency).
create_group([T1,T2|Ts], G, O) :- append(G, O, [T1,T2|Ts]), length(G, Lg), Lg >= 2.
create_group([T1,T2|Ts], [G], O) :- append([G], O, [T1,T2|Ts]), member(G, O).
% Split into groups where first group matches regex.
create_group(T, R, G, O) :- append(G, O, T), length(G, L), L >= 2, regex_tokens(G, [], R).

% Find matches.
matches(Cs, [G|Gs], N, Os) :- append(G, Os, Cs), length(Gs, Gl), N is Gl + 1.
matches(Cs, [G|Gs], N, Os) :- \+ append(G, _, Cs), matches(Cs, Gs, N, Os).
