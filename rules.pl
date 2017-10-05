:- consult('characters.pl').


% Regular expression rules.

% Recognize backreferences in single-character groups.
regex_tokens(Cs, Acc, Rs, flags(F, [G|Gs])) :-
    matches(Cs, [G|Gs], N, Os),
    regex_tokens(Os, [back(N)|Acc], Rs, flags(F, [G|Gs])).

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
regex_tokens([C|Cs], Acc, Rs, F) :-
    \+ F = plus,
    regex_tokens([C], [], [R], plus),
    regex_tokens(Cs, [plus(R)|Acc], Rs, F), \+ Acc = [R|_], \+ Acc = [plus(R)|_].
regex_tokens([C|Cs], [plus(R)|Acc], Rs, F) :-
    \+ F = plus,
    regex_tokens([C], [], [R], plus),
    regex_tokens(Cs, [plus(R)|Acc], Rs, F).

% Allow groups.
regex_tokens(Cs, Acc, Rs, flags(F, G)) :-
    \+ F = group,
    create_group(Cs, Gs, Os),
    regex_tokens(Gs, [], R, group),
    regex_tokens(Os, [group(R)|Acc], Rs, flags(F, [Gs|G])).

% Allow group repetition.
regex_tokens(Cs, Acc, Rs, F) :-
    \+ F = group,
    create_group(Cs, Gs, Os),
    regex_tokens(Gs, [], R, group),
    regex_tokens(Os, [plus(group(R))|Acc], Rs, F).
regex_tokens(Cs,[plus(group(R))|Acc], Rs, F) :-
    \+ F = group,
    create_group(Cs, R, _, Os),
    regex_tokens(Os, [plus(group(R))|Acc], Rs, F).

% Base case.
regex_tokens("", Acc, Rs, _) :- reverse(Acc, Rs).

% Split into groups. Don't allow groups of 1 where that character does not appear later in the string (for efficiency).
create_group([T1,T2|Ts], G, O) :- append(G, O, [T1,T2|Ts]), length(G, Lg), Lg >= 2.
create_group([T1,T2|Ts], [G], O) :- append([G], O, [T1,T2|Ts]), member(G, O).
% Split into groups where first group matches regex.
create_group(T, R, G, O) :- append(G, O, T), length(G, L), L >= 2, regex_tokens(G, [], R, flags('', [])).

% Find matches.
matches(Cs, [G|Gs], N, Os) :- append(G, Os, Cs), length(Gs, Gl), N is Gl + 1.
matches(Cs, [G|Gs], N, Os) :- \+ append(G, _, Cs), matches(Cs, Gs, N, Os).
