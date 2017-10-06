:- consult('characters.pl').


% Regular expression rules.

% Recognize backreferences in single-character groups.
regex_tokens(Cs, Acc, Rs, flags(F, [G|Gs], O)) :-
    matches_group(Cs, [G|Gs], N, Os),
    recurse(Os, [back(N)|Acc], Rs, flags(F, [G|Gs], O)).

% Recognize unescaped characters as themselves.
regex_tokens([C|Cs], Acc, Rs, F) :-
    unescaped(C),
    recurse(Cs, [class(C)|Acc], Rs, F).

% Recognize escaped characters as themselves if they should be escaped.
regex_tokens([C|Cs], Acc, Rs, F) :- escaped(C, E), regex_tokens(Cs, [class(E)|Acc], Rs, F).

% Recognize character classes.
regex_tokens([C|Cs], Acc, Rs, F) :- digit(C), recurse(Cs, [class('\\d')|Acc], Rs, F).
regex_tokens([C|Cs], Acc, Rs, F) :- upper(C), recurse(Cs, [class('[A-Z]')|Acc], Rs, F).
regex_tokens([C|Cs], Acc, Rs, F) :- lower(C), recurse(Cs, [class('[a-z]')|Acc], Rs, F).
regex_tokens([C|Cs], Acc, Rs, F) :- letter(C), recurse(Cs, [class('[a-zA-Z]')|Acc], Rs, F).
regex_tokens([C|Cs], Acc, Rs, F) :- word(C), recurse(Cs, [class('\\w')|Acc], Rs, F).

% Recognize repetition.
regex_tokens([C|Cs], Acc, Rs, F) :-
    \+ F = flags(plus, _, _),
    regex_tokens([C], [], [R], flags(plus, [], [])),
    recurse(Cs, [plus(R)|Acc], Rs, F),
    \+ Acc = [R|_], \+ Acc = [plus(R)|_].
regex_tokens([C|Cs], [plus(R)|Acc], Rs, F) :-
    \+ F = flags(plus, _, _),
    regex_tokens([C], [], [R], flags(plus, [], [])),
    recurse(Cs, [plus(R)|Acc], Rs, F).

% Allow groups.
regex_tokens(Cs, Acc, Rs, flags(F, G, O)) :-
    \+ F = group,
    create_group(Cs, Gs, Os),
    regex_tokens(Gs, [], R, flags(group, [], [])),
    recurse(Os, [group(R)|Acc], Rs, flags(F, [Gs|G], O)).

% Allow group repetition.
regex_tokens(Cs, Acc, Rs, F) :-
    \+ F = flags(group, _, _),
    create_group(Cs, Gs, Os),
    regex_tokens(Gs, [], R, flags(group, [], [])),
    \+ R = [plus(_)], % don't allow stuff of form (<>+)+
    recurse(Os, [plus(group(R))|Acc], Rs, F).
regex_tokens(Cs, [plus(group(R))|Acc], Rs, F) :-
    \+ F = flags(group, _, _),
    create_group(Cs, R, _, Os),
    recurse(Os, [plus(group(R))|Acc], Rs, F).

% Base case.
regex_tokens("", Acc, Rs, flags(_, _, [])) :- reverse(Acc, Rs).
regex_tokens("", Acc, Rs, flags(_, _, [O|Os])) :- empty_strings([O|Os]), reverse(Acc, Rs).
empty_strings([""]).
empty_strings([""|Ss]) :- empty_strings(Ss).

% Split into groups. Don't allow groups of 1 where that character does not appear later in the string (for efficiency).
create_group([T1,T2|Ts], G, O) :- append(G, O, [T1,T2|Ts]), length(G, Lg), Lg >= 2.
create_group([T1,T2|Ts], [G], O) :- append([G], O, [T1,T2|Ts]), member(G, O).
% Split into groups where first group matches regex.
create_group(T, R, G, O) :- append(G, O, T), length(G, L), L >= 2, regex_tokens(G, [], R, flags('', [], [])).

% Find matches.
matches_group(Cs, [G|Gs], N, Os) :- append(G, Os, Cs), length(Gs, Gl), N is Gl + 1.
matches_group(Cs, [G|Gs], N, Os) :- \+ append(G, _, Cs), matches_group(Cs, Gs, N, Os).

% Update others
recurse(Cs, [T|Acc], Rs, F) :- update_others(T, F, Fn), regex_tokens(Cs, [T|Acc], Rs, Fn).

update_others(_, flags(F, G, []), flags(F, G, [])).
update_others(_, F, F) :- atom(F).
update_others(T, flags(F, G, [O|Os]), flags(F, G, Ns)) :- update_others(T, [O|Os], Ns, G).
update_others(T, [O|Os], [N|Ns], G) :- remaining_after_match(T, O, N, G), update_others(T, Os, Ns, G).
update_others(_, [], [], _).
remaining_after_match(T, O, N, G) :- append(M, N, O), regex_tokens(M, [], [T], flags('', G, [])).

