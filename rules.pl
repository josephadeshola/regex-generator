:- consult('characters.pl').


% Regular expression rules.


% Recognize backreferences in single-character groups.
regex_tokens(Cs, Acc, Rs, flags(F, [G|Gs], O)) :-
    matches_group(Cs, [G|Gs], N, Os),
    recurse(Os, [back(N)|Acc], Rs, flags(F, [G|Gs], O)).


% Recognize characters as themselves, either escaped or unescaped as necessary.
regex_tokens([C|Cs], Acc, Rs, F) :-
  ( unescaped(C), Class = C
  ; escaped(C, E), Class = E
  ),
  recurse(Cs, [class(Class)|Acc], Rs, F).

% Recognize character classes.
regex_tokens([C|Cs], Acc, Rs, F) :-
  ( digit(C), Class = '\\d'
  ; upper(C),  Class = '[A-Z]'
  ; lower(C), Class = '[a-z]'
  ; letter(C), Class = '[a-zA-Z]'
  ; word(C), Class = '\\w'
  ),
  R = class(Class),
  \+ Acc = [plus(R)|_], % want <>{N,} to be of form <>...<>+ (e.g., \\d\\d\\d+ instead of \\d\\d+\\d for 3+ digits)
  recurse(Cs, [R|Acc], Rs, F).


% Recognize repetition for single characters.
regex_tokens([C|Cs], Acc, Rs, F) :-
    \+ F = flags(plus, _, _),
    regex_tokens([C], [], [R], flags(plus, [], [])),
    \+ Acc = [plus(R)|_],
    recurse(Cs, [plus(R)|Acc], Rs, F).

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
regex_tokens("", Acc, Rs, flags(_, _, O)) :-
    \+ (member(S, O), \+ S = ""), % we've matched all the other strings
    reverse(Acc, Rs). % reverse for correct order


% Split into groups. Don't allow groups of 1 where that character does not appear later in the string (for efficiency).
create_group([T1,T2|Ts], G, O) :-
    append(G, O, [T1,T2|Ts]),
    length(G, Lg), Lg >= 2.

create_group([T1,T2|Ts], [G], O) :-
    append([G], O, [T1,T2|Ts]),
    member(G, O).

% Split into groups where first group matches regex.
create_group(T, R, G, O) :-
    append(G, O, T), length(G, L), L >= 2,
    regex_tokens(G, [], R, flags('', [], [])).


% Find matches.
matches_group(Cs, [G|Gs], N, Os) :-
    append(G, Os, Cs), length(Gs, Gl), N is Gl + 1
  ; \+ append(G, _, Cs), matches_group(Cs, Gs, N, Os).


% Update others
recurse(Cs, [T|Acc], Rs, F) :-
    update_others(T, F, Fn),
    regex_tokens(Cs, [T|Acc], Rs, Fn).

update_others(_, F, F) :-
    atom(F)
  ; F = flags(_, _, []).

update_others(T, flags(F, G, [O|Os]), flags(F, G, Ns)) :-
    update_others(T, [O|Os], Ns, G).

update_others(T, [O|Os], [N|Ns], G) :-
    remaining_after_match(T, O, N, G),
    update_others(T, Os, Ns, G).

update_others(_, [], [], _).

remaining_after_match(T, O, N, G) :-
    split_respecting_arity(T, M, N, O),
    regex_tokens(M, [], [T], flags('', G, [])).

split_respecting_arity(R, [O], Os, [O|Os]) :-
    single_char(R), !.

split_respecting_arity(_, M, N, O) :-
    append(M, N, O).

single_char(R) :- R = class(_) ; R = group([class(_)]).
