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


% Update other strings to remove part matching T
recurse(_, Acc, _, F) :-
    fails_to_match_others(Acc, F),
    !.

recurse(Cs, Acc, Rs, F) :-
    regex_tokens(Cs, Acc, Rs, F).

fails_to_match_others(Acc, flag(_, G, O)) :-
    member(E, O),
    \+ (append(M, _, E), forward_match(M, Acc, G)).


% Faster version of matching where both string and regex are known
forward_match(S, [group([R])|Rs], Gs) :-
    !, % don't match groups in non-group clause
    remaining_after_match(R, S, N, Gs),
    append(G, N, S),
    forward_match(N, Rs, [G|Gs]).

forward_match(S, [group(R)|Rs], Gs) :-
    !, % don't match groups in non-group clause
    remaining_after_match(group(R), S, N, Gs),
    append(G, N, S),
    forward_match(N, Rs, [G|Gs]).

forward_match(S, [R|Rs], Gs) :-
    remaining_after_match(R, S, N, Gs),
    forward_match(N, Rs, Gs).

forward_match("", [], _).


% Find remainder of string that doesn't match given regex.
remaining_after_match(group([T]), O, N, G) :-
    !,
    split_respecting_arity(T, O, M, N),
    regex_tokens(M, [], [T], flags('', G, [])).

remaining_after_match(group(T), O, N, G) :-
    !,
    split_respecting_arity(T, O, M, N),
    regex_tokens(M, [], T, flags('', G, [])).

remaining_after_match(T, O, N, G) :-
    split_respecting_arity(T, O, M, N),
    regex_tokens(M, [], [T], flags('', G, [])).


% Split the string, ensuring that the first part has arity matching a given regex.
split_respecting_arity(R, [O|Os], [O], Os) :-
    arity(R, 1),
    !.

split_respecting_arity(R, O, M, N) :-
    arity(R, A),
    append(M, N, O),
    length(M, A),
    !.

split_respecting_arity(_, O, M, N) :-
    append(M, N, O),
    \+ M = [].


% Arity of certain kinds of regex, where this can be determined (i.e., no plus).
arity(R, 1) :-
    R = class(_)
  ; R = group([class(_)]),
    !.

arity(R, N) :-
    R = group(G),
    length(G, N),
    \+ (member(E, G), \+ arity(E, 1))
    .
