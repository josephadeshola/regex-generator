% Split into groups. Don't allow groups of 1 where that character does not appear later in the string (for efficiency).
create_group([T1,T2|Ts], Group, Rest) :-
    append(Group, Rest, [T1,T2|Ts]),
    length(Group, L), L >= 2.

create_group([T1,T2|Ts], [Group], Rest) :-
    append([Group], Rest, [T1,T2|Ts]),
    member(Group, Rest).


% Split into groups where first group matches regex.
create_group(Whole, R, Group, Rest) :-
    append(Group, Rest, Whole), length(Group, L), L >= 2,
    regex_tokens(Group, [], R, flags('', [], [])).


% Find matches.
matches_group(Cs, [G|Groups], Index, Rest) :-
    append(G, Rest, Cs), % G is a prefix of Cs
    length(Groups, L), Index is L + 1. % figure out back reference index

matches_group(Cs, [G|Groups], Index, Rest) :-
    \+ append(G, _, Cs), % G is not a prefix of Cs
    matches_group(Cs, Groups, Index, Rest). % use back reference index found by recursion, if found


% Faster version of matching where both string and regex are known
forward_match(String, [group([R])|Rs], Groups) :-
    !, % don't match groups in non-group clause
    remaining_after_match(R, String, Rest, Groups),
    append(Match, Rest, String),
    forward_match(Rest, Rs, [Match|Groups]).

forward_match(String, [group(R)|Rs], Groups) :-
    !, % don't match groups in non-group clause
    remaining_after_match(group(R), String, Rest, Groups),
    append(Match, Rest, String),
    forward_match(Rest, Rs, [Match|Groups]).

forward_match(String, [R|Rs], Groups) :-
    remaining_after_match(R, String, Rest, Groups),
    forward_match(Rest, Rs, Groups).

forward_match("", [], _).


% Find remainder of string that doesn't match given regex.
remaining_after_match(group([R]), String, Rest, Groups) :-
    !,
    split_respecting_arity(R, String, Match, Rest),
    regex_tokens(Match, [], [R], flags('', Groups, [])).

remaining_after_match(group(R), String, Rest, Groups) :-
    !,
    split_respecting_arity(R, String, Match, Rest),
    regex_tokens(Match, [], R, flags('', Groups, [])).

remaining_after_match(R, String, Rest, Groups) :-
    split_respecting_arity(R, String, Match, Rest),
    regex_tokens(Match, [], [R], flags('', Groups, [])).


% Split the string, ensuring that the first part has arity matching a given regex.
split_respecting_arity(R, [O|Os], [O], Os) :-
    arity(R, 1),
    !.

split_respecting_arity(R, String, Match, Rest) :-
    arity(R, A),
    append(Match, Rest, String),
    length(Match, A),
    !.

split_respecting_arity(_, String, Match, Rest) :-
    append(Match, Rest, String),
    \+ Match = [].


% Arity of certain kinds of regex, where this can be determined (i.e., no plus).
arity(R, 1) :-
    R = class(_)
  ; R = group([class(_)]),
    !.

arity(R, N) :-
    R = group(G),
    length(G, N),
    \+ (member(E, G), \+ arity(E, 1)) % no pluses in the group
    .
