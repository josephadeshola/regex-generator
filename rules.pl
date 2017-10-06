:- consult('characters.pl').


% Regular expression rules.

% Recognize backreferences in single-character groups.
regex_tokens(Cs, Acc, Rs, flags(F, [G|Gs])) :-
    matches_group(Cs, [G|Gs], N, Os),
    regex_tokens(Os, [back(N)|Acc], Rs, flags(F, [G|Gs])).

% Recognize characters as themselves, either escaped or unescaped as necessary.
regex_tokens([C|Cs], Acc, Rs, F) :-
  ( unescaped(C), Class = C
  ; escaped(C, E), Class = E
  ),
  regex_tokens(Cs, [class(Class)|Acc], Rs, F).

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
  regex_tokens(Cs, [R|Acc], Rs, F).

% Recognize repetition for single characters.
regex_tokens([C|Cs], Acc, Rs, F) :-
    \+ F = plus,
    regex_tokens([C], [], [R], plus),
    \+ Acc = [plus(R)|_],
    regex_tokens(Cs, [plus(R)|Acc], Rs, F).
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
    \+ R = [plus(_)], % don't allow stuff of form (<>+)+
    regex_tokens(Os, [plus(group(R))|Acc], Rs, F).
regex_tokens(Cs, [plus(group(R))|Acc], Rs, F) :-
    \+ F = group,
    create_group(Cs, R, _, Os),
    regex_tokens(Os, [plus(group(R))|Acc], Rs, F).

% Base case.
regex_tokens("", Acc, Rs, _) :- reverse(Acc, Rs).

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
    regex_tokens(G, [], R, flags('', [])).

% Find matches.
matches_group(Cs, [G|Gs], N, Os) :-
    append(G, Os, Cs),
    length(Gs, Gl), N is Gl + 1.
matches_group(Cs, [G|Gs], N, Os) :-
    \+ append(G, _, Cs),
    matches_group(Cs, Gs, N, Os).
