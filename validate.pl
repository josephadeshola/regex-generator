% Prune the number of regex's returned to the user by not allowing nested groups, or single element groups that aren't
% backreferenced.

validate(Rs) :-
    back_count(Rs, [], N),
    validate(Rs, N).

validate([R|Rs], G) :-
    \+ functor(R, group, 1),
    \+ functor(R, plus, 1),
    validate(Rs, G).

validate([plus(R)|Rs], G) :-
    \+ contains_group(R),
    validate(Rs, G).

validate([plus(group(R))|Rs], G) :-
    length(R, L), L >= 2,
    group_validate(R),
    validate(Rs, G).

validate([group([R])|Rs], G) :-
    G > 0,
    group_validate([R]), Gn is G - 1,
    validate(Rs, Gn).

validate([plus(group([R]))|Rs], G) :-
    group_validate([R]), Gn is G - 1,
    validate(Rs, Gn).

validate([], _).


% Validate no nested groups, and inside of groups is okay
group_validate(R) :-
    \+ contains_group(R),
    validate(R).

contains_group(R) :-
    R = group(_)
  ; R = [group(_)|_]
  ; R = [plus(P)|_], contains_group(P)
  ; R = [_|Rs], contains_group(Rs).


% Count the number of unique back references in a regex.
back_count([R|Rs], Bs, N) :-  % not a back reference
    \+ functor(R, back, 1),
    back_count(Rs, Bs, N).

back_count([back(B)|Rs], Bs, N) :- % repeat back reference
    member(B, Bs),
    back_count(Rs, Bs, N).

back_count([back(B)|Rs], Bs, N) :- % new back reference
    \+ member(B, Bs),
    back_count(Rs, [B|Bs], Bn), N is Bn + 1.

back_count([], _, 0). % base case
