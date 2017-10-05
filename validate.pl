% Prune the number of regex's returned to the user by not allowing nested groups, or single element groups that aren't
% backreferenced.

validate(Rs) :- back_count(Rs, [], N), validate(Rs, N).
validate([R|Rs], G) :- \+ functor(R, group, 1), \+ functor(R, plus, 1), validate(Rs, G).
validate([plus(R)|Rs], G) :- \+ contains_group(R), validate(Rs, G).
validate([plus(group(R))|Rs], G) :- length(R, L), L >= 2, group_validate(R), validate(Rs, G).
validate([group([R])|Rs], G) :- G > 0, group_validate([R]), Gn is G - 1, validate(Rs, Gn).
validate([plus(group([R]))|Rs], G) :- group_validate([R]), Gn is G - 1, validate(Rs, Gn).
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
