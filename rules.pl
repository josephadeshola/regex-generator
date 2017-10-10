:- compile('characters.pl').
:- compile('utils.pl').


% Regular expression rules.


% Recognize backreferences.
regex_tokens(Cs, Acc, Rs, flags(F, [G|Groups], O)) :-
    matches_group(Cs, [G|Groups], Index, Rest),
    regex_tokens(Rest, [back(Index)|Acc], Rs, flags(F, [G|Groups], O)).


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
    \+ F = flags(plus, _, _),
    regex_tokens([C], [], [R], flags(plus, [], [])),
    \+ Acc = [plus(R)|_],
    regex_tokens(Cs, [plus(R)|Acc], Rs, F).

regex_tokens([C|Cs], [plus(R)|Acc], Rs, F) :-
    \+ F = flags(plus, _, _),
    regex_tokens([C], [], [R], flags(plus, [], [])),
    regex_tokens(Cs, [plus(R)|Acc], Rs, F).


% Allow groups.
regex_tokens(Cs, Acc, Rs, flags(F, Gs, Os)) :-
    \+ F = group,
    create_group(Cs, Group, Rest),
    regex_tokens(Group, [], R, flags(group, [], [])),
    regex_tokens(Rest, [group(R)|Acc], Rs, flags(F, [Group|Gs], Os)).


% Allow group repetition.
regex_tokens(Cs, Acc, Rs, F) :-
    \+ F = flags(group, _, _),
    create_group(Cs, Group, Rest),
    regex_tokens(Group, [], R, flags(group, [], [])),
    \+ R = [plus(_)], % don't allow stuff of form (<>+)+
    regex_tokens(Rest, [plus(group(R))|Acc], Rs, F).

regex_tokens(Cs, [plus(group(R))|Acc], Rs, F) :-
    \+ F = flags(group, _, _),
    create_group(Cs, R, _, Rest),
    regex_tokens(Rest, [plus(group(R))|Acc], Rs, F).


% Base case.
regex_tokens("", Acc, Rs, flags(_, _, Others)) :-
    reverse(Acc, Rs), % reverse for correct order
    \+ (member(E, Others), \+ forward_match(E, Rs, [])), % we've matched all the other strings
    !.
