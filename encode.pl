% Convert regular expression tokens to regular expression strings.

regex_encode([R|Rs], S) :- regex_encode(R, Sr), regex_encode(Rs, Ss), atom_concat(Sr, Ss, S).
regex_encode([], '').
regex_encode(class(C), C).
regex_encode(plus(E), P) :- regex_encode(E, C), atom_concat(C, +, P).
regex_encode(group(E), G) :- regex_encode(E, R), atom_concat('(', R, O), atom_concat(O, ')', G).
% This number -> atom conversion means we only support up to 9 groups.  Okay for now.
regex_encode(back(N), B) :- number_chars(N, [C]), atom_concat('$', C, B).
