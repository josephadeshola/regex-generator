digit(0).
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

% Start case and base case.
regx(Cs, Rs) :- regx_i(Cs, Rs).
regx_i([], []).

% A character is itself in a regex.  Need to add exceptions for escaped characters.
regx_i([C|Cs], [C|Rs]) :- regx_i(Cs, Rs).

% Recognize digits.
regx_i([C|Cs], [\d|Rs]) :- digit(C), regx_i(Cs, Rs).

% Recognize repetition.
regx_i([C|Cs], [R,+|Rs]) :- regx_i([C], [R]), regx_i(Cs, [R|Rs]).
regx_i([C|Cs], [R,+|Rs]) :- regx_i([C], [R]), regx_i(Cs, [R,+|Rs]).
