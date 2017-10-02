% Strings should be interpreted as a list of character atoms.
:- set_prolog_flag('double_quotes', 'chars').

% Define the kinds of characters that exist.

digit('0').
digit('1').
digit('2').
digit('3').
digit('4').
digit('5').
digit('6').
digit('7').
digit('8').
digit('9').

lower('a').
lower('b').
lower('c').
lower('d').
lower('e').
lower('f').
lower('g').
lower('h').
lower('i').
lower('j').
lower('k').
lower('l').
lower('m').
lower('n').
lower('o').
lower('p').
lower('q').
lower('r').
lower('s').
lower('t').
lower('u').
lower('v').
lower('w').
lower('x').
lower('y').
lower('z').

upper('A').
upper('B').
upper('C').
upper('D').
upper('E').
upper('F').
upper('G').
upper('H').
upper('I').
upper('J').
upper('K').
upper('L').
upper('M').
upper('N').
upper('O').
upper('P').
upper('Q').
upper('R').
upper('S').
upper('T').
upper('U').
upper('V').
upper('W').
upper('X').
upper('Y').
upper('Z').

letter(C) :- lower(C).
letter(C) :- upper(C).

word(C) :- letter(C).
word(C) :- digit(C).
word('_').

unescaped(C) :- letter(C).
unescaped(C) :- digit(C).

escaped('.').
escaped(C, E) :- escaped(C), atom_concat(\, C, E).


% Start case and base case.

regx(Cs, [R|Rs]) :- regx([], Cs, [R|Rs]).
regx([], [], []).


% Regular expressions.

% Recognize unescaped characters as themselves.
regx(Cs, [C|Ps], [C|Rs]) :- unescaped(C), regx(Cs, Ps, Rs).

% Recognize escaped characters as themselves if they should be escaped.
regx(Cs, [C|Ps], [E|Rs]) :- escaped(C, E), regx(Cs, Ps, Rs).

% Recognize character classes.
regx(Cs, [C|Ps], ['\\d'|Rs]) :- digit(C), regx(Cs, Ps, Rs).
regx(Cs, [C|Ps], ['[A-Z]'|Rs]) :- upper(C), regx(Cs, Ps, Rs).
regx(Cs, [C|Ps], ['[a-z]'|Rs]) :- lower(C), regx(Cs, Ps, Rs).
regx(Cs, [C|Ps], ['[a-zA-Z]'|Rs]) :- letter(C), regx(Cs, Ps, Rs).
regx(Cs, [C|Ps], ['\\w'|Rs]) :- word(C), regx(Cs, Ps, Rs).

% Recognize repetition.
regx([], [C], [R,+]) :- regx([], [C], [R]).
regx(Cs, [C|Ps], [R,+|Rs]) :- regx([], [C], [R]), regx(Cs, Ps, [R,+|Rs]).
