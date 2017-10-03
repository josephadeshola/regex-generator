% Define the kinds of characters that will be accepted in regular expressions.

% Digits
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

% Lowercase
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

% Uppercase
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

% Character classes involving digits and letters
letter(C) :- lower(C).
letter(C) :- upper(C).

:- dynamic word/1.
word(C) :- letter(C).
word(C) :- digit(C).
word('_').

unescaped(C) :- letter(C).
unescaped(C) :- digit(C).

% Special characters
escaped('.').
escaped('(').
escaped(')').
escaped(C, E) :- escaped(C), atom_concat(\, C, E).


