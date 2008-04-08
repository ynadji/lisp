#!/bin/bash
# Runs the necessary commands to work
# as a trampoline for sbcl.

if [ $# -eq 0 ]
then
	echo "Usage ./sudoku.sh INPUT_PUZZLE [SOLUTION]"
else
	if [ $# -eq 1 ]
	then
		sbcl --noinform --load config-parser.fasl --load sudoku.fasl --eval "(progn (sudoku:backtracking \"$1\") (quit))"
	else
		sbcl --noinform --load config-parser.fasl --load sudoku.fasl --eval "(progn (sudoku:backtracking \"$1\" \"$2\") (quit))"
	fi
fi
