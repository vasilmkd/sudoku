# sudoku

A simple sudoku constraint solver. Each field of the puzzle is represented as a
set of all potential digits that can be written in that field. The algorithm
tries to satisfy all constraints. When it cannot continue, it tries to fix a
new constraint and continues depth first to try to bring it to a solution,
otherwise it backtracks.

The Scala Option is used somewhat unidiomatically, but that is mainly to try
to keep the main looping function tail recursive.
