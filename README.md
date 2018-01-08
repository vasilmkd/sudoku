# sudoku

A simple sudoku constraint solver. Each field of the puzzle is represented as a
set of all potential digits that can be written in that field. The algorithm
tries to satisfy all constraints. When it cannot continue, it tries to fix a
new constraint and continues depth first in order to satisfy. It backtracks
unless it finds a solution.

The Scala Option is used somewhat unidiomatically, but that is mainly to try
to keep the main looping function tail recursive.

```scala
import Sudoku._
import Puzzles._

solve(puzzle1) // pure function, finds the solution to the puzzle

visualSolve(puzzle1) // solves and pretty prints the solution
```
