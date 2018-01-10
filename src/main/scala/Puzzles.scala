import Sudoku.Puzzle

object Puzzles {

  val puzzle1: Puzzle[Int] = IndexedSeq(
    IndexedSeq(4, 0, 0, 5, 0, 0, 0, 0, 6),
    IndexedSeq(0, 0, 0, 0, 0, 0, 0, 2, 0),
    IndexedSeq(1, 0, 9, 0, 0, 7, 0, 5, 0),
    IndexedSeq(0, 0, 0, 7, 0, 0, 0, 0, 0),
    IndexedSeq(0, 4, 0, 0, 0, 0, 0, 0, 0),
    IndexedSeq(7, 0, 6, 0, 0, 3, 1, 0, 8),
    IndexedSeq(0, 0, 0, 2, 0, 0, 0, 0, 5),
    IndexedSeq(0, 8, 0, 0, 9, 0, 7, 0, 0),
    IndexedSeq(0, 0, 3, 0, 8, 0, 0, 0, 4)
  )

  val solution1: Puzzle[Int] = IndexedSeq(
    IndexedSeq(4, 3, 2, 5, 1, 8, 9, 7, 6),
    IndexedSeq(8, 5, 7, 6, 3, 9, 4, 2, 1),
    IndexedSeq(1, 6, 9, 4, 2, 7, 8, 5, 3),
    IndexedSeq(3, 1, 8, 7, 4, 2, 5, 6, 9),
    IndexedSeq(9, 4, 5, 8, 6, 1, 2, 3, 7),
    IndexedSeq(7, 2, 6, 9, 5, 3, 1, 4, 8),
    IndexedSeq(6, 9, 1, 2, 7, 4, 3, 8, 5),
    IndexedSeq(5, 8, 4, 3, 9, 6, 7, 1, 2),
    IndexedSeq(2, 7, 3, 1, 8, 5, 6, 9, 4)
  )

  val puzzle2: Puzzle[Int] = IndexedSeq(
    IndexedSeq(3, 0, 5, 0, 0, 0, 0, 8, 7),
    IndexedSeq(0, 4, 0, 0, 0, 0, 0, 3, 2),
    IndexedSeq(7, 0, 1, 0, 0, 0, 0, 5, 0),
    IndexedSeq(0, 0, 9, 5, 0, 0, 0, 0, 0),
    IndexedSeq(0, 0, 3, 0, 6, 2, 0, 0, 0),
    IndexedSeq(0, 0, 0, 0, 0, 7, 0, 2, 0),
    IndexedSeq(0, 6, 0, 0, 0, 0, 0, 0, 4),
    IndexedSeq(0, 0, 0, 0, 1, 0, 0, 0, 6),
    IndexedSeq(0, 0, 0, 4, 3, 0, 8, 0, 0)
  )

  val solution2: Puzzle[Int] = IndexedSeq(
    IndexedSeq(3, 2, 5, 6, 9, 1, 4, 8, 7),
    IndexedSeq(9, 4, 6, 7, 5, 8, 1, 3, 2),
    IndexedSeq(7, 8, 1, 3, 2, 4, 6, 5, 9),
    IndexedSeq(2, 1, 9, 5, 4, 3, 7, 6, 8),
    IndexedSeq(8, 7, 3, 9, 6, 2, 5, 4, 1),
    IndexedSeq(6, 5, 4, 1, 8, 7, 9, 2, 3),
    IndexedSeq(5, 6, 8, 2, 7, 9, 3, 1, 4),
    IndexedSeq(4, 3, 7, 8, 1, 5, 2, 9, 6),
    IndexedSeq(1, 9, 2, 4, 3, 6, 8, 7, 5)
  )

  val puzzle3: Puzzle[Int] = IndexedSeq(
    IndexedSeq(0, 0, 0, 0, 6, 0, 0, 8, 5),
    IndexedSeq(0, 0, 1, 0, 2, 0, 0, 0, 0),
    IndexedSeq(0, 0, 0, 8, 0, 1, 0, 9, 0),
    IndexedSeq(0, 0, 0, 1, 4, 7, 0, 0, 9),
    IndexedSeq(5, 0, 0, 9, 0, 0, 0, 0, 0),
    IndexedSeq(0, 0, 0, 0, 0, 0, 0, 4, 0),
    IndexedSeq(6, 1, 0, 0, 0, 0, 0, 0, 2),
    IndexedSeq(0, 9, 7, 0, 0, 0, 3, 0, 0),
    IndexedSeq(0, 0, 2, 0, 5, 0, 0, 0, 0)
  )

  val solution3: Puzzle[Int] = IndexedSeq(
    IndexedSeq(7, 2, 3, 4, 6, 9, 1, 8, 5),
    IndexedSeq(9, 8, 1, 5, 2, 3, 4, 6, 7),
    IndexedSeq(4, 5, 6, 8, 7, 1, 2, 9, 3),
    IndexedSeq(2, 6, 8, 1, 4, 7, 5, 3, 9),
    IndexedSeq(5, 3, 4, 9, 8, 6, 7, 2, 1),
    IndexedSeq(1, 7, 9, 2, 3, 5, 6, 4, 8),
    IndexedSeq(6, 1, 5, 3, 9, 4, 8, 7, 2),
    IndexedSeq(8, 9, 7, 6, 1, 2, 3, 5, 4),
    IndexedSeq(3, 4, 2, 7, 5, 8, 9, 1, 6)
  )

  val puzzle4: Puzzle[Int] = IndexedSeq(
    IndexedSeq(7, 9, 0, 0, 0, 3, 0, 0, 2),
    IndexedSeq(0, 6, 0, 5, 1, 0, 0, 0, 0),
    IndexedSeq(0, 0, 0, 0, 0, 2, 0, 0, 6),
    IndexedSeq(0, 0, 0, 8, 0, 1, 0, 4, 0),
    IndexedSeq(0, 0, 0, 0, 0, 0, 1, 0, 3),
    IndexedSeq(5, 0, 0, 0, 2, 0, 0, 0, 0),
    IndexedSeq(0, 0, 0, 0, 0, 0, 0, 7, 4),
    IndexedSeq(0, 0, 1, 0, 0, 0, 0, 6, 5),
    IndexedSeq(0, 0, 8, 0, 9, 7, 0, 0, 0)
  )

  val solution4: Puzzle[Int] = IndexedSeq(
    IndexedSeq(7, 9, 5, 6, 8, 3, 4, 1, 2),
    IndexedSeq(2, 6, 4, 5, 1, 9, 7, 3, 8),
    IndexedSeq(1, 8, 3, 7, 4, 2, 5, 9, 6),
    IndexedSeq(9, 3, 6, 8, 5, 1, 2, 4, 7),
    IndexedSeq(8, 4, 2, 9, 7, 6, 1, 5, 3),
    IndexedSeq(5, 1, 7, 3, 2, 4, 6, 8, 9),
    IndexedSeq(3, 2, 9, 1, 6, 5, 8, 7, 4),
    IndexedSeq(4, 7, 1, 2, 3, 8, 9, 6, 5),
    IndexedSeq(6, 5, 8, 4, 9, 7, 3, 2, 1)
  )

  val puzzle5: Puzzle[Int] = IndexedSeq(
    IndexedSeq(0, 0, 0, 3, 4, 0, 0, 0, 0),
    IndexedSeq(6, 0, 0, 0, 0, 1, 0, 4, 0),
    IndexedSeq(7, 0, 4, 0, 0, 5, 1, 0, 0),
    IndexedSeq(8, 0, 0, 0, 6, 0, 0, 1, 5),
    IndexedSeq(0, 2, 0, 0, 0, 0, 4, 0, 7),
    IndexedSeq(5, 0, 0, 0, 0, 0, 0, 0, 3),
    IndexedSeq(0, 0, 0, 9, 0, 0, 0, 0, 0),
    IndexedSeq(0, 0, 7, 0, 5, 3, 0, 0, 0),
    IndexedSeq(0, 0, 0, 4, 1, 0, 5, 2, 0)
  )

  val solution5: Puzzle[Int] = IndexedSeq(
    IndexedSeq(1, 8, 2, 3, 4, 9, 7, 5, 6),
    IndexedSeq(6, 3, 5, 2, 7, 1, 8, 4, 9),
    IndexedSeq(7, 9, 4, 6, 8, 5, 1, 3, 2),
    IndexedSeq(8, 4, 3, 7, 6, 2, 9, 1, 5),
    IndexedSeq(9, 2, 1, 5, 3, 8, 4, 6, 7),
    IndexedSeq(5, 7, 6, 1, 9, 4, 2, 8, 3),
    IndexedSeq(4, 5, 8, 9, 2, 6, 3, 7, 1),
    IndexedSeq(2, 1, 7, 8, 5, 3, 6, 9, 4),
    IndexedSeq(3, 6, 9, 4, 1, 7, 5, 2, 8)
  )

  val hard: Puzzle[Int] = IndexedSeq(
    IndexedSeq(0, 0, 2, 0, 0, 0, 0, 0, 0),
    IndexedSeq(8, 9, 0, 0, 4, 0, 0, 0, 0),
    IndexedSeq(0, 0, 0, 5, 0, 0, 0, 1, 0),
    IndexedSeq(0, 0, 5, 0, 0, 0, 7, 0, 0),
    IndexedSeq(0, 0, 0, 0, 8, 0, 0, 3, 0),
    IndexedSeq(0, 0, 0, 0, 0, 9, 0, 0, 0),
    IndexedSeq(0, 0, 0, 0, 0, 0, 0, 0, 8),
    IndexedSeq(0, 0, 0, 0, 0, 2, 0, 4, 9),
    IndexedSeq(0, 0, 6, 7, 0, 0, 0, 0, 0)
  )
}
