object Transformed {
  import Sudoku.Puzzle

  val transformed1: Puzzle[Set[Int]] = IndexedSeq(
    IndexedSeq(Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4))
  )

  val transformed2: Puzzle[Set[Int]] = IndexedSeq(
    IndexedSeq(Set(3), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8), Set(7)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3), Set(2)),
    IndexedSeq(Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9), Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(3), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  )

  val transformed3: Puzzle[Set[Int]] = IndexedSeq(
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8), Set(5)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(4), Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9)),
    IndexedSeq(Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(6), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9), Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  )

  val transformed4: Puzzle[Set[Int]] = IndexedSeq(
    IndexedSeq(Set(7), Set(9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3)),
    IndexedSeq(Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(7), Set(4)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6), Set(5)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(8), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9), Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  )

  val transformed5: Puzzle[Set[Int]] = IndexedSeq(
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3), Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(6), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(8), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(6), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1), Set(5)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(7)),
    IndexedSeq(Set(5), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(3)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(7), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(3), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9)),
    IndexedSeq(Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(4), Set(1), Set(1, 2, 3, 4, 5, 6, 7, 8, 9), Set(5), Set(2), Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  )
}
