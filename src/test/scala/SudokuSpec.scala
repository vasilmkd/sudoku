import org.scalatest._

import Puzzles._
import Sudoku._

class SudokuSpec extends FlatSpec with Matchers {

  "isSingle" should "return true only for sets with exactly one element" in {
    isSingle(Set(1)) should be(true)
    isSingle(Set('a')) should be(true)
    isSingle(Set("a")) should be(true)
  }

  it should "return false for the empty set" in {
    isSingle(Set()) should be(false)
  }

  it should "return false for any set with more than one element" in {
    isSingle(Set(1, 2, 3)) should be(false)
    isSingle(Set('a', 'b', 'c', 'd', 'e')) should be(false)
    isSingle(Set("abc", "def")) should be(false)
  }

  "replaceNth" should "replace the nth element in each collection" in {
    replaceNth(IndexedSeq(1, 2, 3), 0, 4) should be(Some(IndexedSeq(4, 2, 3)))
    replaceNth(IndexedSeq(1, 2, 3), 1, 5) should be(Some(IndexedSeq(1, 5, 3)))
  }

  it should "return None if an invalid index is specified" in {
    replaceNth(IndexedSeq(), 5, 5) should be(None)
    replaceNth(IndexedSeq(1, 2, 3), 5, 5) should be(None)
    replaceNth(IndexedSeq(1, 2, 3), -1, 5) should be(None)
  }

  "deleteFrom" should
    "delete the specified value from all sets in the collection" in {
    deleteFrom(IndexedSeq(Set(1, 2), Set(2, 3), Set(1, 2, 3, 4), Set(4, 5, 6)),
               Set(2)) should be
    (IndexedSeq(Set(1), Set(3), Set(1, 3, 4), Set(4, 5, 6)))
    deleteFrom(IndexedSeq(Set(1, 2, 3)), Set(1)) should be(
      IndexedSeq(Set(2, 3)))
  }

  it should "unless the set is a single set containing the same value" in {
    deleteFrom(IndexedSeq(Set(1)), Set(1)) should be(IndexedSeq(Set(1)))
  }

  it should "leave the sets unchanged if they don't contain the value" in {
    deleteFrom(IndexedSeq(Set(2, 3, 4)), Set(1)) should be(
      IndexedSeq(Set(2, 3, 4)))
  }

  it should "do nothing if operating on an empty sequence" in {
    deleteFrom(IndexedSeq(), Set(1)) should be(IndexedSeq())
  }

  "whichSubmatrix" should
    "return the corresponding submatrix for the row and column" in {
    whichSubmatrix(2, 8) should be(Some(2))
    whichSubmatrix(7, 1) should be(Some(6))
    whichSubmatrix(8, 8) should be(Some(8))
  }

  it should "return None for invalid row and column indices" in {
    whichSubmatrix(10, 10) should be(None)
    whichSubmatrix(-1, -1) should be(None)
  }

  "nthRow" should "return the nth row of the puzzle" in {
    nthRow(puzzle1, 0) should be(Some(IndexedSeq(4, 0, 0, 5, 0, 0, 0, 0, 6)))
    nthRow(puzzle1, 5) should be(Some(IndexedSeq(7, 0, 6, 0, 0, 3, 1, 0, 8)))
    nthRow(puzzle1, 8) should be(Some(IndexedSeq(0, 0, 3, 0, 8, 0, 0, 0, 4)))
  }

  it should "return None if the index is invalid" in {
    nthRow(puzzle1, -1) should be(None)
    nthRow(puzzle1, 10) should be(None)
  }

  "nthColumn" should "return the nth column of the puzzle" in {
    nthColumn(puzzle1, 0) should be(Some(IndexedSeq(4, 0, 1, 0, 0, 7, 0, 0, 0)))
    nthColumn(puzzle1, 5) should be(Some(IndexedSeq(0, 0, 7, 0, 0, 3, 0, 0, 0)))
    nthColumn(puzzle1, 8) should be(Some(IndexedSeq(6, 0, 0, 0, 0, 8, 5, 0, 4)))
  }

  it should "return None if the index is invalid" in {
    nthColumn(puzzle1, -1) should be(None)
    nthColumn(puzzle1, 10) should be(None)
  }

  "nthSubmatrix" should "return the nth submatrix of the puzzle" in {
    nthSubmatrix(puzzle1, 0) should be(
      Some(IndexedSeq(4, 0, 0, 0, 0, 0, 1, 0, 9)))
    nthSubmatrix(puzzle1, 5) should be(
      Some(IndexedSeq(0, 0, 0, 0, 0, 0, 1, 0, 8)))
    nthSubmatrix(puzzle1, 8) should be(
      Some(IndexedSeq(0, 0, 5, 7, 0, 0, 0, 0, 4)))
  }

  it should "return None if the index is invalid" in {
    nthSubmatrix(puzzle1, -1) should be(None)
    nthSubmatrix(puzzle1, 10) should be(None)
  }

  private def selfInverse[A](
      f: Puzzle[A] => Option[Puzzle[A]]): Puzzle[A] => Option[Puzzle[A]] =
    p =>
      for {
        fp <- f(p)
        ffp <- f(fp)
      } yield ffp

  "columns" should "be self inverse" in {
    selfInverse(columns[Int])(puzzle1) should be(Some(puzzle1))
    selfInverse(columns[Int])(puzzle2) should be(Some(puzzle2))
    selfInverse(columns[Int])(puzzle3) should be(Some(puzzle3))
    selfInverse(columns[Int])(puzzle4) should be(Some(puzzle4))
    selfInverse(columns[Int])(puzzle5) should be(Some(puzzle5))
  }

  "submatrices" should "be self inverse" in {
    selfInverse(submatrices[Int])(puzzle1) should be(Some(puzzle1))
    selfInverse(submatrices[Int])(puzzle2) should be(Some(puzzle2))
    selfInverse(submatrices[Int])(puzzle3) should be(Some(puzzle3))
    selfInverse(submatrices[Int])(puzzle4) should be(Some(puzzle4))
    selfInverse(submatrices[Int])(puzzle5) should be(Some(puzzle5))
  }

  "onlyOneIn" should
    "return true if the value can be found only once in the sets of the collection" in {
    onlyOneIn(IndexedSeq(Set(2, 3), Set(1)), 1) should be(true)
    onlyOneIn(IndexedSeq(Set(1)), 1) should be(true)
  }

  it should "return false otherwise" in {
    onlyOneIn(IndexedSeq(Set(1, 2, 3), Set(1), Set(5, 6, 7)), 1) should be(
      false)
    onlyOneIn(IndexedSeq(Set(1, 2, 3), Set(1, 2, 3)), 1) should be(false)
  }

  it should "return false if the collection is empty" in {
    onlyOneIn(IndexedSeq(), 1) should be(false)
  }

  "nextField" should "return the row and column index of the next field" in {
    nextField(4, 5) should be(Some(4, 6))
  }

  it should "start the next row" in {
    nextField(5, 8) should be(Some(6, 0))
  }

  it should "return None if the end of the puzzle was reached" in {
    nextField(8, 8) should be(None)
  }

  it should "return None if an invalid row or column is specified" in {
    nextField(5, -1) should be(None)
    nextField(10, 2) should be(None)
  }

  "solve" should "solve the sudoku puzzle correctly" in {
    solve(puzzle1) should be(solution1)
    solve(puzzle2) should be(solution2)
    solve(puzzle3) should be(solution3)
    solve(puzzle4) should be(solution4)
    solve(puzzle5) should be(solution5)
  }
}
