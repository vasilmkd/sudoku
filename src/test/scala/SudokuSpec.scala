import org.scalatest._

class SudokuSpec extends FlatSpec with Matchers {
  import Sudoku._
  import Puzzles._

  "isSingle" should "return true only for sets with exactly one element" in {
    isSingle(Set(1)) should be (true)
    isSingle(Set('a')) should be (true)
    isSingle(Set("a")) should be (true)
  }

  it should "return false for the empty set" in {
    isSingle(Set()) should be (false)
  }

  it should "return false for any set with more than one element" in {
     isSingle(Set(1, 2, 3)) should be (false)
     isSingle(Set('a', 'b', 'c', 'd', 'e')) should be (false)
     isSingle(Set("abc", "def")) should be (false)
  }

  "transformNumber" should "return the number in a set if the number is not zero" in {
    transformNumber(1) should be (Set(1))
    transformNumber(2) should be (Set(2))
    transformNumber(3) should be (Set(3))
    transformNumber(4) should be (Set(4))
    transformNumber(5) should be (Set(5))
    transformNumber(6) should be (Set(6))
    transformNumber(7) should be (Set(7))
    transformNumber(8) should be (Set(8))
    transformNumber(9) should be (Set(9))
  }

  it should "return the set with all digits if the number is zero" in {
    transformNumber(0) should be (Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  "transform" should "transform the input sudoku puzzle into a puzzle suitable for solving" in {
    import Transformed._
    transform(puzzle1) should be (transformed1)
    transform(puzzle2) should be (transformed2)
    transform(puzzle3) should be (transformed3)
    transform(puzzle4) should be (transformed4)
    transform(puzzle5) should be (transformed5)
  }

  "inverseTransformNumber" should "be the inverse function of transformNumber" in {
    inverseTransformNumber(transformNumber(1)) should be (1)
    inverseTransformNumber(transformNumber(2)) should be (2)
    inverseTransformNumber(transformNumber(3)) should be (3)
    inverseTransformNumber(transformNumber(4)) should be (4)
    inverseTransformNumber(transformNumber(5)) should be (5)
    inverseTransformNumber(transformNumber(6)) should be (6)
    inverseTransformNumber(transformNumber(7)) should be (7)
    inverseTransformNumber(transformNumber(8)) should be (8)
    inverseTransformNumber(transformNumber(9)) should be (9)
    inverseTransformNumber(transformNumber(0)) should be (0)
  }

  "inverseTransform" should "be the inverse function of transform" in {
    inverseTransform(transform(puzzle1)) should be (puzzle1)
    inverseTransform(transform(puzzle2)) should be (puzzle2)
    inverseTransform(transform(puzzle3)) should be (puzzle3)
    inverseTransform(transform(puzzle4)) should be (puzzle4)
    inverseTransform(transform(puzzle5)) should be (puzzle5)
  }

  "replaceNth" should "replace the nth element in each collection" in {
    replaceNth(IndexedSeq(1, 2, 3), 0, 4) should be (IndexedSeq(4, 2, 3))
    replaceNth(IndexedSeq(1, 2, 3), 1, 5) should be (IndexedSeq(1, 5, 3))
  }

  it should "throw IndexOutOfBoundsException if an invalid index is specified" in {
    a [IndexOutOfBoundsException] should be thrownBy {
      replaceNth(IndexedSeq(), 5, 5)
    }
    a [IndexOutOfBoundsException] should be thrownBy {
      replaceNth(IndexedSeq(1, 2, 3), 5, 5)
    }
    a [IndexOutOfBoundsException] should be thrownBy {
      replaceNth(IndexedSeq(1, 2, 3), -1, 5)
    }
  }

  "deleteFrom" should "delete the specified value from all sets in the collection" in {
    deleteFrom(IndexedSeq(Set(1, 2), Set(2, 3), Set(1, 2, 3, 4), Set(4, 5, 6)), Set(2)) should be
      (IndexedSeq(Set(1), Set(3), Set(1, 3, 4), Set(4, 5, 6)))
    deleteFrom(IndexedSeq(Set(1, 2, 3)), Set(1)) should be (IndexedSeq(Set(2, 3)))
  }

  it should "unless the set is a single set containing the same value" in {
    deleteFrom(IndexedSeq(Set(1)), Set(1)) should be (IndexedSeq(Set(1)))
  }

  it should "leave the sets unchanged if they don't contain the value" in {
    deleteFrom(IndexedSeq(Set(2, 3, 4)), Set(1)) should be (IndexedSeq(Set(2, 3, 4)))
  }

  it should "do nothing if operating on an empty sequence" in {
    deleteFrom(IndexedSeq(), Set(1)) should be (IndexedSeq())
  }

  "whichSubmatrix" should "return the corresponding submatrix for the row and column" in {
    whichSubmatrix(2, 8) should be (2)
    whichSubmatrix(7, 1) should be (6)
    whichSubmatrix(8, 8) should be (8)
  }

  it should "throw IllegalArgumentException for invalid row and column indices" in {
    a [IllegalArgumentException] should be thrownBy {
      whichSubmatrix(10, 10)
    }
    a [IllegalArgumentException] should be thrownBy {
      whichSubmatrix(-1, -1)
    }
  }

  "nthRow" should "return the nth row of the puzzle" in {
    nthRow(puzzle1, 0) should be (IndexedSeq(4, 0, 0, 5, 0, 0, 0, 0, 6))
    nthRow(puzzle1, 5) should be (IndexedSeq(7, 0, 6, 0, 0, 3, 1, 0, 8))
    nthRow(puzzle1, 8) should be (IndexedSeq(0, 0, 3, 0, 8, 0, 0, 0, 4))
  }

  it should "throw IndexOutOfBoundsException if the index is invalid" in {
    a [IndexOutOfBoundsException] should be thrownBy {
      nthRow(puzzle1, -1)
    }
    a [IndexOutOfBoundsException] should be thrownBy {
      nthRow(puzzle1, 10)
    }
  }

  "nthColumn" should "return the nth column of the puzzle" in {
    nthColumn(puzzle1, 0) should be (IndexedSeq(4, 0, 1, 0, 0, 7, 0, 0, 0))
    nthColumn(puzzle1, 5) should be (IndexedSeq(0, 0, 7, 0, 0, 3, 0, 0, 0))
    nthColumn(puzzle1, 8) should be (IndexedSeq(6, 0, 0, 0, 0, 8, 5, 0, 4))
  }

  it should "throw IndexOutOfBoundsException if the index is invalid" in {
    a [IndexOutOfBoundsException] should be thrownBy {
      nthColumn(puzzle1, -1)
    }
    a [IndexOutOfBoundsException] should be thrownBy {
      nthColumn(puzzle1, 10)
    }
  }

  "nthSubmatrix" should "return the nth submatrix of the puzzle" in {
    nthSubmatrix(puzzle1, 0) should be (IndexedSeq(4, 0, 0, 0, 0, 0, 1, 0, 9))
    nthSubmatrix(puzzle1, 5) should be (IndexedSeq(0, 0, 0, 0, 0, 0, 1, 0, 8))
    nthSubmatrix(puzzle1, 8) should be (IndexedSeq(0, 0, 5, 7, 0, 0, 0, 0, 4))
  }

  it should "throw IllegalArgumentException if the index is invalid" in {
    a [IllegalArgumentException] should be thrownBy {
      nthSubmatrix(puzzle1, -1)
    }
    a [IllegalArgumentException] should be thrownBy {
      nthSubmatrix(puzzle1, 10)
    }
  }

  "columns" should "be self inverse" in {
    columns(columns(puzzle1)) should be (puzzle1)
    columns(columns(puzzle2)) should be (puzzle2)
    columns(columns(puzzle3)) should be (puzzle3)
    columns(columns(puzzle4)) should be (puzzle4)
    columns(columns(puzzle5)) should be (puzzle5)
  }

  "submatrices" should "be self inverse" in {
    submatrices(submatrices(puzzle1)) should be (puzzle1)
    submatrices(submatrices(puzzle2)) should be (puzzle2)
    submatrices(submatrices(puzzle3)) should be (puzzle3)
    submatrices(submatrices(puzzle4)) should be (puzzle4)
    submatrices(submatrices(puzzle5)) should be (puzzle5)
  }

  "onlyOneIn" should "return true if the value can be found only once in the sets of the collection" in {
    onlyOneIn(IndexedSeq(Set(2, 3), Set(1)), 1) should be (true)
    onlyOneIn(IndexedSeq(Set(1)), 1) should be (true)
  }

  it should "return false otherwise" in {
    onlyOneIn(IndexedSeq(Set(1, 2, 3), Set(1), Set(5, 6, 7)), 1) should be (false)
    onlyOneIn(IndexedSeq(Set(1, 2, 3), Set(1, 2, 3)), 1) should be (false)
  }

  it should "return false if the collection is empty" in {
    onlyOneIn(IndexedSeq(), 1) should be (false)
  }

  "nextField" should "return the row and column index of the next field" in {
    nextField(4, 5) should be (Some(4, 6))
  }

  it should "start the next row" in {
    nextField(5, 8) should be (Some(6, 0))
  }

  it should "return None if the end of the puzzle was reached" in {
    nextField(8, 8) should be (None)
  }

  it should "return None if an invalid row or column is specified" in {
    nextField(5, -1) should be (None)
    nextField(10, 2) should be (None)
  }

  "solve" should "solve the sudoku puzzle correctly" in {
    solve(puzzle1) should be (solution1)
    solve(puzzle2) should be (solution2)
    solve(puzzle3) should be (solution3)
    solve(puzzle4) should be (solution4)
    solve(puzzle5) should be (solution5)
  }

  "numberString" should "return the number as string" in {
    numberString(1) should be ("1")
    numberString(2) should be ("2")
    numberString(3) should be ("3")
    numberString(4) should be ("4")
    numberString(5) should be ("5")
    numberString(6) should be ("6")
    numberString(7) should be ("7")
    numberString(8) should be ("8")
    numberString(9) should be ("9")
  }

  it should "return a space string" in {
    numberString(0) should be (" ")
  }
}
