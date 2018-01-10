import Sudoku._

object Transform {

  /** The set which replaces all 0s in the input puzzle. */
  private val allDigits = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  /** Replaces all 0s with the set of all potential digits. */
  def transformNumber(n: Int): Set[Int] =
    if (n == 0) allDigits else Set(n)
  
  /** Transforms each number in the given row of the puzzle. */
  def transformRow(r: IndexedSeq[Int]): IndexedSeq[Set[Int]] =
    r map transformNumber
  
  /** Transforms each number in the input sudoku puzzle. */
  def transform(p: Puzzle[Int]): Puzzle[Set[Int]] =
    p map transformRow
  
  /** Replaces a given set of potential digits with a number. */
  def inverseTransformNumber(s: Set[Int]): Int =
    if (isSingle(s)) s.head else 0
  
  /** Replaces all sets in a row with a number. */
  def inverseTransformRow(r: IndexedSeq[Set[Int]]): IndexedSeq[Int] =
    r map inverseTransformNumber
  
  /** Replaces all sets in a puzzle with a number. */
  def inverseTransform(p: Puzzle[Set[Int]]): Puzzle[Int] =
    p map inverseTransformRow
}
