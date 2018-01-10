import org.scalatest._

class TransformSpec extends FlatSpec with Matchers {
  import Transform._
  import Puzzles._
  import TransformedPuzzles._

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
}
