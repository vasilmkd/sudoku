import Sudoku._

object Show {
  private val borderString = "+---+---+---+---+---+---+---+---+---+\n"

  private def numberString(n: Int): String =
    if (n == 0) " " else n.toString

  private def rowString(r: IndexedSeq[Int]): String =
    r.map(numberString).mkString("| ", " | ", " |\n")

  def show(p: Puzzle[Int]): String =
    p.map(rowString).mkString(borderString, borderString, borderString)
}
