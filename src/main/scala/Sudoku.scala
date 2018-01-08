object Sudoku {

  type Puzzle[A] = IndexedSeq[IndexedSeq[A]]

  /** Checks if the set has exactly one element. */
  def isSingle[A](s: Set[A]): Boolean =
    s.size == 1
  
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
  
  /** Replaces the nth element in the indexed sequence. */
  def replaceNth[A](seq: IndexedSeq[A], n: Int, e: A): IndexedSeq[A] =
    seq.updated(n, e)
  
  /** Deletes the value v from all sets of potential digits in the same collection. */
  def deleteFrom(seq: IndexedSeq[Set[Int]], v: Set[Int]): IndexedSeq[Set[Int]] =
    for {
      s <- seq
    } yield (if (isSingle(s)) s else (s diff v))
  
  /** Returns the index of the submatrix that the field is in. */
  def whichSubmatrix(r: Int, c: Int): Int =
    if (r < 0 || c < 0) throw new IllegalArgumentException(s"row = $r, column = $c")
    else if (r < 3 && c < 3) 0
    else if (r < 3 && c < 6) 1
    else if (r < 3 && c < 9) 2
    else if (r < 6 && c < 3) 3
    else if (r < 6 && c < 6) 4
    else if (r < 6 && c < 9) 5
    else if (r < 9 && c < 3) 6
    else if (r < 9 && c < 6) 7
    else if (r < 9 && c < 9) 8
    else throw new IllegalArgumentException(s"row = $r, column = $c")
  
  /** Returns the nth row of the puzzle. */
  def nthRow[A](p: Puzzle[A], n: Int): IndexedSeq[A] =
    p(n)
  
  /** Returns the nth column of the puzzle. */
  def nthColumn[A](p: Puzzle[A], n: Int): IndexedSeq[A] =
    for {
      r <- p
    } yield r(n)

  /** Returns the nth submatrix of the puzzle. */
  def nthSubmatrix[A](p: Puzzle[A], n: Int): IndexedSeq[A] =
    if (n == 0) p.take(3).flatMap(_.take(3))
    else if (n == 1) p.take(3).flatMap(_.drop(3).take(3))
    else if (n == 2) p.take(3).flatMap(_.drop(6))
    else if (n == 3) p.drop(3).take(3).flatMap(_.take(3))
    else if (n == 4) p.drop(3).take(3).flatMap(_.drop(3).take(3))
    else if (n == 5) p.drop(3).take(3).flatMap(_.drop(6))
    else if (n == 6) p.drop(6).flatMap(_.take(3))
    else if (n == 7) p.drop(6).flatMap(_.drop(3).take(3))
    else if (n == 8) p.drop(6).flatMap(_.drop(6))
    else throw new IllegalArgumentException(s"n = $n")
  
  /** Returns all columns of the puzzle. */
  def columns[A](p: Puzzle[A]): Puzzle[A] =
    for {
      n <- (0 until 9)
    } yield (nthColumn(p, n))
  
  /** Returns all submatrices of the puzzle. */
  def submatrices[A](p: Puzzle[A]): Puzzle[A] =
    for {
      n <- (0 until 9)
    } yield (nthSubmatrix(p, n))
  

  /** Checks if the given value is found in only one field in the same collection. */
  def onlyOneIn(seq: IndexedSeq[Set[Int]], v: Int): Boolean =
    seq.count(_.contains(v)) == 1
  
  /** Checks if the given value can be found in other fields in the same row. */
  def checkRow(p: Puzzle[Set[Int]], n: Int, v: Int): Boolean =
    onlyOneIn(nthRow(p, n), v)
  
  /** Checks if the given value can be found in other fields in the same column. */
  def checkColumn(p: Puzzle[Set[Int]], n: Int, v: Int): Boolean =
    onlyOneIn(nthColumn(p, n), v)
  
  /** Checks if the given value can be found in other fields in the same column. */
  def checkSubmatrix(p: Puzzle[Set[Int]], n: Int, v: Int): Boolean =
    onlyOneIn(nthSubmatrix(p, n), v)
  
  /**
   * Checks if the given value can be found in the other fields in the same
   * row, column and submatrix.
   */
  def checkValue(p: Puzzle[Set[Int]], r: Int, c: Int, v: Int): Boolean = {
    val s = whichSubmatrix(r, c)
    checkRow(p, r, v) ||
      checkColumn(p, c, v) ||
      checkSubmatrix(p, s, v)
  }

  /** Replaces a whole row of the puzzle. */
  def patchRow[A](p: Puzzle[A], n: Int, r: IndexedSeq[A]): Puzzle[A] =
    replaceNth(p, n, r)
  
  /** Replaces a whole column of the puzzle. */
  def patchColumn[A](p: Puzzle[A], n: Int, c: IndexedSeq[A]): Puzzle[A] =
    columns(replaceNth(columns(p), n, c))
  
  /** Replaces a whole submatrix of the puzzle. */
  def patchSubmatrix[A](p: Puzzle[A], n: Int, s: IndexedSeq[A]): Puzzle[A] =
    submatrices(replaceNth(submatrices(p), n, s))
  
  /**
   * Called when a field with only one potential digit is encountered.
   * The same value is then deleted from all sets in the same row, column
   * and submatrix.
   */
  def transformation1(p: Puzzle[Set[Int]], r: Int, c: Int, s: Set[Int]): Puzzle[Set[Int]] = {
    val nr = deleteFrom(nthRow(p, r), s)
    val pr = patchRow(p, r, nr)
    val nc = deleteFrom(nthColumn(pr, c), s)
    val pc = patchColumn(pr, c, nc)
    val sub = whichSubmatrix(r, c)
    val nsub = deleteFrom(nthSubmatrix(pc, sub), s)
    patchSubmatrix(pc, sub, nsub)
  }

  /** Finds the value for which the second type of transformation will be carried out. */
  def findValue(p: Puzzle[Set[Int]], r: Int, c: Int, s: Set[Int]): Option[Int] = {
    val vals = for {
      v <- s if checkValue(p, r, c, v)
    } yield v
    vals.headOption
  }

  /**
   * Called when a field which can hold a potential value which is not found
   * in any other set in the same row, column or submatrix. That value replaces
   * the whole set.
   */
  def transformation2(p: Puzzle[Set[Int]], r: Int, c: Int, v: Int): Puzzle[Set[Int]] = {
    val row = nthRow(p, r)
    val nrow = replaceNth(row, c, Set(v))
    patchRow(p, r, nrow)
  }

  /** Returns the row and column index of the next field in the puzzle. */
  def nextField(r: Int, c: Int): Option[(Int, Int)] =
    if (r < 0 || c < 0) None
    else if (r >= 9 || c >= 9) None
    else if (r == 8 && c == 8) None
    else if (c == 8) Some(r + 1, 0)
    else Some(r, c + 1)
  
  private val correctRow = IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9)

  /** Checks if the number is not zero. */
  def isNotZero(n: Int): Boolean =
    n != 0
  
  /** Checks if the current state of the puzzle represents a possible solution. */
  def isSolution(p: Puzzle[Int]): Boolean =
    p.forall(_.forall(isNotZero))
  
  /** Checks if the current state of the puzzle is a correct solution. */
  def isErrorSolution(p: Puzzle[Int]): Boolean =
    p.map(_.sorted).exists(_ != correctRow)
  
  /** Backtracking check. */
  def checkError(p: Puzzle[Set[Int]]): Boolean = {
    val it = inverseTransform(p)
    isSolution(it) && (
      isErrorSolution(it) ||
      isErrorSolution(columns(it)) ||
      isErrorSolution(submatrices(it)))
  }

  /** Checks if all of the sets in the puzzle have exactly one possible value. */
  def isSolved(p: Puzzle[Set[Int]]): Boolean =
    p.forall(_.forall(isSingle))
  

  /** Returns the set representing the possible values for the field. */
  def getField(p: Puzzle[Set[Int]], r: Int, c: Int): Set[Int] =
    nthRow(p, r)(c)
  
  /** One iteration of the algorithm. */
  @annotation.tailrec
  def sudokuLoop(p: Puzzle[Set[Int]], r: Int, c: Int): Puzzle[Set[Int]] = {
    val s = getField(p, r, c)
    val next = nextField(r, c)
    if (next.isEmpty) p
    else {
      val (nr, nc) = next.get
      if (isSingle(s)) sudokuLoop(transformation1(p, r, c, s), nr, nc)
      else {
        val ov = findValue(p, r, c, s)
        if (ov.isDefined) sudokuLoop(transformation2(p, r, c, ov.get), nr, nc)
        else sudokuLoop(p, nr, nc)
      }
    }
  }

  /** Finds the first undecided field. */
  def findFirstNonSingle(p: Puzzle[Set[Int]]): (Int, Int, Set[Int]) = {
    @annotation.tailrec
    def loop(r: Int, c: Int): (Int, Int, Set[Int]) = {
      val s = getField(p, r, c)
      if (!isSingle(s)) (r, c, s)
      else {
        val (nr, nc) = nextField(r, c) getOrElse (-1, -1)
        loop(nr, nc)
      }
    }

    loop(0, 0)
  }

  /**
   * Implementation of the algorithm. When neither transformation1 nor
   * transformation2 can be carried it, it starts guessing numbers and
   * doing a depth first search for the solution.
   */
  def sudoku(p: Puzzle[Set[Int]]): Option[Puzzle[Set[Int]]] =
    if (checkError(p)) None
    else if (isSolved(p)) Some(p)
    else {
      val np = sudokuLoop(p, 0, 0)
      if (p == np) {
        val (r, c, s) = findFirstNonSingle(p)
        val res = for {
          v <- s.toStream
          o <- sudoku(transformation2(p, r, c, v))
        } yield o
        res.dropWhile(_.isEmpty).headOption
      } else sudoku(np)
    }
  
  /** Solves a sudoku puzzle. */
  def solve(p: Puzzle[Int]): Puzzle[Int] =
    inverseTransform(sudoku(transform(p)).get)
  
  private val borderString = "+---+---+---+---+---+---+---+---+---+\n"

  def numberString(n: Int): String =
    if (n == 0) " " else n.toString
  
  def rowString(r: IndexedSeq[Int]): String =
    r.map(numberString).mkString("| ", " | ", " |\n")
  
  def puzzleString(p: Puzzle[Int]): String =
    p.map(rowString).mkString(borderString, borderString, borderString)
  
  def printPuzzle(p: Puzzle[Int]): Unit =
    print(puzzleString(p))
  
  /** Shows the input and its solution on screen. */
  def visualSolve(p: Puzzle[Int]): Unit = {
    val s = solve(p)
    println("Input:")
    println()
    printPuzzle(p)
    println()
    println("Solution:")
    println()
    printPuzzle(s)
    println()
  }
}
