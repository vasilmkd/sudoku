import scala.util.Try

import Transform._
import Traverse._

object Sudoku {

  type Puzzle[A] = IndexedSeq[IndexedSeq[A]]

  /** Checks if the set has exactly one element. */
  def isSingle[A](s: Set[A]): Boolean =
    s.size == 1
  
  /** Replaces the nth element in the indexed sequence. */
  def replaceNth[A](seq: IndexedSeq[A], n: Int, e: A): Option[IndexedSeq[A]] =
    Try(seq.updated(n, e)).toOption
  
  /** Deletes the value v from all sets of potential digits in the same collection. */
  def deleteFrom(seq: IndexedSeq[Set[Int]], v: Set[Int]): IndexedSeq[Set[Int]] =
    for {
      s <- seq
    } yield (if (isSingle(s)) s else (s diff v))
  
  /** Returns the index of the submatrix that the field is in. */
  def whichSubmatrix(r: Int, c: Int): Option[Int] =
    if (r < 0 || c < 0) None
    else if (r < 3 && c < 3) Some(0)
    else if (r < 3 && c < 6) Some(1)
    else if (r < 3 && c < 9) Some(2)
    else if (r < 6 && c < 3) Some(3)
    else if (r < 6 && c < 6) Some(4)
    else if (r < 6 && c < 9) Some(5)
    else if (r < 9 && c < 3) Some(6)
    else if (r < 9 && c < 6) Some(7)
    else if (r < 9 && c < 9) Some(8)
    else None
  
  /** Returns the nth row of the puzzle. */
  def nthRow[A](p: Puzzle[A], n: Int): Option[IndexedSeq[A]] =
    Try(p(n)).toOption
  
  /** Returns the nth column of the puzzle. */
  def nthColumn[A](p: Puzzle[A], n: Int): Option[IndexedSeq[A]] = {
    val seq = for (r <- p) yield Try(r(n)).toOption
    sequence(seq)
  }

  /** Returns the nth submatrix of the puzzle. */
  def nthSubmatrix[A](p: Puzzle[A], n: Int): Option[IndexedSeq[A]] =
    if (n < 0) None
    else if (n == 0) Some(p.take(3).flatMap(_.take(3)))
    else if (n == 1) Some(p.take(3).flatMap(_.drop(3).take(3)))
    else if (n == 2) Some(p.take(3).flatMap(_.drop(6)))
    else if (n == 3) Some(p.drop(3).take(3).flatMap(_.take(3)))
    else if (n == 4) Some(p.drop(3).take(3).flatMap(_.drop(3).take(3)))
    else if (n == 5) Some(p.drop(3).take(3).flatMap(_.drop(6)))
    else if (n == 6) Some(p.drop(6).flatMap(_.take(3)))
    else if (n == 7) Some(p.drop(6).flatMap(_.drop(3).take(3)))
    else if (n == 8) Some(p.drop(6).flatMap(_.drop(6)))
    else None
    
  /** Returns all columns of the puzzle. */
  def columns[A](p: Puzzle[A]): Option[Puzzle[A]] = {
    val seq = for (n <- (0 until 9)) yield nthColumn(p, n)
    sequence(seq)
  }
  
  /** Returns all submatrices of the puzzle. */
  def submatrices[A](p: Puzzle[A]): Option[Puzzle[A]] = {
    val seq = for (n <- (0 until 9)) yield nthSubmatrix(p, n)
    sequence(seq)
  }

  /** Checks if the given value is found in only one field in the same collection. */
  def onlyOneIn(seq: IndexedSeq[Set[Int]], v: Int): Boolean =
    seq.count(_.contains(v)) == 1

  private def checkSeq(os: Option[IndexedSeq[Set[Int]]], v: Int): Boolean =
    os.map(onlyOneIn(_, v)).getOrElse(false)
  
  /** Checks if the given value can be found in other fields in the same row. */
  def checkRow(p: Puzzle[Set[Int]], n: Int, v: Int): Boolean =
    checkSeq(nthRow(p, n), v)
  
  /** Checks if the given value can be found in other fields in the same column. */
  def checkColumn(p: Puzzle[Set[Int]], n: Int, v: Int): Boolean =
    checkSeq(nthColumn(p, n), v)
  
  /** Checks if the given value can be found in other fields in the same column. */
  def checkSubmatrix(p: Puzzle[Set[Int]], n: Int, v: Int): Boolean =
    checkSeq(nthSubmatrix(p, n), v)
  
  /**
   * Checks if the given value can be found in the other fields in the same
   * row, column and submatrix.
   */
  def checkValue(p: Puzzle[Set[Int]], r: Int, c: Int, v: Int): Boolean =
    whichSubmatrix(r, c).map { s =>
      checkRow(p, r, v) || checkColumn(p, c, v) || checkSubmatrix(p, s, v)
    }.getOrElse(false)

  /** Replaces a whole row of the puzzle. */
  def patchRow[A](p: Puzzle[A], n: Int, r: IndexedSeq[A]): Option[Puzzle[A]] =
    replaceNth(p, n, r)
  
  /** Replaces a whole column of the puzzle. */
  def patchColumn[A](p: Puzzle[A], n: Int, c: IndexedSeq[A]): Option[Puzzle[A]] =
    for {
      cs <- columns(p)
      rep <- replaceNth(cs, n, c)
      res <- columns(rep)
    } yield res
  
  /** Replaces a whole submatrix of the puzzle. */
  def patchSubmatrix[A](p: Puzzle[A], n: Int, s: IndexedSeq[A]): Option[Puzzle[A]] =
    for {
      subs <- submatrices(p)
      rep <- replaceNth(subs, n, s)
      res <- submatrices(rep)
    } yield res
  
  /**
   * Called when a field with only one potential digit is encountered.
   * The same value is then deleted from all sets in the same row, column
   * and submatrix.
   */
  def transformation1(p: Puzzle[Set[Int]], r: Int, c: Int, s: Set[Int]): Option[Puzzle[Set[Int]]] =
    for {
      nr <- nthRow(p, r)
      dr = deleteFrom(nr, s)
      pr <- patchRow(p, r, dr)
      nc <- nthColumn(pr, c)
      dc = deleteFrom(nc, s)
      pc <- patchColumn(pr, c, dc)
      sub <- whichSubmatrix(r, c)
      ns <- nthSubmatrix(pc, sub)
      ds = deleteFrom(ns, s)
      ps <- patchSubmatrix(pc, sub, ds)
    } yield ps

  /** Finds the value for which the second type of transformation will be carried out. */
  def findValue(p: Puzzle[Set[Int]], r: Int, c: Int, s: Set[Int]): Option[Int] = {
    val vals = for {
      v <- s.toStream if checkValue(p, r, c, v)
    } yield v
    vals.headOption
  }

  /**
   * Called when a field which can hold a potential value which is not found
   * in any other set in the same row, column or submatrix. That value replaces
   * the whole set.
   */
  def transformation2(p: Puzzle[Set[Int]], r: Int, c: Int, v: Int): Option[Puzzle[Set[Int]]] =
    for {
      row <- nthRow(p, r)
      nrow <- replaceNth(row, c, Set(v))
      prow <- patchRow(p, r, nrow)
    } yield prow

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
  def checkError(p: Puzzle[Set[Int]]): Boolean =
    (for {
      it <- Some(inverseTransform(p))
      cs <- columns(it)
      ss <- submatrices(it)
    } yield {
      isSolution(it) && (isErrorSolution(it) || isErrorSolution(cs) || isErrorSolution(ss))
    }).getOrElse(true)

  /** Checks if all of the sets in the puzzle have exactly one possible value. */
  def isSolved(p: Puzzle[Set[Int]]): Boolean =
    p.forall(_.forall(isSingle))
  

  /** Returns the set representing the possible values for the field. */
  def getField(p: Puzzle[Set[Int]], r: Int, c: Int): Option[Set[Int]] =
    for {
      nr <- nthRow(p, r)
      s <- Try(nr(c)).toOption
    } yield s
  
  private def chooseTransformation(p: Puzzle[Set[Int]], r: Int, c: Int, s: Set[Int]): Option[Puzzle[Set[Int]]] =
    if (isSingle(s)) transformation1(p, r, c, s)
    else findValue(p, r, c, s).fold(Option(p)) { v =>
      transformation2(p, r, c, v)
    }
  
  /** One iteration of the algorithm. */
  @annotation.tailrec
  def sudokuLoop(p: Puzzle[Set[Int]], r: Int, c: Int): Puzzle[Set[Int]] = {
    val next = nextField(r, c)
    val op = for {
      _ <- next
      s <- getField(p, r, c)
      t <- chooseTransformation(p, r, c, s)
    } yield t
    if (op.isEmpty) p
    else {
      val (nr, nc) = next.get
      sudokuLoop(op.get, nr, nc)
    }
  }

  /** Finds the first undecided field. */
  def findFirstNonSingle(p: Puzzle[Set[Int]]): (Int, Int, Set[Int]) = {
    @annotation.tailrec
    def loop(r: Int, c: Int): (Int, Int, Set[Int]) = {
      val op = for {
        s <- getField(p, r, c) if !isSingle(s)
      } yield (r, c, s)
      if (op.isDefined) op.get
      else {
        val (nr, nc) = nextField(r, c) getOrElse (-1, -1)
        loop(nr, nc)
      }
    }

    loop(0, 0)
  }

  /**
   * Implementation of the algorithm. When neither transformation1 nor
   * transformation2 can be carried out, it starts guessing numbers and
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
          t <- transformation2(p, r, c, v)
          o <- sudoku(t)
        } yield o
        res.headOption
      } else sudoku(np)
    }
  
  /** Solves a sudoku puzzle. */
  def solve(p: Puzzle[Int]): Puzzle[Int] = {
    val transformed = transform(p)
    inverseTransform(sudoku(transformed).get)
  }
  
  /** Shows the input and its solution on screen. */
  def visualSolve(p: Puzzle[Int]): Unit = {
    import Show._

    def printPuzzle(p: Puzzle[Int]): Unit =
      println(show(p))

    val s = solve(p)
    println("Input:")
    println()
    printPuzzle(p)
    println("Solution:")
    println()
    printPuzzle(s)
  }
}
