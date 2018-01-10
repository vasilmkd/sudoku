object Traverse {
  private def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- oa
      b <- ob
    } yield f(a, b)

  def traverse[A, B](seq: IndexedSeq[A])(f: A => Option[B]): Option[IndexedSeq[B]] =
    seq.foldLeft(Option(IndexedSeq[B]()))((acc, a) => map2(acc, f(a))(_ :+ _))
  
  def sequence[A](seq: IndexedSeq[Option[A]]): Option[IndexedSeq[A]] =
    traverse(seq)(oa => oa)
}
