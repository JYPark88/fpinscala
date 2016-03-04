import scala.annotation.tailrec

/**
  * Created by jypark on 2016. 2. 7..
  */
object Ex02_02 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n == as.length) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    }

    loop(0)
  }
}
