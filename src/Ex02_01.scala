import scala.annotation.tailrec

/**
  * Created by jypark on 2016. 2. 7..
  */
object Ex02_01 {
  def factorial(n: Int) : Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  private def formatFactorial(x: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(x, factorial(x))
  }

  def fib(n: Int) : Int = {
    @tailrec
    def go(n: Int, prev: Int = 0, prevprev: Int = 1): Int = n match {
      case 0 => prevprev
      case 1 => prev
      case _ => go(n-1, prevprev, prev + prevprev)
    }

    go(n)
  }

  private def formatFib(x: Int) = {
    val msg = "The fibonacci of %d is %d."
    msg.format(x, fib(x))
  }


  def main(args: Array[String]) {
    println(formatFactorial(3))
    println(formatFib(7))
  }
}
