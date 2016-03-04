/**
  * Created by jypark on 2016. 2. 8..
  */
object Ex02_05 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
