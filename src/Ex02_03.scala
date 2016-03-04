/**
  * Created by jypark on 2016. 2. 8..
  */
object Ex02_03 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)
}
