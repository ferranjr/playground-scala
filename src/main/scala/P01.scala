import scala.annotation.tailrec

object P01 {

  /**
   * Find the last element of a list.
   *
   * scala> P01.last(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 8
   *
   * scala> P01.last(List("foo"))
   * res0: String = foo
   *
   */
  def last[A](elements: List[A]):A =
    elements match {
      case h :: Nil   => h
      case h :: t     => last(t)
      case _          => throw new NoSuchElementException
    }
}


object P02 {

  /**
   * Find the last but one element of a list.
   *
   * scala> P02.penultimate(List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   *
   * scala> P02.penultimate(List('a', 'b'))
   * res0: Char = a
   *
   */
  def penultimate[A](elements: List[A]):A =
    elements match {
      case h :: a :: Nil  => h
      case _ :: tail      => penultimate(tail)
      case _              => throw new NoSuchElementException
    }

}


object P03 {

  /**
   * Find the Kth element of a list
   *
   * scala> P03.nth(2, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 2
   *
   * scala> P03.nth(4, List(1, 1, 2, 3, 5, 8))
   * res0: Int = 5
   *
   */
  def nth[A](i: Int, elements: List[A]):A = {

    ( i, elements ) match {
      case (0, h::_)        => h
      case (n, _ :: tail )  => nth(n-1, tail)
      case (_, Nil)         => throw new NoSuchElementException
    }
//
//    elements.drop(i)
//      .headOption
//      .getOrElse(throw new NoSuchElementException)
  }

}

object P04 {

  /**
   * Find the number of elements of a list.
   *
   * scala> P04.length(Nil)
   * res0: Int = 0
   *
   * scala> P04.length(List(1))
   * res0: Int = 1
   *
   * scala> P04.length(List(1,2,3,4))
   * res0: Int = 4
   *
   */
  def length[A](elements: List[A]):Int =
    elements match {
      case Nil        => 0
      case h :: Nil   => 1
      case h :: tail  => 1 + length(tail)
    }
}

object P05 {

  /**
   * Reverse a list
   *
   * scala> P05.reverse(List())
   * res0: List[Nothing] = List()
   *
   * scala> P05.reverse(List(8))
   * res0: List[Int] = List(8)
   *
   * scala> P05.reverse(List(1, 1, 2, 3, 5, 8))
   * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   *
   */
  def reverse[A](elements: List[A]):List[A] =
    elements match {
      case Nil        => Nil
      case h :: Nil   => List(h)
      case h :: tail  => reverse(tail) :+ h
    }
}

object P06 {

  /**
   * Find out whether a list is a palindrome
   *
   * scala> P06.isPalindrome(List(1, 2, 3, 3, 1))
   * res0: Boolean = false
   *
   * scala> P06.isPalindrome(List(1, 2, 3, 2, 1))
   * res0: Boolean = true
   *
   */
  def isPalindrome[A](elements: List[A]):Boolean =
    P05.reverse(elements) == elements

}