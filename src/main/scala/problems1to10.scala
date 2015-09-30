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
  def last[A](ls: List[A]):A =
    ls match {
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
  def penultimate[A](ls: List[A]):A =
    ls match {
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
  def nth[A](i: Int, ls: List[A]):A = {

    ( i, ls ) match {
      case (0, h::_)        => h
      case (n, _ :: tail )  => nth(n-1, tail)
      case (_, Nil)         => throw new NoSuchElementException
    }
//
//    ls.drop(i)
//      .headOption
//      .getOrElse(throw new NoSuchElementException)
  }

}

object P04 {

  /**
   * Find the number of ls of a list.
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
  def length[A](ls: List[A]):Int =
    ls match {
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
  def reverse[A](ls: List[A]):List[A] =
    ls match {
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
  def isPalindrome[A](ls: List[A]):Boolean =
    P05.reverse(ls) == ls

}

object P07 {

  /**
   * Flatten a nested list structure
   *
   * scala> P07.flatten(List(List(1, 1), 2, List(3, List(5, 8))))
   * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   *
   */
  def flatten(ls: List[Any]):List[Any] =
    ls.flatMap {
      case m: List[_] => flatten(m)
      case e          => List(e)
    }
}

object P08 {

  /**
   * Eliminate consecutive duplicates of list elements
   * If a list contains repeated elements they should be replaced with a single copy of the element.
   * The order of the elements should not be changed.
   *
   * scala> P08.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   *
   */
  def compress[A](ls: List[A]): List[A] =
    ls match {
      case h :: Nil                   => List(h)
      case h :: ys :: tail if h == ys => compress( h :: tail )
      case h :: ys :: tail            => h :: compress( ys :: tail )
    }
}

object P09 {

  /**
   * Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   *
   * scala> P09.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
   *
   */
  def pack[A](ls: List[A]):List[List[A]] =
    if(ls.isEmpty) Nil
    else
      ls.span( _ == ls.head ) match {
        case (packed, Nil) => List(packed)
        case (packed, next) => packed :: pack(next)
      }

}

object P10 {

  /**
   * Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
   * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
   *
   * scala> P10.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   *
   */
  def encode[A](ls: List[A]):List[(Int, A)] =
    P09.pack(ls).map( s => (P04.length(s), s.head) )

}

object P11 {

  /**
   * Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
   * Only elements with duplicates are transferred as (N, E) terms.
   *
   * scala> P11.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   *
   */
  def encodeModified[A](ls: List[A]):List[Any] =
    P10.encode(ls).map{ s =>
      if(s._1 == 1) s._2
      else          s
    }

}

object P12 {

  /**
   * Decode a run-length encoded list.
   * Given a run-length code list generated as specified in problem P10,
   * construct its uncompressed version.
   *
   * scala> P12.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
   * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   *
   */
  def decode[A](ls: List[(Int, A)]):List[A] =
    ls match {
      case Nil              => Nil
      case (n, e) :: tail   => List.fill(n)(e) ++ decode(tail)
    }

}

object P13 {

  /**
   * Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method directly.
   * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
   *
   * scala> P13.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   *
   */
  def encodeDirect[A](ls: List[A]):List[(Int, A)] =
    if(ls.isEmpty) Nil
    else {
      val (packed, next) = ls.span(_ == ls.head)
      (packed.length, packed.head) :: encodeDirect(next)
    }
}

object P14 {

  /**
   * Duplicate the elements of a list.
   *
   * scala> P14.duplicate(List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   *
   */
  def duplicate[A](ls: List[A]):List[A] =
    ls match {
      case Nil => Nil
      case h :: tail => h :: h :: duplicate(tail)
    }
}

object P15 {

  /**
   * Duplicate the elements of a list a given number of times.
   *
   * scala> P15.duplicateN(3, List('a, 'b, 'c, 'c, 'd))
   * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   *
   */
  def duplicateN[A](n: Int, ls: List[A]):List[A] =
    ls match {
      case Nil        => Nil
      case h :: tail  => List.fill(n)(h) ++ duplicateN(n, tail)
    }

}

object P16 {

  /**
   * Drop every Nth element from a list.
   *
   * scala> P16.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   *
   */
  def drop[A](n: Int, ls: List[A]):List[A] =
    ls.zipWithIndex.filter{ s => ((s._2 + 1) % n ) != 0 }.map( _._1 )

}

object P17 {

  /**
   * Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   *
   * scala> P17.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   *
   */
  def split[A](n: Int, ls: List[A]):(List[A], List[A]) =
    (ls.take(n), ls.drop(n))

}

object P18 {

  /**
   * Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to
   * but not including the Kth element of the original list. Start counting the elements with 0.
   *
   * scala> P18.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * res0: List[Symbol] = List('d, 'e, 'f, 'g)
   *
   */
  def slice[A](i: Int, k: Int, ls: List[A]): List[A] =
    ls drop i take( k - i )
}

//object P19 {
//
//  /**
//   * Rotate a list N places to the left.
//   *
//   * scala> P19.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//   * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
//   *
//   * scala> P19.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//   * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
//   *
//   */
//  def rotate[A](n: Int, ls: List[A]):List[A] =
//    ???
//
//
//}