package puzzles.bridge

import scala.annotation.tailrec
import scala.math._
import scala.annotation.tailrec
import util.Random.nextInt

object BridgeTraversalProblem extends App {

  //@tailrec
  def reduce(isOnLeft: Boolean, left: List[Int], right: List[Int], acc: Int): Int = {
    (isOnLeft, left) match {
      case (false, one :: two :: List()) if (right.isEmpty) =>
        Int.MaxValue
      case (true, one :: two :: List()) =>
        acc + max(one, two)
      case (cond, _) =>
        if (cond)
          (left.combinations(2) map { c => reduce(false, left diff c, right ++ c, acc + c.max) }).min
        else
          (right.combinations(1) map { c => reduce(true, left ++ c, right diff c, acc + c.head) }).min
    }
  }

  def reduceWithPath(isOnLeft: Boolean, left: List[Int], right: List[Int], acc: Int, path: (List[List[Int]], List[List[Int]])): Int = {
    (isOnLeft, left) match {
      case (false, one :: two :: List()) if (right.isEmpty) =>
        Int.MaxValue
      case (true, one :: two :: List()) =>
        val result = acc + max(one, two)
        val l = path._1.zip(path._2)
        if (result <= 432) println("PATH: [" + result + "], result:\n" + l.mkString("\n"))
        result
      case (cond, _) =>
        if (cond)
          (left.combinations(2) map {
            c =>
              val newLeft = left diff c
              val newRight = right ++ c
              val newPath = (path._1 :+ newLeft, path._2 :+ newRight)
              reduceWithPath(false, newLeft, newRight, acc + c.max, newPath)
          }).min
        else
          (right.combinations(1) map {
            c =>
              val newLeft = left ++ c
              val newRight = right diff c
              val newPath = (path._1 :+ newLeft, path._2 :+ newRight)
              reduceWithPath(true, newLeft, newRight, acc + c.head, newPath)
          }).min
    }
  }

  // TODO: optimize
  def exhaustive(isOnLeft: Boolean, left: List[Int], right: List[Int], acc: Int): Int = {
    (isOnLeft, left) match {
      case (false, one :: two :: List()) if (right.isEmpty) =>
        Int.MaxValue
      case (true, one :: two :: List()) =>
        acc + max(one, two)
      case (cond, _) =>
        if (cond)
          (left.combinations(2) map { c => exhaustive(false, left diff c, right ++ c, acc + c.max) }).min
        else
          (right.combinations(1) map { c => exhaustive(true, left ++ c, right diff c, acc + c.head) }).min
    }
  }

  @tailrec
  def reduceNabil(left: List[Int], right: List[Int], acc: Int): Int = {
    if (left.size <= 2) {
      acc + left.max
    } else if (left.size % 2 == 0) {
      val firstMin = left.min
      val secondMin = (left diff List(firstMin)).min
      val goners = List(firstMin, secondMin)
      val passed = right ++ goners
      val returned = passed.min
      reduceNabil(returned :: (left diff goners), passed diff List(returned), acc + secondMin + returned)
    } else {
      val firstMax = left.max
      val secondMax = (left diff List(firstMax)).max
      val goners = List(firstMax, secondMax)
      val passed = right ++ goners
      val returned = passed.min
      reduceNabil(returned :: (left diff goners), passed diff List(returned), acc + firstMax + returned)
    }
  }

  val ngroup = 2

  def callAlex(people: List[Int]): Int = {
    if (people.length <= ngroup) {
      people.max
    } else {
      val sortedPeople = people.sorted
      alex(dropIndex(sortedPeople, 1), List(sortedPeople(1)), sortedPeople.take(2).sum)
    }
  }

  @tailrec
  def alex(left: List[Int], right: List[Int], acc: Int): Int = {
//    println("left: " + left + ", right: " + right + ", acc: " + acc)
    if (left.length <= ngroup) { // last go
      left.max + acc
    } else if (left.length % 2 == 0) { // even - take min 2
      alex(dropIndex(left, 1), left(1) :: right, acc + left.take(2).sum)
    } else { // odd - take max 2
      val newRem = left dropRight (ngroup)
      val lastTwo = left.takeRight(2)
      val minRight = right.min
      alex(insertIndex(newRem, 1, minRight), removeFirst(lastTwo ::: right)(_ == minRight), acc + lastTwo.max + minRight)
    }
  }

  /**
   * Drops the 'i'th element of a list.
   */
  def dropIndex[T](xs: List[T], n: Int) = {
    val (l1, l2) = xs splitAt n
    l1 ::: (l2 drop 1)
  }

  def replaceIndex[T](xs: List[T], n: Int, elem: T) = {
    val (l1, l2) = xs splitAt n
    l1 ::: List(elem) ::: (l2 drop 1)
  }

  def insertIndex[T](xs: List[T], n: Int, elem: T) = {
    val (l1, l2) = xs splitAt n
    l1 ::: List(elem) ::: l2
  }

  def removeFirst[T](list: List[T])(pred: (T) => Boolean): List[T] = {
    val (before, atAndAfter) = list span (x => !pred(x))
    before ::: atAndAfter.drop(1)
  }

  //val people = List(1, 2, 5, 10, 20) // solution is 33
  //val people = List(1, 2, 5, 10) // solution is 17
//      val people = (1 until 15000).toList // 56287492
  //val people = List(1, 2, 3, 4, 5)
//  //val people = List(66, 47, 75, 80, 70) // broken one
  
// val people = List(3, 47, 63, 42, 67)//List(42, 88, 52, 47, 15, 93, 48) //fail 2

    val nPeople = 12
    
    val people = Stream.continually(nextInt(100)).take(nPeople).toList

  println("For these numbers: [%s]", people)

    val firstMin = people.min
    val secondMin = (people diff List(firstMin)).min
    val goners = List(firstMin, secondMin)
    println("NABIL:      the solution is: [%d]".format(reduceNabil(people diff List(secondMin), List(secondMin), goners.sum)))

  println("EXHAUSTIVE: the solution is: [%d]".format(reduce(true, people, List(), 0)))

  println("ALEX:       the solution is: [%d]".format(callAlex(people)))

// println("EXHAUSTIVE PATH: the solution is: [%d]".format(reduceWithPath(true, people, List(), 0, (List(), List()))))

}