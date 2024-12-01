package day01

import scala.io.Source

val source = Source.fromResource("day01.in")
val input: Seq[(Int, Int)] = source.getLines().map { case s"${one}   ${two}" => (one.toInt, two.toInt) }.toIndexedSeq

val list1 = input.map(_._1)
val list2 = input.map(_._2)

def difference(a: Int, b: Int): Int = Math.abs(a - b)

def solve1(a: Seq[Int], b: Seq[Int]): Int =
    a.sorted.lazyZip(b.sorted).map(difference.tupled).sum

def solve2(a: Seq[Int], b: Seq[Int]): Int =
    // Instead of having to calculate x * occurrences(x) and having to calculate the occurrences by summing 1's,
    // we can precompute the score values directly by summing the values themselves.
    val scoreValues = b.groupMapReduce(identity)(identity)(Integer.sum).withDefaultValue(0)
    a.map(scoreValues).sum

@main def main(): Unit = {

    val result1 = solve1(list1, list2)
    println(result1)

    val result2 = solve2(list1, list2)
    println(result2)

}