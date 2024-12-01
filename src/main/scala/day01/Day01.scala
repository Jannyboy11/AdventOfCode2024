package day01

import scala.io.Source

val source = Source.fromResource("day01.in")
val input: Seq[(Int, Int)] = source.getLines().map { case s"${one}   ${two}" => (one.toInt, two.toInt) }.toSeq

val list1 = input.map(_._1)
val list2 = input.map(_._2)

def distance(a: Int, b: Int): Int = Math.abs(a - b)

def totalDistance(a: Seq[Int], b: Seq[Int]): Int =
    a.sorted.lazyZip(b.sorted).map(distance.tupled).sum

def similarityScore(a: Seq[Int], b: Seq[Int]): Int =
    // Instead of having to calculate x * occurrences(x) and having to calculate the occurrences by summing 1s,
    // we can precompute the score values directly by summing the values themselves. This saves us on multiplications.
    // We use the following equality: x * (1 + 1 + ... + 1) == x + x + ... + x.
    val scoreValues = b.groupMapReduce(identity)(identity)(Integer.sum).withDefaultValue(0)
    a.map(scoreValues).sum

@main def main(): Unit = {

    val result1 = totalDistance(list1, list2)
    println(result1)

    val result2 = similarityScore(list1, list2)
    println(result2)

}