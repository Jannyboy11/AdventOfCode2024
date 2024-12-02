package day02

import scala.io.Source

val source = Source.fromResource("day02.in")
val input: Seq[Report] = source.getLines().map(s => s.split(" ").map(l => l.toInt).toSeq).toSeq

type Report = Seq[Level]
type Level = Int

def difference(a: Int, b: Int): Int = Math.abs(a - b)
def isSafeDifference(difference: Int): Boolean = 1 <= difference && difference <= 3

def isSafe1(report: Report): Boolean =
    val pairs = report.lazyZip(report.tail)
    val safeDifferences = pairs.forall((l1, l2) => isSafeDifference(difference(l1, l2)))
    val hasOrdering = pairs.forall(_ > _) || pairs.forall(_ < _)
    hasOrdering && safeDifferences

def dampen(report: Report): Seq[Report] =
    for i <- LazyList.from(report.indices) yield report.patch(i, Seq(), 1)

def isSafe2(report: Report): Boolean =
    isSafe1(report) || dampen(report).exists(isSafe1)

@main def main(): Unit = {

    val result1 = input.count(isSafe1)
    println(result1)

    val result2 = input.count(isSafe2)
    println(result2)

}