package day05

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day05.in")
val (orderings, updates) = parseInput(source.getLines())

type Update = Seq[Int]
type Ordering = (Int, Int)

def parseInput(input: Iterator[String]): (Seq[Ordering], Seq[Update]) = {
    val orderings = Seq.newBuilder[Ordering]
    val updates = Seq.newBuilder[Update]
    var readingUpdates = false
    for line <- input do
        if line.isEmpty then readingUpdates = true
        else if readingUpdates then updates.addOne(line.split(",").map(_.toInt))
        else orderings.addOne({ val Array(before, after) = line.split("\\|"); (before.toInt, after.toInt) })
    end for
    (orderings.result(), updates.result())
}

def middleElement(update: Update): Int = update(update.size / 2)

type Order = Map[Int, Set[Int]]

def buildOrder(orderings: Seq[Ordering]): Order = {
    val res = mutable.Map.empty[Int, Set[Int]]
    for (before, after) <- orderings do
        res.updateWith(before) {
            case None => Some(Set(after))
            case Some(values) => Some(values + after)
        }
    end for
    res.toMap
}

def isInOrder(one: Int, two: Int, order: Order): Boolean = order.get(two) match
    case None => true
    case Some(set) => !set.contains(one)

def isInOrder(update: Update, order: Order): Boolean = update match
    case Seq() => true
    case h +: tail => isInOrder(tail, order) && tail.forall(afterElem => isInOrder(h, afterElem, order))

def fixUpdateOrdering(update: Update, order: Order): Update =
    update.sortWith((one, two) => order.get(one) match { case None => true; case Some(set) => set.contains(two) })

@main def main(): Unit = {

    val order = buildOrder(orderings)

    val result1 = updates.filter(isInOrder(_, order)).map(middleElement).sum
    println(result1)

    val result2 = updates.filter(!isInOrder(_, order)).map(fixUpdateOrdering(_, order)).map(middleElement).sum
    println(result2)

}