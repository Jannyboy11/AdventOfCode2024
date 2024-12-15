## Advent of Code 2024, Day 5

### Introduction

Day 5's puzzle starts off by introducing the puzzle input. It consists of two parts: 1) A set of page ordering rules, and 2) a set of updates.
1. Each page ordering rule is a pair of page numbers.
2. Each update is a sequence of page numbers.

Mathematically speaking, the page numbers form a [Partial Order](https://en.wikipedia.org/wiki/Partially_ordered_set).
We can compare any possible pair of pages, and determine whether one precedes the other.
It's also possible the ordering rules don't enforce any ordering between two page number; we consider them equal in such case.

### Implementation

The programming language of choice for my solutions this year is [Scala](https://www.scala-lang.org/); I like it for its expressiveness, rich type system, performance and support for multiple programming styles.
Let us see some domain modelling!
```scala 3
type PageNumber = Int
type Update = Seq[PageNumber]
type Ordering = (PageNumber, PageNumber)
```
I don't think this snippet needs much explanation. We define some types using type aliases.
`Seq[T]` is a sequence of elements of type `T`, and `(X, Y)` is a tuple type with element types `X` and `Y`.

Next up, we have to parse the input. The puzzle input consists of a list of ordering rules, followed by a blank line, followed by a list of updates. 
I didn't try to do anything smart or fancy here; my parser just tracks a boolean variable which indicates whether we have seen the blank line yet - and if so, we parse the line as an update instead of as an ordering rule.
```scala 3
import scala.io.Source

val source = Source.fromResource("day05.in")
val (orderings, updates) = parseInput(source.getLines())

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
```
And there we go; the parseInput routine takes in the lines of the input file, and produces a sequence of ordering rules, and a sequence of updates.

#### Part 1

Next, we get to part 1 of the puzzle. We have to determine which updates are correctly ordered.
To do this efficiently, I calculated the entire partial order as a [Directed Acyclic Graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) (DAG).
From this DAG, we can easily query which page numbers comes after another page number.

In Scala, I represent this DAG as a `Map` of key-value pairs.
In this instance, a pair consists of a key which is a page number, and a value which is a set of page numbers which must come _after_ the key according to the ordering rules.
Since I like descriptive naming, I define another type alias:
```scala 3
type Order = Map[PageNumber, Set[PageNumber]]
```

Then, we build up this order using the ordering rules which we got from the puzzle input:
```scala 3
def buildOrder(orderings: Seq[Ordering]): Order =
    orderings.groupMapReduce((before, _) => before)((_, after) => Set(after))(_ union _)
```
Whoah, that's some nice higher order function wizardry going on! Let's break it down..
- `groupMapReduce` is a method on the `Seq` type with three single-parameter parameter lists. (Yes, that's right, in Scala methods can have multiple parameter lists.)
- The argument to the first parameter list is the _key extractor_: a function that, when given an element from the sequence, convert it a key value for the result Map.
We just take the first value from the ordering rule here (it precedes the second value).
- The argument to the second parameter list is the _value extractor_: a function that, when given an element from the sequence, convert it a value for the result Map.
We just take the second value from the ordering rule here (it succeeds the first value), and put it in a (singleton) Set.
- The argument to the third parameter list is the _value combiner_: This function is called when two values need to be combined for the same key.
Here we just [union](https://en.wikipedia.org/wiki/Union_(set_theory)) two sets together. Instead of writing `(set1, set2) => set1.union(set2)` we make use of some nice syntactic sugar here.

The result is exactly what we want: a Map where each pair contains a page number as the key, and its successors (according to the ordering rules) as the value.

To answer whether an update is correctly ordered, we will first write a routine which answers whether two numbers are correctly ordered.
```scala 3
def isInOrder(one: PageNumber, two: PageNumber, order: Order): Boolean = order.get(two) match
    case None => true
    case Some(set) => !set.contains(one)
```
`order(two)` returns an `Option[Set[PageNumber]]`. `Option` is an [Algebraic Data Type](https://en.wikipedia.org/wiki/Algebraic_data_type) with only two cases: Some and None.
Some denotes that some value is present, and None denotes that no value is present. We use pattern matching to find out which case we're dealing with.
`one` and `two` will be numbers from the update sequence, where `one` is written before `two` in the sequence. Then we consider them correctly ordered if `one` is _not after_ `two` in the order DAG.

Then, to determine whether an update sequence is correctly ordered, we have the following:
```scala 3
def isInOrder(update: Update, order: Order): Boolean = update match
    case Seq() => true
    case head +: tail => isInOrder(tail, order) && tail.forall(tailElem => isInOrder(head, tailElem, order))
```
This procedure is recursive, and again we use pattern matching here to distinguish between two cases of updates:
1. The empty case: an empty update sequence is always ordered.
2. The non-empty case: We split the sequence into its head (=first) element and tail (=remaining) elements. Then, the sequence is ordered if all tail elements are ordered _after_ the head element, and the tail itself is also ordered.

Then, we can finally get our answer to the first puzzle:
```scala 3
def middleElement(update: Update): PageNumber = update(update.size / 2)

val order = buildOrder(orderings)
val result1 = updates.filter(isInOrder(_, order)).map(middleElement).sum
println(result1)
```

#### Part 2

Instead of retrieving the correctly ordered updates, we are now tasked with getting the incorrectly ordered updates.
Then we have to order them correctly, and sum the middle values again. We already have all the logic available, except for one piece: ordering the unordered updates correctly.

To sort sequences using some custom ordering, Scala provides us with three easy options: `Seq.sorted`, `Seq.sortBy` and `Seq.sortWith`.
The difference between the first two options and the last once, is the first two options take a `scala.math.Ordering` instance as an argument,
while the last one takes a comparison function "less than". I opted for the `sortWith` option, since or `ordering` DAG makes it easy to check whether a page number preceeds another one.
```scala 3
def fixUpdateOrdering(update: Update, order: Order): Update =
    update.sortWith((one, two) => order.get(one) match { case None => true; case Some(set) => set.contains(two) })
```
And there we go. Given two items, we consider the first one smaller than the second item if the second item is in the successor set. If there is no successor set, or the second item is not contained in the successor set, then it must be equal to or greater than the first item.

And with that, we obtain our answer for the second puzzle:
```scala 3
val result2 = updates.filter(!isInOrder(_, order)).map(fixUpdateOrdering(_, order)).map(middleElement).sum
println(result2)
```
Cheers!