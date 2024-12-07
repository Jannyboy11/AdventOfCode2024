package day07

import scala.annotation.tailrec
import scala.io.Source

val testInput = """190: 10 19
                  |3267: 81 40 27
                  |83: 17 5
                  |156: 15 6
                  |7290: 6 8 6 15
                  |161011: 16 10 13
                  |192: 17 8 14
                  |21037: 9 7 18 13
                  |292: 11 6 16 20""".stripMargin

val source = Source.fromResource("day07.in")
val input: Seq[Equation] = source.getLines().map {
    case s"${testValue}: ${operands}" => (testValue.toLong, operands.split(" ").map(_.toLong).toSeq)
}.toSeq

type TestValue = Long
type Operand = Long
type Equation = (TestValue, Seq[Operand])

enum Operator:
    case Add
    case Multiply
    case Concatenate

    def apply(one: Operand, two: Operand): Long = this match
        case Add => one + two
        case Multiply => one * two
        case Concatenate => one * 10 * orderOfMagnitude(two) + two

def orderOfMagnitude(operand: Operand): Long =
    if operand >= 10 then 10 * orderOfMagnitude(operand / 10) else 1L

// Could cache these results for any given size. Unfortunately, Java does not have StableValues yet, and I'm too lazy to do my own memoisation
def operatorCombinations1(size: Int): Seq[Seq[Operator]] = {
    if size == 0 then
        LazyList(LazyList.empty)
    else
        val tails = operatorCombinations1(size - 1)
        tails.map(tail => Operator.Add +: tail) ++ tails.map(tail => Operator.Multiply +: tail)
    end if
}

def evaluate(operands: Seq[Operand], operators: Seq[Operator]): Long =
    @tailrec def evaluate(acc: Operand, operators: Seq[Operator], operands: Seq[Operand]): Long =
        if operands.isEmpty then acc
        else evaluate(operators.head.apply(acc, operands.head), operators.tail, operands.tail)
    evaluate(operands.head, operators, operands.tail)

def isSolvable1(equation: Equation): Boolean =
    val (testValue, operands) = equation
    operatorCombinations1(operands.size - 1)
        .exists(operators => evaluate(operands, operators) == testValue)

def calibrationResult1(equations: Seq[Equation]): Long =
    equations.filter(isSolvable1).map { case (testValue, _) => testValue }.sum

def operatorCombinations2(size: Int): Seq[Seq[Operator]] = {
    if size == 0 then
        LazyList(LazyList.empty)
    else
        val tails = operatorCombinations1(size - 1)
        tails.map(tail => Operator.Add +: tail) ++
            tails.map(tail => Operator.Multiply +: tail) ++
            tails.map(tail => Operator.Concatenate +: tail)
    end if
}

def isSolvable2(equation: Equation): Boolean =
    val (testValue, operands) = equation
    operatorCombinations2(operands.size - 1)
        .exists(operators => evaluate(operands, operators) == testValue)

def calibrationResult2(equations: Seq[Equation]): Long =
    equations.filter(isSolvable2).map { case (testValue, _) => testValue }.sum

@main def main(): Unit = {

    val result1 = calibrationResult1(input)
    println(result1)

    val result2 = calibrationResult2(input)
    println(result2) //15916264547402 too low

}