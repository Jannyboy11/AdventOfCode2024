package day03

import fastparse.*
import fastparse.NoWhitespace.*
import fastparse.given

import scala.io.Source

val source = Source.fromResource("day03.in")
val input = source.mkString

enum Instruction:
    case Mul(a: Int, b: Int)
    case Do
    case Dont
import Instruction.*

def parseNum[$: P]: P[Int] =
    P(CharIn("0-9").rep(min=1, max=3).!.map(_.toInt))

def parseMul[$: P]: P[Mul] =
    P("mul(" ~ parseNum ~ "," ~ parseNum ~ ")").map { case (x, y) => Mul(x, y) }

def parseDo[$: P]: P[Do.type] =
    P("do()").!.map(_ => Do)

def parseDont[$: P]: P[Dont.type] =
    P("don't()").!.map(_ => Dont)

// part 1
def parseMaybeMul[$: P]: P[Mul | Unit] =
    parseMul | P(AnyChar)

def parseMultiplications[$: P]: P[Seq[Mul]] =
    parseMaybeMul.rep.map(seq => seq.collect { case x: Mul => x })

def inputMultiplications: Seq[Mul] =
    val Parsed.Success(pairs, _) = parse(input, parseMultiplications): @unchecked
    pairs

def solve1(multiplications: Seq[Mul]): Int = multiplications.map { case Mul(x, y) => x * y }.sum

// part 2
def parseInstruction[$: P]: P[Instruction | Unit] =
    parseMul | parseDo | parseDont | P(AnyChar)

def parseInstrunctions[$: P]: P[Seq[Instruction]] =
    parseInstruction.rep.map(seq => seq.collect { case x: Instruction => x })

def inputInstructions: Seq[Instruction] =
    val Parsed.Success(instructions, _) = parse(input, parseInstrunctions): @unchecked
    instructions

def solve2(instructions: Seq[Instruction]): Int =
    var enabled = true
    var sum = 0
    for ins <- instructions do
        ins match
            case Do => enabled = true
            case Dont => enabled = false
            case Mul(x, y) => if enabled then sum += x * y
    sum

@main def main(): Unit = {

    val result1 = solve1(inputMultiplications)
    println(result1)

    val result2 = solve2(inputInstructions)
    println(result2)

}