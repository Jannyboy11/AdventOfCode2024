package technolution

enum Instruction:
    case IncrementValue
    case DecrementValue
    case JumpToEndOfBlockIfZero
    case JumpToBeginningOfBlockIfNonZero
    case IncrementPointer
    case DecrementPointer
    case Output
    case Input
import Instruction.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

val code1 = "PPPPPSRPPPPPPPPPPTFWRPPLFFL"

// waarschijnlijk: S=[, en (T of W)=]
// waarschijnlijk: P=increment memory value
// waarschijnlijk: R=increment pointer

// wat ook mogelijk is: R=[ en L=]

val code2 = "PPPPPPPSRPPPPPPTFWRPPPPPPPLPPPPPPPPLFLFL"

type Interpretation = Map[Char, Instruction]
type Translation = Char => Instruction

val characters = "PSRTFWL"
val instructions = List(IncrementValue, DecrementValue, JumpToEndOfBlockIfZero, JumpToBeginningOfBlockIfNonZero, IncrementPointer, DecrementPointer, Output, Input)

val interpretations: Seq[Interpretation] = characters.permutations
    .map(wrappedString => wrappedString.zip(instructions).toMap)
    .toSeq

type Program = IndexedSeq[Instruction]

def calculateJumpTargets(program: Program): Option[Map[Int, Int]] = {
    val resultBuilder = Map.newBuilder[Int, Int]
    val stack = new mutable.Stack[Int]()
    var i = 0
    while i < program.length do
        program(i) match
            case JumpToEndOfBlockIfZero =>
                stack.push(i)
            case JumpToBeginningOfBlockIfNonZero =>
                if stack.isEmpty then
                    return None
                end if

                val beginning = stack.pop()
                resultBuilder.addOne(beginning -> i)
                resultBuilder.addOne(i -> beginning)
            case _ =>
        i += 1
    end while

    if stack.isEmpty then
        Some(resultBuilder.result())
    else
        None
}

def run(program: Program): Option[List[Char]] = {

    val result = new ListBuffer[Char]

    val jumpTargets = calculateJumpTargets(program) match
        case Some(targets) => targets
        case None => return None

    var programCounter = 0
    val memory = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var pointer = 0

    while programCounter < program.length do
        program(programCounter) match
            case IncrementValue => memory.put(pointer, memory(pointer) + 1)
            case DecrementValue => memory.put(pointer, memory(pointer) - 1)
            case JumpToEndOfBlockIfZero => if memory(pointer) == 0 then programCounter = jumpTargets(programCounter)
            case JumpToBeginningOfBlockIfNonZero => if memory(pointer) != 0 then programCounter = jumpTargets(programCounter)
            case IncrementPointer => pointer += 1
            case DecrementPointer => pointer -= 1
            case Output =>
                // TODO remove shortcut for part 1.
                if result.length == 2 then
                    return None

                val value = memory(pointer).toChar // proper brainfuck uses ascii encoding.
                result.append(value)
        end match
        programCounter += 1
    end while

    Some(result.toList)
}

@main
def part1(): Unit = {

    for interpretation <- interpretations do
        println(s"Trying interpretation ${interpretation}")

        val interpP = interpretation('P')
        val interpL = interpretation('L')
        if interpP != JumpToEndOfBlockIfZero &&
            interpP != JumpToBeginningOfBlockIfNonZero &&
            interpL != JumpToEndOfBlockIfZero then

            val program1: Program = code1.map(interpretation)
            println(s"program1=$program1")

            val program1Result = run(program1)
            println(program1Result)
            program1Result match
                case None => //skip
                case Some(p1res) =>
                    if p1res.size == 2 && p1res.head - 2 == p1res.last then
                        println(p1res)

                        val program2 = code2.map(interpretation)
                        println(run(program2))
                    end if
            end match
        end if
    end for

}