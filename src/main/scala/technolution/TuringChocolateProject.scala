// puzzles found at: https://www.technolution.com/career/advent-of-code-2024/
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
    case Unknown
import Instruction.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.boundary
import scala.util.boundary.break

val part1code1 = "PPPPPSRPPPPPPPPPPTFWRPPLFFL"
val part1code2 = "PPPPPPPSRPPPPPPTFWRPPPPPPPLPPPPPPPPLFLFL"

type Interpretation = Map[Char, Instruction]
type Translation = Char => Instruction

val characters1 = "PSRTFWL"
val instructions1 = List(IncrementValue, DecrementValue, JumpToEndOfBlockIfZero, JumpToBeginningOfBlockIfNonZero, IncrementPointer, DecrementPointer, Output)

val interpretations1: Seq[Interpretation] = characters1.permutations
    .map(wrappedString => wrappedString.zip(instructions1).toMap)
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

def run1Code1(program: Program): Option[List[Char]] = {

    val start = System.currentTimeMillis()

    val result = new ListBuffer[Char]

    val jumpTargets = calculateJumpTargets(program) match
        case Some(targets) => targets
        case None => return None

    var programCounter = 0
    val memory = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var pointer = 0

    while programCounter < program.length do
        val currently = System.currentTimeMillis()
        if currently - start > 10_000 then
            // 10 seconds passed, just give up.
            return None

        program(programCounter) match
            case IncrementValue => memory.put(pointer, memory(pointer) + 1)
            case DecrementValue => memory.put(pointer, memory(pointer) - 1)
            case JumpToEndOfBlockIfZero => if memory(pointer) == 0 then programCounter = jumpTargets(programCounter)
            case JumpToBeginningOfBlockIfNonZero => if memory(pointer) != 0 then programCounter = jumpTargets(programCounter)
            case IncrementPointer => pointer += 1
            case DecrementPointer => pointer -= 1
            case Output =>
                if result.length == 2 then
                    // short-circuit if we pass 42
                    return None

                val value = memory(pointer).toChar // proper brainfuck uses ascii encoding.
                result.append(value)
        end match
        programCounter += 1
    end while

    Some(result.toList)
}

def run1Code2(program: Program): Option[List[Char]] = {

    val start = System.currentTimeMillis()

    val result = new ListBuffer[Char]

    val jumpTargets = calculateJumpTargets(program) match
        case Some(targets) => targets
        case None => return None

    var programCounter = 0
    val memory = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var pointer = 0

    while programCounter < program.length do
        val currently = System.currentTimeMillis()
        if currently - start > 10_000 then
            return None

        program(programCounter) match
            case IncrementValue => memory.put(pointer, memory(pointer) + 1)
            case DecrementValue => memory.put(pointer, memory(pointer) - 1)
            case JumpToEndOfBlockIfZero => if memory(pointer) == 0 then programCounter = jumpTargets(programCounter)
            case JumpToBeginningOfBlockIfNonZero => if memory(pointer) != 0 then programCounter = jumpTargets(programCounter)
            case IncrementPointer => pointer += 1
            case DecrementPointer => pointer -= 1
            case Output =>
                val value = memory(pointer).toChar // proper brainfuck uses ascii encoding.
                result.append(value)
        end match
        programCounter += 1
    end while

    Some(result.toList)
}

@main
def part1(): Unit = {

    boundary:
        for interpretation <- interpretations1 do
            println(s"Trying interpretation ${interpretation}")

            val interpP = interpretation('P')
            val interpL = interpretation('L')
            if interpP != JumpToEndOfBlockIfZero &&
                interpP != JumpToBeginningOfBlockIfNonZero &&
                interpL != JumpToEndOfBlockIfZero then

                val program1: Program = part1code1.map(interpretation)
                println(s"program1=$program1")

                val program1Result = run1Code1(program1)
                println(program1Result)
                program1Result match
                    case None => //skip
                    case Some(p1res) =>
                        if p1res.size == 2 && p1res.head - 2 == p1res.last then
                            println(p1res)  // should be List('4', '2')

                            val program2 = part1code2.map(interpretation)
                            println(run1Code2(program2))

                            if p1res == List('4', '2') then
                                break()
                            end if
                        end if
                end match
            end if
        end for

}

// part 2

val part2code1 = "TCTNTNTNTTNTHETCTCTTCTNTCTU_NOCECELNHUN_NEENTETUUN_ONETNTTNIETIU"
val part2code2 = "TCTTCHNETCTTCTU_OEHUTNTE_OUCELHETCTUC_OEHETETNENTCUCUCU_OUNUHETETETUUCU_OLHETU_ONLNECEH_NUCUC_ECEONUNTCTCIECENE_CIU_INUCUCUCIENECEECETCITCTTTCTCI_C__NELC_NECTCTNTTHUN_C_C_E_NONUHUIE_OUCUCU_N_C__N_C_IECECIUCUUUCUCTI"

trait InputFunction:
    def nextChar(): Option[Char]

object Input1 extends InputFunction {
    var alreadySupplied = false

    override def nextChar(): Option[Char] = {
        if alreadySupplied then return None
        alreadySupplied = true
        Some('1')
    }
}

val characters2 = part2code1.distinct //TCNHEU_OLI
val instructions2 = List(IncrementValue, DecrementValue, JumpToEndOfBlockIfZero, JumpToBeginningOfBlockIfNonZero, IncrementPointer, DecrementPointer, Output, Input, Unknown, Unknown)

val interpretations2: Seq[Interpretation] = characters2.permutations
    .map(wrappedString => wrappedString.zip(instructions2).toMap)
    .toSeq

class FixedInput(values: String) extends InputFunction {
    var cursor = 0
    override def nextChar(): Option[Char] = {
        if cursor >= values.length then return None
        val value = values.charAt(cursor)
        cursor += 1
        Some(value)
    }
}

def run2Code1(program: Program, input: InputFunction): Option[List[Char]] = {

    val start = System.currentTimeMillis()

    val result = new ListBuffer[Char]

    val jumpTargets = calculateJumpTargets(program) match
        case Some(targets) => targets
        case None => return None

    var programCounter = 0
    val memory = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var pointer = 0

    while programCounter < program.length do
        val currently = System.currentTimeMillis()
        if currently - start > 10_000 then
            // 10 seconds passed, just give up.
            return None

        program(programCounter) match
            case IncrementValue => memory.put(pointer, memory(pointer) + 1)
            case DecrementValue => memory.put(pointer, memory(pointer) - 1)
            case JumpToEndOfBlockIfZero => if memory(pointer) == 0 then programCounter = jumpTargets(programCounter)
            case JumpToBeginningOfBlockIfNonZero => if memory(pointer) != 0 then programCounter = jumpTargets(programCounter)
            case IncrementPointer => pointer += 1
            case DecrementPointer => pointer -= 1
            case Output =>
                if result.length == 2 then
                    // short-circuit if we pass the two characters of 42
                    return None

                val value = memory(pointer).toChar
                result.append(value)
            case Input => input.nextChar() match
                    case None => return None //short-circuit if we couldn't get more input
                    case Some(value) => memory.put(pointer, value.toInt)
            case Unknown => // what could we do here? I don't think we have to perform some operation here TODO or do we?
        end match
        programCounter += 1
    end while

    Some(result.toList)
}


def run2Code2(program: Program, input: InputFunction): Option[List[Char]] = {

    val start = System.currentTimeMillis()

    val result = new ListBuffer[Char]

    val jumpTargets = calculateJumpTargets(program) match
        case Some(targets) => targets
        case None => return None

    var programCounter = 0
    val memory = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var pointer = 0

    while programCounter < program.length do
        val currently = System.currentTimeMillis()
        if currently - start > 10_000 then
            // 10 seconds passed, just give up.
            return None

        program(programCounter) match
            case IncrementValue => memory.put(pointer, memory(pointer) + 1)
            case DecrementValue => memory.put(pointer, memory(pointer) - 1)
            case JumpToEndOfBlockIfZero => if memory(pointer) == 0 then programCounter = jumpTargets(programCounter)
            case JumpToBeginningOfBlockIfNonZero => if memory(pointer) != 0 then programCounter = jumpTargets(programCounter)
            case IncrementPointer => pointer += 1
            case DecrementPointer => pointer -= 1
            case Output =>
                val value = memory(pointer).toChar
                result.append(value)
            case Input => input.nextChar() match
                case None => return None //short-circuit if we couldn't get more input
                case Some(value) => memory.put(pointer, value.toInt)
            case Unknown => // TODO nothing to do here?
        end match
        programCounter += 1
    end while

    Some(result.toList)
}

@main
def part2(): Unit = {
    val input2 = FixedInput(StdIn.readLine())

    val interpretation: Interpretation = boundary {
        for interpretation <- interpretations2 do
            println(s"Trying interpretation ${interpretation}")
            val interpU = interpretation('U')
            val interpT = interpretation('T')
            val interpC = interpretation('C')
            val interpN = interpretation('N')
            if interpT != JumpToBeginningOfBlockIfNonZero &&
                interpU != JumpToEndOfBlockIfZero &&
                interpC != JumpToBeginningOfBlockIfNonZero &&
                interpN != JumpToBeginningOfBlockIfNonZero then
                    // TODO can we exclude more interpretations (perhaps use some logical Or expressions?)

                    val program1 = part2code1.map(interpretation)
                    val resultCode1 = run2Code1(program1, Input1)

                    resultCode1 match
                        case None =>
                        case Some(result1) =>
                            if result1 == List('4', '2') then
                                break(interpretation)
                    end match
            end if
        end for
        ??? // Unreachable
    }

    val program2 = part2code2.map(interpretation)
    println(run2Code2(program2, input2))
}
