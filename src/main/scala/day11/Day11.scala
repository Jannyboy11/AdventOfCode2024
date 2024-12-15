package day11

import scala.io.Source

val source = Source.fromResource("day11.in")
val input: Seq[Stone] = source.getLines().next().split(" ").map(int => BigInt(int))

type Stone = BigInt

def step(stone: Stone): Seq[Stone] =
    if stone == BigInt(0) then
        Seq(BigInt(1))
    else
        val mag = magnitude(stone)
        if mag % 2 == 0 then
            split(stone, mag)
        else
            Seq(stone * 2024)

def magnitude(stone: Stone): Int =
    if stone >= 10 then 1 + magnitude(stone / 10) else 1

def split(stone: Stone, magnitude: Int): Seq[Stone] =
    val orderOfMagnitude = BigInt(Math.pow(10, magnitude / 2).toLong)
    Seq(stone / orderOfMagnitude, stone % orderOfMagnitude)

def simulate1(stones: Seq[Stone], steps: Int): Seq[Stone] =
    var state = stones
    var i = 0
    while i < steps do
        state = state.flatMap(step)
        i += 1
    end while
    state

type Stones = Map[Stone, BigInt]

def step(stones: Stones): Stones =
    var state = stones
    for (stone, count) <- stones do
        state = addStones(state, stone, -count)
        for newStone <- step(stone) do
            state = addStones(state, newStone, count)
        end for
    end for
    state

def addStones(stones: Stones, stone: Stone, count: BigInt): Stones =
    stones.updatedWith(stone) {
        case Some(existing) =>
            val newCount = existing + count
            if newCount == BigInt(0) then None else Some(newCount)
        case None => Some(count)
    }

def simulate2(stones: Stones, steps: Int): Stones =
    var state = stones
    var i = 0
    while i < steps do
        state = step(state)
        i += 1
    end while
    state

def size(stones: Stones): BigInt = stones.values.sum

@main def main(): Unit = {

    val result1 = simulate1(input, 25).size
    println(result1)

    val result2 = size(simulate2(input.groupMapReduce(identity)(_ => BigInt(1L))({_ + _}), 75))
    println(result2)

}