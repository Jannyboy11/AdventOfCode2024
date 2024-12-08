package day08

import scala.io.Source

val source = Source.fromResource("day08.in")
val input: Grid = source.getLines().map(line => line.toIndexedSeq).toIndexedSeq

type Tile = Char
type Grid = IndexedSeq[IndexedSeq[Tile]]
case class Point(x: Int, y: Int):
    def +(difference: Difference): Point = Point(this.x + difference.x, this.y + difference.y)
    def -(difference: Difference): Point = Point(this.x - difference.x, this.y - difference.y)
case class Difference(x: Int, y:Int)
extension (grid: Grid)
    def apply(x: Int, y: Int): Tile = grid(y)(x)

type Antenna = Char
def isAntenna(tile: Tile): Boolean =
    ('a' <= tile && tile <= 'z') || ('A' <= tile && tile <= 'Z') || ('0' <= tile && tile <= '9')

def collectAntennas(grid: Grid): Seq[Point] =
    for y <- grid.indices; x <- grid(y).indices if isAntenna(grid(x, y)) yield Point(x, y)

def buildAntennaMap(antennaPoints: Seq[Point], grid: Grid): Map[Antenna, Seq[Point]] =
    antennaPoints.groupMap(point => grid(point.x, point.y))(identity)

def difference(a: Point, b: Point): Difference = Difference(b.x - a.x, b.y - a.y)

def antinodes1(sameAntennaPoints: Seq[Point]): Seq[Point] =
    sameAntennaPoints.combinations(2)
        .flatMap { case Seq(a, b) => val diff = difference(a, b); Seq(a - diff, b + diff) }.toSeq

def isInside(point: Point, grid: Grid): Boolean = point match
    case Point(x, y) => 0 <= x && x < grid(0).size && 0 <= y && y < grid.size

def solve1(grid: Grid): Int =
    val antennaMap = buildAntennaMap(collectAntennas(grid), grid)
    val antinodePoints = antennaMap.values.toSet.flatMap(antennaPoints => antinodes1(antennaPoints).toSet)
    antinodePoints.count(isInside(_, grid))

def antinode2(sameAntennaPoints: Seq[Point], grid: Grid): Seq[Point] =
    sameAntennaPoints.combinations(2).flatMap { case Seq(a, b) =>
        val diff = difference(a, b)
        LazyList.iterate(a)(x => x - diff).takeWhile(isInside(_, grid)) ++
            LazyList.iterate(b)(y => y + diff).takeWhile(isInside(_, grid))
    }.toSeq

def solve2(grid: Grid): Int =
    val antennaMap = buildAntennaMap(collectAntennas(grid), grid)
    val antinodePoints = antennaMap.values.toSet.flatMap(antennaPoints => antinode2(antennaPoints, grid).toSet)
    antinodePoints.size

@main def main(): Unit = {

    val result1 = solve1(input)
    println(result1)

    val result2 = solve2(input)
    println(result2)

}