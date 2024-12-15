package day10

import scala.io.Source

val source = Source.fromResource("day10.in")
val input: HeightMap = source.getLines().map(line => line.map(_ - '0')).toIndexedSeq

type Height = Int
type HeightMap = IndexedSeq[IndexedSeq[Height]]
extension (map: HeightMap)
    def apply(x: Int, y: Int): Height = map(y)(x)
    def height: Int = map.size
    def width: Int = map.head.size
    def inRangeX(x: Int): Boolean = 0 <= x && x < width
    def inRangeY(y: Int): Boolean = 0 <= y && y < height
    def apply(point: Point): Height = apply(point.x, point.y)

case class Point(x: Int, y: Int)

def findStartingPoints(map: HeightMap): Seq[Point] =
    for y <- 0 until map.height; x <- 0 until map.width if map(x, y) == 0 yield Point(x, y)

def findNextTrailSteps(x: Int, y: Int, map: HeightMap): Seq[Point] =
    for (newX, newY) <- Seq((x, y+1), (x, y-1), (x+1, y), (x-1, y)) if map.inRangeX(newX) && map.inRangeY(newY) && map(newX, newY) == map(x, y) + 1 yield Point(newX, newY)

case class TrailTree(x: Int, y: Int, nexts: Seq[TrailTree]) // too lazy to make actual DAG stucture, lol :)

def findHikingTrails(startX: Int, startY: Int, map: HeightMap): TrailTree =
    TrailTree(startX, startY, for Point(nextX, nextY) <- findNextTrailSteps(startX, startY, map) yield findHikingTrails(nextX, nextY, map))

def getEndPoints(trailTree: TrailTree): Seq[Point] = trailTree match
    case TrailTree(x, y, Seq()) => Seq(Point(x, y))
    case TrailTree(_, _, nexts) => nexts.flatMap(getEndPoints)

def score(trailTree: TrailTree, map: HeightMap): Int =
    getEndPoints(trailTree).toSet.count(point => map(point) == 9)

def solve1(map: HeightMap): Int =
    findStartingPoints(map)
        .map { case Point(startX, startY) => findHikingTrails(startX, startY, map) }
        .map(trailTree => score(trailTree, map))
        .sum

def rating(trailTree: TrailTree, map: HeightMap): Int =
    getEndPoints(trailTree).count(point => map(point) == 9)

def solve2(map: HeightMap): Int =
    findStartingPoints(map)
        .map { case Point(startX, startY) => findHikingTrails(startX, startY, map) }
        .map(trailTree => rating(trailTree, map))
        .sum

@main def main(): Unit = {

    val result1 = solve1(input)
    println(result1)

    val result2 = solve2(input)
    println(result2)

}