package day06

import scala.collection.mutable
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break

val source = Source.fromResource("day06.in")
val input: Map = source.getLines().map(_.map {
    case '.' => Tile.Empty
    case '^' => Tile.Guard
    case '#' => Tile.Obstacle
}).toSeq

enum Tile:
    case Guard
    case Empty
    case Obstacle

type Map = Seq[Seq[Tile]]

def getTile(x: Int, y: Int, map: Map): Tile = map(y)(x)

enum Direction:
    case North, East, South, West
    def turnRight: Direction = Direction.fromOrdinal((ordinal + 1 ) % 4)

case class State(guardX: Int, guardY: Int, direction: Direction)

def initialState(map: Map): State =
    boundary:
        for y <- 0 until map.size do
            for x <- 0 until map(y).size do
                if getTile(x, y, map) == Tile.Guard then
                    break(State(x, y, Direction.North))
        ???

def nextCoordinates(guardX: Int, guardY: Int, direction: Direction): (Int, Int) = direction match
    case Direction.North => (guardX, guardY-1)
    case Direction.East => (guardX+1, guardY)
    case Direction.South => (guardX, guardY+1)
    case Direction.West => (guardX-1, guardY)

def step(state: State, map: Map): State = {
    var direction = state.direction
    val (x, y) = nextCoordinates(state.guardX, state.guardY, direction)
    var nextX = x
    var nextY = y
    while isInMap(nextX, nextY, map) && getTile(nextX, nextY, map) == Tile.Obstacle do
        direction = direction.turnRight
        val (x, y) = nextCoordinates(state.guardX, state.guardY, direction)
        nextX = x
        nextY = y
    end while
    State(nextX, nextY, direction)
}

def isInMap(x: Int, y: Int, map: Map): Boolean =
    0 <= y && y < map.size && 0 <= x && x < map(y).size

def collectCoordinatesTilOutOfMap(initialState: State, map: Map): Set[(Int, Int)] = {
    val res = Set.newBuilder[(Int, Int)]
    var state = initialState
    while isInMap(state.guardX, state.guardY, map) do
        res.addOne((state.guardX, state.guardY))
        state = step(state, map)
    end while
    res.result()
}

def willGetStuck(initialState: State, map: Map): Boolean = {
    val states = mutable.Set.empty[State]
    var state = initialState
    while isInMap(state.guardX, state.guardY, map) do
        if states.contains(state) then
            return true
        states.addOne(state)
        state = step(state, map)
    end while
    false
}

def extraMaps(map: Map): Seq[Map] =
    for y <- map.indices; x <- map(y).indices; if getTile(x, y, map) == Tile.Empty yield replaceTile(x, y, map, Tile.Obstacle)

def replaceTile(x: Int, y: Int, map: Map, tile: Tile): Map =
    map.updated(y, map(y).updated(x, tile))

// Idea which should perform better (implementation is currently bugged though..):
//def collectStatesTilOutOfMap(initialState: State, map: Map): Set[(Int, Int)] = {
//    val obstaclePositions = mutable.Set.empty[(Int, Int)]
//    val states = mutable.Set.empty[State]
//    var state = initialState
//    while isInMap(state.guardX, state.guardY, map) do
//        if states.contains(State(state.guardX, state.guardY, state.direction.turnRight)) then
//            // We've been here before, we went right!
//            val (nextX, nextY) = nextCoordinates(state.guardX, state.guardY, state.direction)
//            // So at the *next* tile we could have an obstacle to force a right turn (and end up in a loop)!
//            if isInMap(nextX, nextY, map) && getTile(nextX, nextY, map) != Tile.Obstacle then
//                // Only register the extra obstacle tile if it's not already an obstacle, and it's not outside the map.
//                obstaclePositions.addOne((nextX, nextY))
//            end if
//        end if
//        states.addOne(state)
//        state = step(state, map)
//    end while
//    obstaclePositions.remove((initialState.guardX, initialState.guardY))
//    obstaclePositions.toSet
//}

@main def main(): Unit = {

    val result1 = collectCoordinatesTilOutOfMap(initialState(input), input).size
    println(result1)

    //val result2 = collectStatesTilOutOfMap(initialState(input), input).size //278 too low
    val result2 = extraMaps(input).count(map => willGetStuck(initialState(input), map))
    println(result2)

}