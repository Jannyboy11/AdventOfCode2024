package day09

import day09.Fragment.{File, FreeSpace}

import scala.collection.mutable
import scala.io.Source

//val testInput = "12345".map(_ - '0').toIndexedSeq
val testInput = "2333133121414131402".map(_ - '0').toIndexedSeq

val source = Source.fromResource("day09.in")
val input: Seq[Int] = source.map(_ - '0').toIndexedSeq

type ID = Int
type BlockSize = Int
enum Fragment:
    case File(index: Int, id: ID, size: BlockSize)
    case FreeSpace(index: Int, size: BlockSize)

type Disk = Seq[Fragment]

def parseDisk(diskMap: Seq[Int]): Disk = diskMap.zipWithIndex
    .map:
        case (size, index) => if index % 2 == 0 then Fragment.File(index, index / 2, size) else Fragment.FreeSpace(index, size)
    .filter { case Fragment.FreeSpace(_, 0) => false; case _ => true }

// for debugging
def diskToString(disk: Disk): String = disk.flatMap {
    case Fragment.FreeSpace(index, size) => ".".repeat(size)
    case Fragment.File(index, id, size) => s"$id".repeat(size)
}.mkString

def defragment1(disk: Disk): Disk = {
    val deque = scala.collection.mutable.ArrayDeque.from(disk)

    var insertIndex = 0
    var removeIndex = deque.size - 1

    def moveInsertIndex() : Unit = while !deque(insertIndex).isInstanceOf[Fragment.FreeSpace] do insertIndex += 1
    def moveRemoveIndex(): Unit = removeIndex = Math.min(removeIndex, deque.size - 1)

    while { moveRemoveIndex(); removeIndex } > { moveInsertIndex(); insertIndex } do
        deque(removeIndex) match
            case Fragment.FreeSpace(_, _) =>
                removeIndex -= 1
            case file @ Fragment.File(fileIdx, fileId, fileSize) =>
                val Fragment.FreeSpace(freeSpaceIdx, availableSize) = deque(insertIndex): @unchecked

                if fileSize < availableSize then
                    deque.patchInPlace(removeIndex, Seq(), 1)   // not necessary to put a Fragment.FreeSpace back
                    deque.patchInPlace(insertIndex, Seq(file, Fragment.FreeSpace(freeSpaceIdx, availableSize - fileSize)), 1)
                else if fileSize == availableSize then
                    deque.patchInPlace(removeIndex, Seq(), 1)   // not necessary to put a Fragment.FreeSpace back
                    deque.patchInPlace(insertIndex, Seq(file), 1)
                    removeIndex -= 1
                else
                    deque.patchInPlace(removeIndex, Seq(Fragment.File(fileIdx, fileId, fileSize - availableSize)), 1)
                    deque.patchInPlace(insertIndex, Seq(Fragment.File(fileIdx, fileId, availableSize)), 1)
                end if
        end match
    end while

    deque.toSeq
}

def filesystemChecksum(disk: Disk): Long = {
    var index = 0
    var position = 0
    var sum = 0L

    while index < disk.size do
        disk(index) match
            case Fragment.FreeSpace(_, size) =>
                position += size
            case Fragment.File(_, id, size) =>
                var i = 0
                while i < size do
                    sum += position * id

                    position += 1
                    i += 1
                end while
        end match
        index += 1
    end while

    sum
}

def defragment2(disk: Disk): Disk = {

    




    ???
}

@main def main(): Unit = {

    val disk = parseDisk(input)

    val result1 = filesystemChecksum(defragment1(disk))
    println(result1)

    val result2 = filesystemChecksum(defragment2(parseDisk(testInput)))
    println(result2)

}