package day09

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day09.in")
val input: Seq[Int] = source.map(_ - '0').toIndexedSeq

type ID = Int
type BlockSize = Int
enum Fragment:
    case File(id: ID, size: BlockSize)
    case FreeSpace(size: BlockSize)

type Disk = Seq[Fragment]

def parseDisk(diskMap: Seq[Int]): Disk = diskMap.zipWithIndex
    .map:
        case (size, index) => if index % 2 == 0 then Fragment.File(index / 2, size) else Fragment.FreeSpace(size)
    .filter { case Fragment.FreeSpace(0) => false; case _ => true }

def defragment1(disk: Disk): Disk = {
    val deque = scala.collection.mutable.ArrayDeque.from(disk)

    var insertIndex = 0
    var removeIndex = deque.size - 1

    def moveInsertIndex() : Unit = while !deque(insertIndex).isInstanceOf[Fragment.FreeSpace] do insertIndex += 1
    def moveRemoveIndex(): Unit = removeIndex = Math.min(removeIndex, deque.size - 1)

    while { moveRemoveIndex(); removeIndex } > { moveInsertIndex(); insertIndex } do
        deque(removeIndex) match
            case Fragment.FreeSpace(_) =>
                removeIndex -= 1
            case file @ Fragment.File(fileId, fileSize) =>
                val Fragment.FreeSpace(availableSize) = deque(insertIndex): @unchecked

                if fileSize < availableSize then
                    deque.patchInPlace(removeIndex, Seq(), 1)   // not necessary to put a Fragment.FreeSpace back
                    deque.patchInPlace(insertIndex, Seq(file, Fragment.FreeSpace(availableSize - fileSize)), 1)
                else if fileSize == availableSize then
                    deque.patchInPlace(removeIndex, Seq(), 1)   // not necessary to put a Fragment.FreeSpace back
                    deque.patchInPlace(insertIndex, Seq(file), 1)
                    removeIndex -= 1
                else
                    deque.patchInPlace(removeIndex, Seq(Fragment.File(fileId, fileSize - availableSize)), 1)
                    deque.patchInPlace(insertIndex, Seq(Fragment.File(fileId, availableSize)), 1)
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
            case Fragment.FreeSpace(size) =>
                position += size
            case Fragment.File(id, size) =>
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
    // Possible optimisation: keep track of FreeSpace in a Map-like structure.
    type Index = Int

    val deque = scala.collection.mutable.ArrayDeque.from(disk)

    def getInsertIndex(requiredSize: BlockSize, lastIdx: Index): Option[Index] = {
        var insertIndex = 0
        while insertIndex < lastIdx && (deque(insertIndex) match { case Fragment.FreeSpace(size) if size >= requiredSize => false; case _ => true }) do insertIndex += 1
        if insertIndex == lastIdx then None else Some(insertIndex)
    }

    var removeIdx = deque.size - 1
    while removeIdx > 0 do
        deque(removeIdx) match
            case Fragment.FreeSpace(_) => // nothing to do. we don't want to move free space.
                removeIdx -= 1
            case file @ Fragment.File(_, fileSize) =>
                getInsertIndex(fileSize, removeIdx) match
                    case Some(freeSpaceIndex) =>
                        val Fragment.FreeSpace(availableSize) = deque(freeSpaceIndex): @unchecked
                        deque.patchInPlace(removeIdx, Seq(Fragment.FreeSpace(fileSize)), 1)
                        if fileSize < availableSize then
                            deque.patchInPlace(freeSpaceIndex, Seq(file, Fragment.FreeSpace(availableSize - fileSize)), 1)
                        else if fileSize == availableSize then
                            deque.patchInPlace(freeSpaceIndex, Seq(file), 1)
                            removeIdx -= 1
                        end if
                    case None => // nothing to do. don't move file.
                        removeIdx -= 1
                end match
        end match
    end while

    deque.toSeq
}

@main def main(): Unit = {

    val disk = parseDisk(input)

    val result1 = filesystemChecksum(defragment1(disk))
    println(result1)

    val result2 = filesystemChecksum(defragment2(disk))
    println(result2)

}