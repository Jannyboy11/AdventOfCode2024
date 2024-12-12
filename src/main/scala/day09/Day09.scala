package day09

import day09.Fragment.FreeSpace

import scala.collection.mutable
import scala.io.Source

val source = Source.fromResource("day09.in")
val input: Seq[Int] = source.map(_ - '0').toIndexedSeq

//val EMPTY = -1
//
//def diskSpace(diskMap: Seq[Int]): Seq[Int] = diskMap.zipWithIndex.flatMap:
//    case (size, index) => if index % 2 == 0 then Seq.fill(size)(index / 2) else Seq.fill(size)(EMPTY)
//
//def defrag1(disk: Seq[Int]): Seq[Int] = {
//    val array: Array[Int] = disk.toArray
//    var upperIdx = array.length - 1
//    var lowerIdx = 0
//
//    def moveLowerUp(): Unit = while array(lowerIdx) != EMPTY do lowerIdx += 1
//    def moveUpperDown(): Unit = while array(upperIdx) == EMPTY do upperIdx -= 1
//
//    while upperIdx > {moveLowerUp(); lowerIdx} do
//        println(java.util.Arrays.toString(array))
//
//        array(lowerIdx) = array(upperIdx)
//        array(upperIdx) = EMPTY
//        moveUpperDown()
//    end while
//
//    array.toSeq
//}
//
//def filesystemChecksum(disk: Seq[Int]): Int =
//    disk.takeWhile(_ != EMPTY).zipWithIndex.map(_ * _).sum

type ID = Int
type BlockSize = Int
enum Fragment(size: BlockSize):
    case File(id: ID, size: BlockSize) extends Fragment(size)
    case FreeSpace(size: BlockSize) extends Fragment(size)

type Disk = Seq[Fragment]

def parseDisk(diskMap: Seq[Int]): Disk = diskMap.zipWithIndex.map:
    case (size, index) => if index % 2 == 0 then Fragment.File(index / 2, size) else Fragment.FreeSpace(size)

case class File(id: ID, var size: BlockSize)

class Slot:
    val filled: mutable.Buffer[File] = new mutable.ArrayBuffer[File]()
    var empty: BlockSize = 0

    def size: BlockSize = empty + filled.size
    def isFull: Boolean = empty == 0
    def isEmpty: Boolean = filled.isEmpty || filled.forall(_.size == 0)
    def fill(`with`: File): Unit =
        val sizeToAdd = Math.min(empty, `with`.size)
        filled.addOne(File(`with`.id, sizeToAdd))
        `with`.size -= sizeToAdd
    def firstFile: File = filled.head

    override def toString: String = s"Slot(files=$filled,emptySize=$empty)"

def toSlot(fragment: Fragment): Option[Slot] = fragment match
    case Fragment.File(id, size) => val slot = Slot(); slot.filled.addOne(File(id, size)); Some(slot)
    case FreeSpace(0) => None
    case FreeSpace(size) => val slot = Slot(); slot.empty = size; Some(slot)

def toFragments(slot: Slot): Seq[Fragment] =
    val files = slot.filled.toSeq.map { case File(id, size) => Fragment.File(id, size) }
    if slot.isFull then files else files :+ Fragment.FreeSpace(slot.empty)

def defrag(disk: Disk): Disk = {
    val slots = disk.flatMap(toSlot).toArray
    println(java.util.Arrays.toString(slots.asInstanceOf[Array[Object]]))

    var lowerIdx = 0
    var upperIdx = slots.length - 1

    def moveLowerUp(): Unit = while slots(lowerIdx).isFull do lowerIdx += 1

    while {moveLowerUp(); lowerIdx} < upperIdx do
        val lowerSlot = slots(lowerIdx)
        val upperSlot = slots(upperIdx)

        println(s"lowerSlot=$lowerSlot")
        println(s"upperSlot=$upperSlot")

        if !upperSlot.isEmpty then
            assert(!lowerSlot.isFull)
            val upperSlotFile = upperSlot.firstFile
            lowerSlot.fill(upperSlotFile)   //TODO why do we keep doing this?
        end if

        if upperSlot.isEmpty then
            upperIdx -= 1
        end if
    end while

    slots.flatMap(toFragments)
}

//def compress(disk: Disk): Disk = {
//    ??? // TODO
//}

@main def main(): Unit = {

    val input = "2333133121414131402".map(_ - '0')

    val disk = parseDisk(input)
    println(disk)
    val defragged = defrag(disk)
    println(defragged)

//    val result1 = filesystemChecksum(defrag1(diskSpace(input)))
    val result1 = ???
    println(result1) //1523575464 is too low.

}