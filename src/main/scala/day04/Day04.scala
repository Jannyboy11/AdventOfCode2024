package day04

import scala.io.Source

val testInput1 = """..X...
                   |.SAMX.
                   |.A..A.
                   |XMAS.S
                   |.X....""".stripMargin

val testInput2 =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin    //output: 19. expected: 18.

val source = Source.fromResource("day04.in")
val input: Matrix = source.getLines().map(_.toSeq).toSeq

type Matrix = Seq[Seq[Char]]
extension (matrix: Matrix)
    def width: Int = matrix(0).size
    def height: Int = matrix.size
    def apply(x: Int, y: Int): Char = matrix(y)(x)

type Step = -1 | 0 | 1

def gatherSequence(matrix: Matrix, startX: Int, startY: Int, stepX: Step, stepY: Step, maxSteps: Int): Seq[Char] = {
    var x = startX
    var y = startY
    var steps = 0
    val resBuilder = Seq.newBuilder[Char]
    while 0 <= x && x < matrix.width && 0 <= y && y < matrix.height && steps < maxSteps do
        resBuilder.addOne(matrix(x, y))
        x += stepX
        y += stepY
        steps += 1
    end while
    resBuilder.result()
}

def findStringsStartingAt(haystack: Matrix, startX: Int, startY: Int, needle: Seq[Char]): Int = {
    val rows = for
        stepX <- -1 to 1
        stepY <- -1 to 1
        if !(stepX == 0 && stepY == 0)
    yield gatherSequence(haystack, startX, startY, stepX.asInstanceOf[Step], stepY.asInstanceOf[Step], needle.size)
    rows.count(_ == needle)
}

def solve1(matrix: Matrix): Int =
    val XMAS = "XMAS".toSeq
    val counts = for x <- 0 until matrix.width; y <- 0 until matrix.height yield findStringsStartingAt(matrix, x, y, XMAS)
    counts.sum

def isX_MAS(haystack: Matrix, startX: Int, startY: Int): Boolean = {
    def isMAS(seq: Seq[Char]): Boolean = seq == "MAS".toSeq || seq == "SAM".toSeq

    val firstDiagonal = gatherSequence(haystack, startX-1, startY-1, 1, 1, 3)
    val secondDiagonal = gatherSequence(haystack, startX-1, startY+1, 1, -1, 3)

    isMAS(firstDiagonal) && isMAS(secondDiagonal)
}

def solve2(matrix: Matrix): Int =
    (for x <- 0 until matrix.width; y <- 0 until matrix.height yield isX_MAS(matrix, x, y)).count(identity)

@main def main(): Unit = {

    val result1 = solve1(input)
    println(result1)

    val result2 = solve2(input)
    println(result2)

}