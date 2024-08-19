package day13

import scala.util.boundary, boundary.break
import utils.Utils
import scala.io.Source

object DayThirteenPartTwo {
    def main(args: Array[String]): Unit = {
        // Actual
        val blocks = readBlocks("day13/test_data.txt")
        val res = processAllBlocks(blocks, assertSmudgeMatch)
        println(res)
    }
}

