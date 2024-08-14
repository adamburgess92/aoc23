package day13

import scala.util.boundary, boundary.break
import utils.Utils
import scala.io.Source

object DayThirteenPartOne {
    def main(args: Array[String]): Unit = {
        // Actual
        val blocks = readBlocks("day13/test_data.txt")
        val res = processAllBlocks(blocks)
        println(res)
    }
}

def readBlocks(path: String): List[List[String]] = {
    def inner(lines: List[String], currentBlock: List[String], a: List[List[String]]): List[List[String]] = lines match {
        case h::t => {
            // println(h)
            if (h.trim.isEmpty) {
                val resetCurrentBlock: List[String] = Nil
                inner(t, resetCurrentBlock, a:+currentBlock)
            } else {
                inner(t, currentBlock:+h, a)
            }
        }
        case Nil => a
    }
    val source = Source.fromResource(path)
    val lines = source.getLines().toList :+ ""
    val currentBlockInit: List[String] = Nil
    val aInit: List[List[String]] = Nil

    val out = inner(lines, currentBlockInit, aInit)
    source.close()
    return out
}

def atomiseData(data: List[String]): List[List[Char]] = {
    def inner(data: List[String], a: List[List[Char]]): List[List[Char]] = data match {
        case h::t => inner(t, a:+h.toList)
        case Nil => a
    }
    val aInit: List[List[Char]] = Nil
    inner(data, aInit)
}

def transposeData(data: List[String]): List[String] = {
    val grid = atomiseData(data)
    val gridT = grid.transpose
    // gridT.map(i=>i.reverse).map(j=>j.mkString)
    gridT.map(j=>j.mkString)
}

// def assertMatch(z: List[Tuple]): Boolean = {
//     z.forall { case (a, b) => a==b}
// }

def getReflectPointHorizontal(data: List[String], multiplier: Int): Option[Int] = boundary {
    for (i <- 0 to data.length){
        val above = data.slice(0, i)
        val below = data.slice(i, data.length)
        // Reverse above and zip:
        val revAbove = above.reverse
        val z = {
            if (revAbove.length <= below.length) {
                revAbove.zip(below)
            } else {
                below.zip(revAbove)
            }
        }
        val allMatch = z.forall { (a, b) => a==b}
        println(z)
        if (allMatch && z.nonEmpty) {
            break(Some((i)*multiplier))
        }
    }
    None
}

def getReflectionScore(data: List[String]): Int = {
    val dataT = transposeData(data)
    getReflectPointHorizontal(dataT, 1).getOrElse {
        getReflectPointHorizontal(data, 100).getOrElse(0)
    }
}

def processAllBlocks(dataBlocks: List[List[String]]): Int = {
    def inner(dataBlocks: List[List[String]], a: Int): Int = dataBlocks match {
        case h::t => inner(t, a+getReflectionScore(h))
        case Nil => a
    }
    inner(dataBlocks, 0)
}
