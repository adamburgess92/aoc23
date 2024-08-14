package day13

import scala.util.boundary, boundary.break
import utils.Utils
import scala.io.Source
import day4.numMatch

object DayThirteenPartOne {
    def main(args: Array[String]): Unit = {
        // Actual
        val blocks = readBlocks("day13/test_data.txt")
        val res = processAllBlocks(blocks, assertMatch)
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

def assertMatch(z: List[(String, String)]): Boolean = {
    z.forall { case (a, b) => a==b}
}

def assertSmudgeMatch(z: List[(String, String)]): Boolean = {
    if (assertMatch(z)) { // Needs to be a different reflection point
        false 
    } else {
        // Iterate over z: 
        val zLength = z.length
        def inner(z: List[(String, String)], nMatch: Int, nDiscrep: Int): (Int, Int) = z match {
            case h::t =>  { 
                // h = (#...##..#, #....#..#)
                val isMatch = h._1 == h._2 // exact match
                val smudgeMatch = h._1.zip(h._2).count( { case (a, b) => a!=b} ) == 1

                if (isMatch) {
                    inner(t, nMatch+1, nDiscrep)
                } else if (smudgeMatch) {
                    inner(t, nMatch, nDiscrep+1)
                } else {
                    inner(t, nMatch, nDiscrep)
                }
            }
            case Nil => (nMatch, nDiscrep)
        }
        val (nMatch, nDiscrep) = inner(z, 0, 0)
        if (nMatch==zLength-1 && nDiscrep==1) true else false
    }
}


def getReflectPointHorizontal(data: List[String], multiplier: Int, func: (z: List[(String, String)]) => Boolean): Option[Int] = boundary {
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
        val allMatch = func(z)
        if (allMatch && z.nonEmpty) {
            break(Some((i)*multiplier))
        }
    }
    None
}

def getReflectionScore(data: List[String], func: (z: List[(String, String)])=>Boolean): Int = {
    val dataT = transposeData(data)
    getReflectPointHorizontal(dataT, 1, func).getOrElse { // Check for vertical reflection by transposing data
        getReflectPointHorizontal(data, 100, func).getOrElse(0) // Check for horizontal reflection
    }
}

def processAllBlocks(dataBlocks: List[List[String]], func:(z:List[(String, String)])=>Boolean): Int = {
    def inner(dataBlocks: List[List[String]], a: Int): Int = dataBlocks match {
        case h::t => inner(t, a+getReflectionScore(h, func))
        case Nil => a
    }
    inner(dataBlocks, 0)
}
