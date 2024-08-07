package day11

import utils.Utils

object DayElevenPartOne {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day11/data.txt")
        val datac = charRows(data)
        datac.foreach(println)
        val expanded = expandBothAxes(datac)
        expanded.foreach(println)
        val galaxies = getGalaxies(expanded)
        println(galaxies)
        val dist = distBetweenGalaxies(galaxies(0), galaxies(6))
        println(dist)
        val combos = galaxies.combinations(2).flatMap(_.combinations(2)).collect{ case List(a, b) => (a, b) }.toList
        combos.foreach(println)
        val res = sumAcrossAll(combos)
        println(res)
    }
}

def rowsToChar(input: String): List[Char] = {
    input.split("").map(_.charAt(0)).toList
}

def charRows(input: List[String]): List[List[Char]] = {
    def inner(input: List[String], output: List[List[Char]]): List[List[Char]] = input match {
        case h::t => {
            inner(t, output :+ rowsToChar(h))
        }
        case Nil => output
    }
    inner(input, List())
}

def expand(input: List[List[Char]]): List[List[Char]] = {
    def inner(input: List[List[Char]], output: List[List[Char]]): List[List[Char]] = input match {
        case h::t => {
            if (h.contains('#')) {
                inner(t, output :+ h)
            } else {
                inner(t, output :+ h :+ h)
            }
        }
        case Nil => output
    }
    inner(input, List())
}

def expandBothAxes(input: List[List[Char]]): List[List[Char]] = {
    val rowExpanded = expand(input)
    val rowExpandedT = rowExpanded.transpose
    val colExpanded = expand(rowExpandedT)
    return colExpanded.transpose
}

// What we want is a List of points that the galaxies are at. 
case class Galaxy(row: Int, col:Int)

def getGalaxies(grid: List[List[Char]]): List[Galaxy] = {
    val nRow = grid.length
    val nCol = grid(0).length
    var out = Array[Galaxy]()
    for {
        i <- 0 to nRow - 1
        j <- 0 to nCol - 1
    } if (grid(i)(j)=='#') out = out :+ Galaxy(i, j)

    out.toList
}

def createPairs(galaxies: List[Galaxy]): List[(Galaxy, Galaxy)] = {
    galaxies.combinations(2).flatMap(_.combinations(2)).collect{ case List(a, b) => (a, b) }.toList
}

def distBetweenGalaxies(g1: Galaxy, g2: Galaxy): Int = {
    val horzDist = (g2.col - g1.col).abs
    val vertDist = (g2.row - g1.row).abs
    horzDist + vertDist
}

def sumAcrossAll(input: List[(Galaxy, Galaxy)]): Int = {
    def inner(input: List[(Galaxy, Galaxy)], a:Int): Int = input match {
        case h::t => {
            val d = distBetweenGalaxies(h._1, h._2)
            inner(t, a+d)
        }
        case Nil => a
    }
    inner(input, 0)
}
