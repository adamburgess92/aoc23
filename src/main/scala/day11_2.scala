package day11

import utils.Utils

object DayElevenPartTwo {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day11/data.txt")
        val datac = charRows(data)
        // datac.foreach(println)
        val r = rowEmpties(datac)
        val c = colEmpties(datac)
        // println("Row expansion")
        // r.foreach(println)
        // println("Col expansion")
        // c.foreach(println)
        val galaxies = getGalaxies(datac)
        val pairs = createPairs(galaxies)
        val res = sumAcrossAll2(pairs, r, c, 1000000)
        println(res)
    }
}

def rowEmpties(input: List[List[Char]]): List[Int] = {
    def inner(input: List[List[Char]], rowIdx: Int, a: List[Int]): List[Int] = input match {
        case h::t => {
            if (h.contains('#')) {
                inner(t, rowIdx+1, a)
            } else {
                inner(t, rowIdx+1, a:+rowIdx)
            }
        }
        case Nil => a
    }
    inner(input, 0, List[Int]())
}

def colEmpties(input: List[List[Char]]): List[Int] = {
    val t = input.transpose
    rowEmpties(t)
}


def adjDist(g1: Galaxy, g2: Galaxy, r: List[Int], c: List[Int], m: Int): Long = {
    val maxH = g2.row.max(g1.row)
    val minH = g2.row.min(g1.row)
    val maxV = g2.col.max(g1.col)
    val minV = g2.col.min(g1.col)

    val rInc = r.filter(x => x>minH && x<maxH).length
    val cInc = c.filter(x => x>minV && x<maxV).length
    
    
    val hDist = (maxH - minH).abs + rInc*(m-1)
    val vDist = (maxV - minV).abs + cInc*(m-1)
    
    val dist = hDist + vDist
    
    // For debugging
    // println(s"g1: $g1 | g2: $g2 | rInc: $rInc | cInc: $cInc | dist: $dist")
     
    return dist
}

def sumAcrossAll2(input: List[(Galaxy, Galaxy)], r:List[Int], c:List[Int], m:Int): Long = {
    def inner(input: List[(Galaxy, Galaxy)], a:Long): Long = input match {
        case h::t => {
            val d = adjDist(h._1, h._2, r, c, m)
            inner(t, a+d)
        }
        case Nil => a
    }
    inner(input, 0L)
}
