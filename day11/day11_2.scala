import scala.io.Source

object DayEleven {
    def main(args: Array[String]): Unit = {
        val data = readLines("day11/data.txt")
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
        val res = sumAcrossAll(pairs, r, c, 1000000)
        println(res)
    }
}

def readLines(path:String): List[String] = {
    val source = Source.fromFile(path)
    val lines = try source.getLines().toList
    finally source.close()
    lines
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

def sumAcrossAll(input: List[(Galaxy, Galaxy)], r:List[Int], c:List[Int], m:Int): Long = {
    def inner(input: List[(Galaxy, Galaxy)], a:Long): Long = input match {
        case h::t => {
            val d = adjDist(h._1, h._2, r, c, m)
            inner(t, a+d)
        }
        case Nil => a
    }
    inner(input, 0L)
}
