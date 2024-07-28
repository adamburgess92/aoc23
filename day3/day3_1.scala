import scala.io.Source
import scala.util.matching.Regex

object DayThree {
    def main(args: Array[String]): Unit = {
        val data = readLinesDayThree("day3/data.txt")
        data.foreach(println)
        val p = getNumberLocations(data)
        val atomised_grid = atomiseGrid(data)
        val res = getResultPartOne(atomised_grid, p)
        println(res)
    }
}

def readLinesDayThree(path:String): List[String] = {
    val source = Source.fromFile(path)
    val lines = try source.getLines().toList
    finally source.close()
    return lines
}

def getIntInRow(s:String): List[String] = {
    val num_pattern: Regex = """\d+""".r
    val m = num_pattern.findAllIn(s).toList
    m
}

def getNumberLocations(s: List[String]): List[Tuple3[Int, List[Int], List[Int]]] = {
    def inner(s: List[String], row_num: Int, a:List[Tuple3[Int, List[Int], List[Int]]]): List[Tuple3[Int, List[Int], List[Int]]] = s match {
        case h::t => { // h is 467..114..
            val ints: List[String] = getIntInRow(h) // [467, 114]
            val starts: List[Int] = ints.map(h.indexOf(_))
            val ls_starts: List[List[Int]] = starts.map(n => List(row_num, n))
            val ends: List[Int] = ints.map(i => h.indexOf(i)+i.length)
            val ls_ends: List[List[Int]] = ends.map(n => List(row_num, n))
            // Make tuple
            val tpl = ints.zip(ls_starts).zip(ls_ends).map({case ((a, b), c) => (a.toInt, b, c)})
            inner(t, row_num+1, a++tpl)
        }
        case Nil => a
    }
    val a: List[(Int, List[Int], List[Int])] = Nil
    inner(s, 0, a)
}

def atomiseGrid(grid: List[String]): List[List[Char]] = {
    def inner(grid: List[String], a: List[List[Char]]): List[List[Char]] = grid match {
        case h::t => inner(t, a:+h.toCharArray().toList)
        case Nil => a
    }
    val a: List[List[Char]] = Nil
    inner(grid, a)
}


def queryGrid(atomised_grid: List[List[Char]], row: Int, col:Int): Option[Char] = {
    try {
        Some(atomised_grid(row)(col))
    } catch {
        case e: IndexOutOfBoundsException => None
    }
}

def isSpecialCharacter(c: Option[Char]): Boolean = {
  c match {
    case Some(ch) => !(ch.isDigit || ch == '.')
    case None => false
  }
}

def checkIfAdjacent(atomised_grid: List[List[Char]], number_location: Tuple3[Int, List[Int], List[Int]]): Int = {
    val number = number_location._1
    val row = number_location._2.head
    val start_col = number_location._2.last
    val end_col = number_location._3.last
    val cols = (start_col to end_col).toList
    // for (c <- cols) {println(c)}
    // Iterate: 
    def inner(row: Int, ls_cols: List[Int], a:Int): Int = ls_cols match {
        case h::t => { // h = 0
            val up_left = queryGrid(atomised_grid, row-1, h-1)
            val up = queryGrid(atomised_grid, row-1, h)
            val up_right = queryGrid(atomised_grid, row-1, h+1)
            val left = queryGrid(atomised_grid, row, h-1)
            val right = queryGrid(atomised_grid, row, h+1)
            val down_left = queryGrid(atomised_grid, row+1, h-1)
            val down = queryGrid(atomised_grid, row+1, h)
            val down_right = queryGrid(atomised_grid, row+1, h+1)

            // If any in "special characters"
            val l = List(up_left, up, up_right, left, right, down_left, down, down_right)
            val s = l.map(i => if (isSpecialCharacter(i)) 1 else 0).sum
            if (s>0){
                inner(row, t, a+1)
            } else {
                inner(row, t, a)
            }
        }
        case Nil => a
    }
    val res = inner(row, cols, 0)
    if (res>0) number else 0
}

def getResultPartOne(atomised_grid: List[List[Char]], ls_number_location: List[Tuple3[Int, List[Int], List[Int]]]): Int = {
    def inner(ls_number_location: List[Tuple3[Int, List[Int], List[Int]]], a: Int): Int = ls_number_location match {
        case h::t => inner(t, a+checkIfAdjacent(atomised_grid, h))
        case Nil => a
    }
    inner(ls_number_location, 0)
}