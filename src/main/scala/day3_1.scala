package day3

import scala.util.matching.Regex
import utils.Utils

object DayThreePartOne {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day3/test_data.txt")
        val atomised_grid = atomiseGrid(data)
        // Part 1:
        val p = getRegexLocations(data, """\d+""".r)
        val res = getResultPartOne(atomised_grid, p)
        println(res)
    }
}

def getRegexLocations(ls_s: List[String], regex_pattern:Regex): List[Tuple3[String, (Int, Int), (Int, Int)]] = {
    def inner(s: List[String], row_num: Int, a:List[Tuple3[String, (Int, Int), (Int, Int)]]): List[Tuple3[String, (Int, Int), (Int, Int)]] = s match {
        case h::t => { // h looks like "467..114..""
            val tpl = regex_pattern.findAllMatchIn(h).map(m => Tuple3(m.matched, (row_num, m.start), (row_num, m.end-1)))
            inner(t, row_num+1, a++tpl)
        }
        case Nil => a
    }
    val a: List[(String, (Int, Int), (Int, Int))] = Nil
    inner(ls_s, 0, a)
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

def isSpecialCharacter(c: Option[Char]): Boolean = c match {
    case Some(ch) => !(ch.isDigit || ch == '.')
    case None => false
}

def checkIfAdjacent(atomised_grid: List[List[Char]], number_location: Tuple3[String, (Int, Int), (Int, Int)]): Int = {
    val number = number_location._1.toInt
    val row = number_location._2._1
    val start_col = number_location._2._2
    val end_col = number_location._3._2
    val cols = (start_col to end_col).toList
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

def getResultPartOne(atomised_grid: List[List[Char]], ls_number_location: List[Tuple3[String, (Int, Int), (Int, Int)]]): Int = {
    def inner(ls_number_location: List[Tuple3[String, (Int, Int), (Int, Int)]], a: Int): Int = ls_number_location match {
        case h::t => inner(t, a+checkIfAdjacent(atomised_grid, h))
        case Nil => a
    }
    inner(ls_number_location, 0)
}
