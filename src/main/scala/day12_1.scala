package day12

import utils.Utils

object DayTwelvePartOne {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day12/test_data.txt")
        val rows = loadRows(data)
        rows.foreach(println)
        println(rows(0).pattern)
        println(rows(0).l)
    }
}

case class Row(input:String) {
    val pattern = input.split(" ")(0).split("").toList
    val l = input.split(" ")(1).split(",").toList
}

def loadRows(input: List[String]): List[Row] = {
    def inner(input: List[String], a: List[Row]): List[Row] = input match {
        case h::t => inner(t, a:+Row(h))
        case Nil => a
    }
    inner(input, List[Row]())
}