package day1

import scala.util.{Try, Using}
import utils.Utils

object DayOnePartOne {
    def main(args: Array[String]): Unit = {
        val in:List[String] = Utils.readLines("day1/test_data.txt")
        val res = processAll(in)
        println(res)
    }
}

def stringToList(input:String): List[Any] = {
    input.flatMap {
        case c if c.isDigit => List(c.asDigit)
        case c => List(c.toString)
    }.toList
}

def getInts(input: List[Any]): List[Int] = {
    input.collect { case i:Int => i }
}

def getFirst(input:List[Int]): Int = {
    input(0)
}

def getLast(input:List[Int]): Int = {
    val r = input.reverse
    r(0)
}

def concatFirstLast(f: Int, l:Int): Int = {
    f.toString.concat(l.toString).toInt
}

def processLine(input:String): Int = {
    val ls = stringToList(input)
    val int_ls = getInts(ls)
    val first = getFirst(int_ls)
    val last = getLast(int_ls)
    concatFirstLast(first, last)
}

def processAll(input: List[String]): Int = {
    def inner(input:List[String], a:Int): Int = input match {
        case h::t => inner(t, a+processLine(h))
        case Nil => a
    }
    inner(input, 0)
}