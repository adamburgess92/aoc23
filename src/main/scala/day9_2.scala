package day9

import scala.util.matching.Regex
import utils.Utils

object DayNinePartTwo {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day9/test_data.txt")
        val res = processAllRows2(data)
        println(res)
        // val res = doTheThing(data(4))
        // println(res)
    }
}

def doTheThing2(input: String): Int = {
    val ls =  rowToListInt(input)
    val initState = State(List(ls))
    // val sumRow = initState.rowSum
    // println(sumRow)

    def inner(s:State, firsts:List[Int]): List[Int] = {
        s.allZero match {
            case true => {
                firsts
            }
            case false => {
                inner(s.addLayer, firsts:+s.a.last.head)
            }
        }
    }
    val r = inner(initState, List())
    // r.foreach(println)
    // r.foldLeft(0)(_-_)
    val firstVal = r(0) 
    val theRest = r.slice(1, r.length)
    var out = firstVal

    for ((elem, i)<-theRest.zipWithIndex) {
        if (i==0 || i%2==0) {
            out -= elem
        } else {
            out += elem
        }
    }
    println(out)
    out
}

def processAllRows2(input:List[String]): Int = {
    def inner(ls: List[String], a:List[Int]): List[Int] = ls match {
        case h::t => {
            inner(t, a:+doTheThing2(h))
        }
        case Nil => a
    }
    inner(input, List()).foldLeft(0)(_+_)
}
