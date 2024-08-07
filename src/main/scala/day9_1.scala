package day9

import scala.util.matching.Regex
import utils.Utils

object DayNinePartOne {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day9/data.txt")
        val res = processAllRows(data)
        println(res)
        // val res = doTheThing(data(4))
        // println(res)
    }
}

def rowToListInt(input: String): List[Int] = {
    val strArray = input.split(" ").toList
    strArray.map(_.toInt)
}

def getDiff(input: List[Int]): List[Int] = {
    def inner(ls: List[Int], a: List[Int], prev: Option[Int]): List[Int] = ls match {
        case h::t => prev match {
            case None => inner(t, a, Some(h))
            case Some(p) => {
                val diff = h - p
                inner(t, a:+diff, Some(h))
            }
        }
        case Nil => a
    }
    inner(input, List(), None)
}

def getLast(input:List[Int]): Int = {
    input.last
}

def getSum(input:List[Int]): Int = {
    input.foldLeft(0)(_+_)
}

def getDepth(input:List[Int]): Int = {
    input.foldLeft(0)(_+_)
}

case class State(a: List[List[Int]]) {
    def addLayer: State = {
        val newLayer = getDiff(a.last)
        State(a:+newLayer)
    }
    def rowSum: Int = {
        getSum(a.last)
    }
    def allZero: Boolean = {
        a.last.forall(_ == 0)
    }
}

def doTheThing(input: String): Int = {
    val ls =  rowToListInt(input)
    val initState = State(List(ls))
    // val sumRow = initState.rowSum
    // println(sumRow)

    def inner(s:State, lasts:List[Int]): List[Int] = {
        s.allZero match {
            case true => {
                lasts
            }
            case false => {
                inner(s.addLayer, lasts:+s.a.last.last)
            }
        }
    }
    val r = inner(initState, List())
    // r.foreach(println)
    r.foldLeft(0)(_+_)
}

def processAllRows(input:List[String]): Int = {
    def inner(ls: List[String], a:List[Int]): List[Int] = ls match {
        case h::t => {
            inner(t, a:+doTheThing(h))
        }
        case Nil => a
    }
    inner(input, List()).foldLeft(0)(_+_)
}

