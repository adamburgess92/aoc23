package day4

import scala.util.matching.Regex
import utils.Utils

// Answer is 5132675

object DayFourPartTwo {
    def main(args: Array[String]): Unit = {
        val in = Utils.readLines("day4/data.txt")
        val ops = f(in)
        val res = fl(ops).sum
        println(res)
    }
}

def numMatch(winners:List[Int], draw:List[Int]): Int = {
    var a:Int = 0
    for (d <- draw) {
        for (w <- winners) {
            if (d==w) {
                a += 1
            }
        }
    }
    a
}

def matchesInLine(input:String): Int = {
    val winners = getWinners(input)
    val draw = getDraw(input)
    val n_matches = numMatch(winners, draw)
    n_matches
}

def f(input:List[String]): List[Int] = {
    def g(input:List[String], a:List[Int]):List[Int] = input match {
        case h::t => {
            val n = matchesInLine(h) 
            g(t, a:+n)
        }
        case Nil => a
    }
    val a:List[Int] = List()
    g(input, a)
}

def update_val(a: Array[Int], value:Int, start:Int, end:Int): Array[Int] = {
    for (i <- start to end) {
        a(i) = a(i)+value
    }
    // a.foreach(println)
    // println("****")
    a
}

def fl(ops:List[Int]): List[Int] ={
    var sa = Array.fill(ops.length)(1)
    for((v, i) <- sa.zipWithIndex) {
        var max_idx = i+ops(i)
        var value = sa(i)
        sa = update_val(sa, value, i+1, max_idx)
    }
    sa.toList
}




