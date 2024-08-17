package day15

import utils.Utils

object DayFifteenPartOne { 
    def main(args: Array[String]): Unit = {
        val data: String = Utils.readLines("day15/data.txt")(0) // Data is only one line
        val l = dataToList(data)
        val res = score(l)
        println(res)

        // println(algString("rn"))
    }
}

def dataToList(s: String): List[String] = {
    s.split(",").toList
}

def alg(init_val: Int, c: Char): Int = {
    val ascii_val = c.toInt
    val updated_val = (init_val + ascii_val) * 17
    updated_val % 256
}

def algString(s:String): Int = {
    val ls_s = s.trim.toList
    ls_s.foldLeft(0)((a, c) => alg(a, c))
}

def score(ls_s: List[String]): Int = {
    val scores = ls_s.map(i=>algString(i))
    scores.foldLeft(0)((a, c) => a+c)
}

