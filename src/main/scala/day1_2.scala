package day1

import utils.Utils

object DayOnePartTwo {
    def main(args: Array[String]): Unit = {
        val in:List[String] = Utils.readLines("day1/data.txt")
        val res = processAll2(in)
        println(res)
    }
}

def replStrings(input:String): String = {
    val numMap = Map(
        "one" -> "1", 
        "two" -> "2", 
        "three" -> "3", 
        "four" -> "4", 
        "five" -> "5", 
        "six" -> "6", 
        "seven" -> "7", 
        "eight" -> "8", 
        "nine" -> "9"
    )
    var stringSoFar =  ""
    for (c <- input) {
        stringSoFar = stringSoFar+c
        for (k <- numMap.keys) {
            if (stringSoFar.contains(k)) {
                var loc = stringSoFar.indexOf(k)
                var head = stringSoFar.slice(0, loc)
                var replacement = numMap(k)
                var tail = stringSoFar.slice(loc+1, stringSoFar.length)
                stringSoFar = head+replacement+tail
            }
        }
    }
    stringSoFar
}


// Would be nice to make this FP but I'm lazy 
def processLine2(input:String): Int = {
    val repl = replStrings(input)
    val ls = stringToList(repl)
    val int_ls = getInts(ls)
    val first = getFirst(int_ls)
    val last = getLast(int_ls)
    concatFirstLast(first, last)
}

def processAll2(input: List[String]): Int = {
    def inner(input:List[String], a:Int): Int = input match {
        case h::t => inner(t, a+processLine2(h))
        case Nil => a
    }
    inner(input, 0)
}