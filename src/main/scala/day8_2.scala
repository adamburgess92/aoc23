package day8

import scala.util.matching.Regex
import utils.Utils

object DayEightPartTwo {
    def main(args: Array[String]):Unit = {
        val stepsString: String = "LLRLLRLRLRRRLLRRRLRRLRLRLRLRLRLRRLRRRLRLLRRLRRLRRRLLRLLRRLLRRRLLLRLRRRLLLLRRRLLRRRLRRLRLLRLRLRRRLRRRLRRLRRLRRLRLLRRRLRRLRRRLLRRRLRLRRLLRRLLRLRLRRLRRLLRLLRRLRLLRRRLLRRRLRRLLRRLRRRLRLRRRLRRLLLRLLRLLRRRLRLRLRLRRLRRRLLLRRRLRRRLRRRLRRLRLRLRLRRRLRRLLRLRRRLRLRLRRLLLRRRR"
        // val stepsString: String = "LR"
        val steps = stepsToList(stepsString)
        val data = Utils.readLines("day8/data.txt")
        val m = buildMap(data)
        val s = getStartLocs(m)
        val dists = getAllDists(steps, data, s)
        val res = lcm(dists.map(i => i.toLong))
        println(res)
    }
}

def getStartLocs(m: Map[String, Map[String, String]]): List[String] = {
    val ks = m.keys.toList
    def inner(ks: List[String], a:List[String]): List[String] = ks match {
        case h::t => {
            // h is 
            // 11Z -> Map(L -> 11B, R -> XXX)
            if (h.last=='A') {
                inner(t, a:+h)
            } else {
                inner(t, a)
            }
        }
        case Nil => a
    }
    inner(ks, List())
}

def traverseMapWStartLoc(steps: List[String], data:List[String], startLoc: String):Int = {
    val map = buildMap(data)
    val steps_copy = steps.toList
    def inner(steps: List[String], currLoc:String, a:Int):Int = currLoc match {
        case c if c.endsWith("Z") => {
            a
        }
        case _ => steps match {
            case h::t => h match {
                case "L" => {
                    inner(t, map(currLoc)("L"), a+1)
                }
                case "R" => {
                    inner(t, map(currLoc)("R"), a+1)
                }
            }
            case Nil => {
                inner(steps_copy, currLoc, a)
            }
        }
    }
    inner(steps, startLoc, 0)
}

def getAllDists(steps: List[String], data:List[String], startLocs: List[String]): List[Int] = {
    def inner(startLocs: List[String], a:List[Int]): List[Int] = startLocs match {
        case h::t => {
            val d = traverseMapWStartLoc(steps, data, h)
            println("h: " + h.toString)
            println("d: " + d.toString)
            println()
            inner(t, a:+d)
        }
        case _ => a
    }
    inner(startLocs, List())
}

// Function to compute the greatest common divisor (GCD) of two numbers
def gcd(a: Long, b: Long): Long = {
    if (b == 0) a else gcd(b, a % b)
}

// Function to compute the LCM of two numbers
def lcm(a: Long, b: Long): Long = {
    (a * b).abs / gcd(a, b)
}

// Function to compute the LCM of a list of numbers
def lcm(nums: List[Long]): Long = {
    nums.reduce(lcm)  // Use reduce to fold the list with the lcm function
}