package day8

import scala.util.matching.Regex
import utils.Utils

object DayEightPartOne {
    def main(args: Array[String]):Unit = {
        // val stepsString: String = "RL"
        // // val stepsString: String = "LLRLLRLRLRRRLLRRRLRRLRLRLRLRLRLRRLRRRLRLLRRLRRLRRRLLRLLRRLLRRRLLLRLRRRLLLLRRRLLRRRLRRLRLLRLRLRRRLRRRLRRLRRLRRLRLLRRRLRRLRRRLLRRRLRLRRLLRRLLRLRLRRLRRLLRLLRRLRLLRRRLLRRRLRRLLRRLRRRLRLRRRLRRLLLRLLRLLRRRLRLRLRLRRLRRRLLLRRRLRRRLRRRLRRLRLRLRLRRRLRRLLRLRRRLRLRLRRLLLRRRR"
        // val steps = stepsToList(stepsString)
        // val data = Utils.readLines("day8/test_data1.txt")
        // val res = traverseMap(steps, data)
        // println(res)

        // Test case: 
        val stepsString:String = "LLR"
        val steps = stepsToList(stepsString)
        val data: List[String] = List("AAA = (BBB, BBB)", "BBB = (AAA, ZZZ)", "ZZZ = (ZZZ, ZZZ)")
        val res = traverseMap(steps, data)
        println(res)

    }
}

def stringToTuple(input:String): (String, String, String) = {
    val patSource: Regex = """^([A-Z0-9]..)""".r
    val patLeft: Regex = """\s*\(([A-Z0-9]..)""".r
    val patRight: Regex = """\s*([A-Z0-9]..)\)""".r
    val s = patSource.findFirstMatchIn(input).map(_.group(1)).getOrElse {""}
    val l = patLeft.findFirstMatchIn(input).map(_.group(1)).getOrElse {""}
    val r = patRight.findFirstMatchIn(input).map(_.group(1)).getOrElse {""}
    (s, l, r)
}

def stepsToList(input:String): List[String] = {
    input.split("").map(_.toString).toList
}

def buildMap(input: List[String]): Map[String, Map[String, String]] = {
    def inner(input: List[String], m: Map[String, Map[String, String]]): Map[String, Map[String, String]] = input match {
        case h::t => {
            val tp = stringToTuple(h)
            inner(t, m + (tp._1 -> Map("L"->tp._2, "R"-> tp._3)))
        }
        case Nil => m
    }
    val m: Map[String, Map[String, String]] = Map("D" -> Map("L"->"", "R"->""))
    inner(input, m)
}

def traverseMap(steps: List[String], data:List[String]):Int = {
    val map = buildMap(data)
    val steps_copy = steps.toList
    def inner(steps: List[String], currLoc:String, a:Int):Int = currLoc match {
        case "ZZZ" => {
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
    inner(steps, "AAA", 0)
}



// case class Node(start:String, left:String, right:String) {
//     def getLeft:String = {
//         left
//     }
//     def getRight:String = {
//         right
//     }
// }
