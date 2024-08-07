// package day8

// import scala.util.matching.Regex
// import utils.Utils

// object DayEightPartTwo {
//     def main(args: Array[String]):Unit = {
//         val stepsString: String = "LLRLLRLRLRRRLLRRRLRRLRLRLRLRLRLRRLRRRLRLLRRLRRLRRRLLRLLRRLLRRRLLLRLRRRLLLLRRRLLRRRLRRLRLLRLRLRRRLRRRLRRLRRLRRLRLLRRRLRRLRRRLLRRRLRLRRLLRRLLRLRLRRLRRLLRLLRRLRLLRRRLLRRRLRRLLRRLRRRLRLRRRLRRLLLRLLRLLRRRLRLRLRLRRLRRRLLLRRRLRRRLRRRLRRLRLRLRLRRRLRRLLRLRRRLRLRLRRLLLRRRR"
//         val steps = stepsToList(stepsString)
//         val data = Utils.readLines("day8/data.txt")
//         val res = play(steps, data)
//         println(res)

//         // Test case
//         // val stepsString: String = "LR"
//         // val steps = stepsToList(stepsString)
//         // val data = readFile("day8/test_data.txt")
//         // val res = play(steps, data)
//         // println("Final:")
//         // println(res)
//     }
// }


// def stringToTuple(input:String): (String, String, String) = {
//     val patSource: Regex = """^([A-Z0-9]..)""".r
//     val patLeft: Regex = """\s*\(([A-Z0-9]..)""".r
//     val patRight: Regex = """\s*([A-Z0-9]..)\)""".r
//     val s = patSource.findFirstMatchIn(input).map(_.group(1)).getOrElse {""}
//     val l = patLeft.findFirstMatchIn(input).map(_.group(1)).getOrElse {""}
//     val r = patRight.findFirstMatchIn(input).map(_.group(1)).getOrElse {""}
//     (s, l, r)
// }

// def stepsToList(input:String): List[String] = {
//     input.split("").map(_.toString).toList
// }

// def buildMap(input: List[String]): Map[String, Map[String, String]] = {
//     val m: Map[String, Map[String, String]] = Map("D" -> Map("L"->"", "R"->""))
//     def inner(input: List[String], m: Map[String, Map[String, String]]): Map[String, Map[String, String]] = input match {
//         case h::t => {
//             val tp = stringToTuple(h)
//             inner(t, m + (tp._1 -> Map("L"->tp._2, "R"-> tp._3)))
//         }
//         case Nil => m
//     }
//     inner(input, m)
// }

// def endA(input:String): Boolean = {
//     input.matches("[A-Z0-9].A")
// }

// def endZ(input:String): Boolean = {
//     input.matches("[A-Z0-9].Z")
// }

// def allEndZ(input: List[String]): Boolean = {
//     input.forall(endZ)
// }

// def getStartPos(input: List[String]): Map[String, Map[String, String]] = {
//     // Build map: 
//     val m = buildMap(input)
//     // Get keys like __a: 
//     val filteredMap:  Map[String, Map[String, String]] = m.filterKeys(endA).toMap
//     println(s"starting map: $filteredMap")
//     return filteredMap
// }

// // case class State(currKeys: Map[String, Map[String, String]], counter:Int)

// case class State(keys:List[String], map:Map[String, Map[String, String]], counter:Int) {
//     def stepThrough(direction:String): State = {
//         def inner(keys:List[String], newKeys:List[String]): List[String] = keys match {
//             case h::t => inner(t, newKeys:+map(h)(direction))
//             case Nil => newKeys
//         }
//         val initNewKeys:List[String] = List()
//         copy(inner(keys, initNewKeys), map, counter+1)
//     }
// }

// // def stepThrough(key:String, map:Map[String, Map[String, String]], direction: String):String = direction match {
// //     case "L" => map(key)("L")
// //     case "R" => map(key)("R")
// // }

// def play(steps: List[String], data: List[String]): Int = {
//     val m = buildMap(data)
//     val start = getStartPos(data).keys.toList
//     val initialState = State(start, m, 0)
//     val stepsCopy = steps.toList

//     initialState.keys.foreach(println)

//     def inner(steps: List[String], gameState:State): Int = steps match {
//         case h::t => {
//             if (allEndZ(gameState.keys)) {
//                 return gameState.counter
//             } else {
//                 if (gameState.counter % 100000 == 0) {
//                     println(gameState.counter)
//                 }
//                 // gameState.keys.foreach(println) // debug
//                 inner(t, gameState.stepThrough(h))
//             }
//         }
//         case Nil => {
//             // println("Going again")
//             inner(stepsCopy, gameState)
//         }
//     }
//     inner(steps, initialState)
// }
