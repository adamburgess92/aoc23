package day15

import utils.Utils
import scala.util.matching.Regex
import scala.collection.mutable.LinkedHashMap

object DayFifteenPartTwo { 
    def main(args: Array[String]): Unit = {
        val data: String = Utils.readLines("day15/test_data.txt")(0) // Data is only one line
        val res = solve(data)
        println(res)
    }
}

case class Instruction(label: String, box: Int, focalLength:Option[Int])

def evalEntry(s: String): Instruction = {
    println(s)
    val splitString = s.split("[=-]").toList
    // List("rn", "1")
    // List("cm")
    val label: String = splitString(0)
    val focalLength: Option[Int] = splitString match {
        case List(_, l) => Some(l.toInt)
        case _ => None
    }
    val box = algString(label)
    // println(s"label: $label")
    // println(s"box: $box")
    // println(s"focalLength: $focalLength")
    // println(" ")
    Instruction(label, box, focalLength)
}

def evalAllEntries(lsData: List[String]): List[Instruction] = {
    def inner(lsData: List[String], a: List[Instruction]): List[Instruction] = lsData match {
        case h::t => inner(t, a:+evalEntry(h))
        case Nil => a
    }
    inner(lsData, Nil)
}

def processInstructions(instructions: List[Instruction]): Map[Int, LinkedHashMap[String, Int]] = {
    val boxes = (0 to 255).map(i => i -> LinkedHashMap.empty[String, Int]).toMap

    boxes.keys
    def inner(instructions: List[Instruction], b: Map[Int, LinkedHashMap[String, Int]]): Map[Int, LinkedHashMap[String, Int]] = instructions match {
        case h::t => {
            val label = h.label
            val box = h.box
            if (h.focalLength.isDefined) {
                // Add
                val bUpdated = b.updated(box, b(box).updated(label, h.focalLength.get))
                inner(t, bUpdated)
            } else {
                // Remove
                val bUpdated = b.updated(box, b(box).-(label))
                inner(t, bUpdated)

            }
        }
        case Nil => b
    }
    inner(instructions, boxes)
}

def getBoxPower(idx: Int, box: LinkedHashMap[String, Int]): List[(Int, Int, Int)] = {
    val k = box.keys.zipWithIndex.map { case (s, i) => i+1}
    val v = box.values
    val z = k.zip(v).map { (k, v) => (idx+1, k, v)}.toList 
    z
}

def getTotalFocusingPower(boxes: Map[Int, LinkedHashMap[String, Int]]): Int = {
    val p = boxes.map { case (k, v) => getBoxPower(k, v)}.flatten // List[(Int, Int, Int)]
    val total = p.map { case (a, b, c) => a*b*c}.sum
    total
}

def solve(data: String): Int = {
    val lsData = dataToList(data)
    val instructions: List[Instruction] = evalAllEntries(lsData)
    val boxes = processInstructions(instructions)
    getTotalFocusingPower(boxes)
}

