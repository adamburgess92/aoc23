// New file because too hard to just add to part 1

package day7_2

import scala.util.matching.Regex
import utils.Utils

object DaySevenPartTwo {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day7/data.txt")
        val games = loadGames(data)
        val s = ruleSort(games)
        val res = multiplier(s)
        println(res)
    }
}

val cardMap: Map[String, Int] = Map(
    "2"->2, 
    "3"->3, 
    "4"->4,
    "5"->5,
    "6"->6,
    "7"->7,
    "8"->8,
    "9"->9,
    "T"->10,
    "J"->1,
    "Q"->12,
    "K"->13,
    "A"->14
)

case class Game(input: String) {
    val hand = {
        val pattern:Regex = """[A-Z0-9]{5}""".r
        val m = pattern.findFirstIn(input).get
        m.split("").toList
    }
    // Replace with numeric values: 
    val handValues = hand.flatMap( i => cardMap.get(i))
    // Get most frequenly occuring cards (that aren't Jokers)
    val counts = handValues.filter(_ !=1).groupBy(identity).view.mapValues(_.size).toMap
    val jokerHand = if (counts.isEmpty) { // If all jokers, do nothing...
        handValues
    } else {
        // replace jokers with most frequently occuring other card.
        val maxCount = counts.values.max
        val mostFreqCard = counts.filter(_._2 == maxCount).keys.max
        handValues.map {
            case 1 => mostFreqCard
            case other => other
        }
    }
    val bid = {
        val pattern: Regex = """\s(\d*)$""".r
        val m = pattern.findFirstIn(input).get.trim
        m.toInt
    }
    // Win types 
    val foo: List[Int] = jokerHand.groupBy(identity).view.mapValues(_.size).values.toList
    val fiveKind: Boolean = if (foo.contains(5)) then true else false
    val fourKind: Boolean = if (foo.contains(4)) then true else false
    val fullHouse: Boolean = if (foo.contains(3) && foo.contains(2)) then true else false
    val threeKind: Boolean = if (foo.contains(3) && !foo.contains(2)) then true else false
    val twoPair: Boolean = {
        val numTwos = foo.count(_==2)
        if (numTwos == 2) then true else false
    }
    val onePair: Boolean = {
        val numTwos = foo.count(_==2)
        val numOnes = foo.count(_==1)
        if (numTwos==1 && numOnes==3) then true else false
    }
    val highCard: Boolean = if (foo.length == 5) then true else false
    val scores: List[(Boolean, Int)] = List(
        (fiveKind, 6), 
        (fourKind, 5), 
        (fullHouse, 4), 
        (threeKind, 3), 
        (twoPair, 2), 
        (onePair, 1), 
        (highCard, 0)
    )
    val score: Int = scores.collectFirst {
        case (predicate, value) if predicate => value
    }.getOrElse(0)
    // Card values - use original hand values (J=1 for sorting)
    val cardOneVal = handValues(0)
    val cardTwoVal = handValues(1)
    val cardThreeVal = handValues(2)
    val cardFourVal = handValues(3)
    val cardFiveVal = handValues(4)
}

def loadGames(data: List[String]): List[Game] = {
    def inner(in: List[String], out: List[Game]): List[Game] = in match {
        case h::t => inner(t, out:+Game(h))
        case Nil => out
    }
    inner(data, List[Game]())
}

def ruleSort(games: List[Game]): List[Game] = {
    games.sortBy(games => (games.score, games.cardOneVal, games.cardTwoVal, games.cardThreeVal, games.cardFourVal, games.cardFiveVal))
}

def multiplier(sortedGames: List[Game]): Int = {
    def inner(sortedGames: List[Game], r: Int, a: Int): Int = sortedGames match {
        case h::t => {
            inner(t, r+1, a+h.bid * r)
        }
        case Nil => a
    }
    inner(sortedGames, 1, 0)
}
