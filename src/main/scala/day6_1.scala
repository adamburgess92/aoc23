package day6

object DaySixPartOne {
    def main(args: Array[String]): Unit = {
        val time: List[Int] = List(61, 67, 75, 71)
        val dist: List[Int] = List(430, 1036, 1307, 1150)

        val r1 = numWaysToWin(61, 430)
        val r2 = numWaysToWin(67, 1036)
        val r3 = numWaysToWin(75, 1307)
        val r4 = numWaysToWin(71, 1150)
        val res = r1*r2*r3*r4
        println(res)

    }
}

def calcDistance(time_held: Int, total_time:Int): Int = {
    (total_time-time_held) * time_held
}

def numWaysToWin(time: Int, record_distance:Int): Int = {
    val time_held = (0 to time).toList
    def inner(time_held: List[Int], a: Int): Int = time_held match {
        case h::t => {
            val distance_travelled: Int = calcDistance(h, time)
            println(s"t: $h | distance: $distance_travelled")
            if (distance_travelled > record_distance) {
                inner(t, a+1)
            } else {
                inner(t, a)
            }
        }
        case Nil => a
    }
    inner(time_held, 0)
}