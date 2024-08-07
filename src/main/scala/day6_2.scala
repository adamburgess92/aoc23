package day6

object DaySixPartTwo {
    def main(args: Array[String]): Unit = {
        val res = numWaysToWinLong(61677571, 430103613071150L)
        println(res)

    }
}

def calcDistanceLong(time_held: Int, total_time:Long): Long = {
    (total_time-time_held) * time_held
}

def numWaysToWinLong(time: Int, record_distance: Long): Int = {
    var time_held: Int = 0
    var a: Int = 0
    while (time_held <= time) { 
        var distance_travelled = calcDistanceLong(time_held, time)
        if (distance_travelled > record_distance) {
            a += 1
        }
        // println(s"Distance travelled $distance_travelled | a = $a")
        time_held += 1
    }
    a
}