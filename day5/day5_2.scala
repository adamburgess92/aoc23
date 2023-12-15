import scala.io.Source

object DayFive {
    def main(args: Array[String]): Unit = {
        val seeds:List[Long] = List(2276375722L, 160148132L, 3424292843L, 82110297L, 1692203766L, 342813967L, 3289792522L, 103516087L, 2590548294L, 590357761L, 1365412380L, 80084180L, 3574751516L, 584781136L, 4207087048L, 36194356L, 1515742281L, 174009980L, 6434225L, 291842774L)
        val res2 = getMin(seeds)
        println(res2)
        
    }
}

def readLines(path:String): List[String] = {
    Source.fromFile(path).getLines.toList
}

def getMapped(v:Long, maps: List[String]): Long = maps match {
    case h::t => {
        val s = h.split(" ").toList.map(_.toLong)
        if (v>=s(1) && v<s(1)+s(2)) {
            // println("h")
            // println(v + (s(0)-s(1)))
            return v + (s(0)-s(1))
        } else
            getMapped(v, t)
        }
    case Nil => {
        // println("N")
        // println(v)
        v
    }
}

def traverse(input:Long):Long = {
    val distance:Long = getMapped(getMapped(getMapped(getMapped(getMapped(getMapped(getMapped(input, seed_soil), soil_fert), fert_water), water_light), light_temp), temp_humidity), humi_loc)
    return distance
}

def getMin(seeds:List[Long]): Long = {
    def inner(seeds:List[Long], a: Long): Long = seeds match {
        case h::t => {
            val distance = traverse(h)
            // val distance = test_traverse(h)
            if (distance<a) {
                inner(t, distance)
            } else {
                inner(t, a)
            }
        }
        case Nil => a
    }
    inner(seeds, 656464920000L)
}

val seed_soil:List[String] = readLines("day5/seed_to_soil.txt")
val soil_fert:List[String] = readLines("day5/soil_to_fertilizer.txt")
val fert_water:List[String] = readLines("day5/fertilizer_to_water.txt")
val water_light:List[String] = readLines("day5/water_to_light.txt")
val light_temp:List[String] = readLines("day5/light_to_temperature.txt")
val temp_humidity:List[String] = readLines("day5/temperature_to_humidity.txt")
val humi_loc:List[String] = readLines("day5/humidity_to_location.txt")
