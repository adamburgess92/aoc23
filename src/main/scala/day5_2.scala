package day5

object DayFivePartTwo {
    def main(args: Array[String]): Unit = {
        val seeds_range: List[(Long, Long)] = List(
            // (start of range, length of range)
            (2276375722L, 160148132L), 
            (3424292843L, 82110297L), 
            (1692203766L, 342813967L), 
            (3289792522L, 103516087L), 
            (2590548294L, 590357761L), 
            (1365412380L, 80084180L), 
            (3574751516L, 584781136L), 
            (4207087048L, 36194356L), 
            (1515742281L, 174009980L), 
            (6434225L, 291842774L)
        )
        val res = chunk(2276375922L, 250000000L)
        println(res)
    }
}

// Do it in chunks: 
def chunk(start_loc: Long, size: Long): Unit = {
    val r = (start_loc to start_loc+size).toList
    for (i <- r) {
        traverse(i)
    }
}
