package day3

import utils.Utils

object DayThreePartTwo {
    def main(args: Array[String]): Unit = {
        val data = Utils.readLines("day3/test_data.txt")
        val atomised_grid = atomiseGrid(data)
        val num_locs = getRegexLocations(data, """\d+""".r)
        val gear_locs = getRegexLocations(data, """\*""".r)
        val res = getResultPartTwo(gear_locs, num_locs)
        println(res)
    }
}

def getResultPartTwo(ls_gear: List[Tuple3[String, (Int, Int), (Int, Int)]], ls_nums: List[Tuple3[String, (Int, Int), (Int, Int)]]): Int = {
    // ls_gear.foreach(println)
    // println()
    // ls_nums.foreach(println)
    
    // Function for recursing over ls_nums, checking adjacency to a given gear
    def inner_nums(gear_row: Int, gear_col: Int, ls_nums: List[Tuple3[String, (Int, Int), (Int, Int)]], a: List[Int]): List[Int] = ls_nums match {
        case h::t => {
            // h is (467, (0, 0), (0, 2))
            val up_left = (gear_row-1, gear_col-1)
            val up = (gear_row-1, gear_col)
            val up_right = (gear_row-1, gear_col+1)
            val left = (gear_row, gear_col-1)
            val right = (gear_row, gear_col+1)
            val down_left = (gear_row+1, gear_col-1)
            val down = (gear_row+1, gear_col)
            val down_right = (gear_row+1, gear_col+1)
            val adj_l = List(up_left, up, up_right, left, right, down_left, down, down_right)
            
            val num = h._1.toInt
            val num_start = h._2
            val num_stop = h._3
            // Generate tuples
            val num_l = (num_start._2 to num_stop._2).map(i => (num_start._1, i)).toList
            // If any of adj_l in num_l, append num to a
            if (adj_l.exists(num_l.contains)) {
                inner_nums(gear_row, gear_col, t, a:+num)
            } else {
                inner_nums(gear_row, gear_col, t, a)
            }
        }
        case Nil => a
    }

    // Recurse through ls_gear: 
    def inner_gear(ls_gear: List[Tuple3[String, (Int, Int), (Int, Int)]], a:Int): Int = ls_gear match {
        // h is (*,List(1, 3),List(1, 3))
        case h::t => {
            val gear_row = h._2.head
            val gear_col = h._2.last
            val init_a: List[Int] = Nil
            val adj = inner_nums(gear_row, gear_col, ls_nums, init_a)
            if (adj.length == 2) {
                inner_gear(t, a+adj.head*adj.last)
            } else {
                inner_gear(t, a)
            }
        }
        case Nil => a
    }
    inner_gear(ls_gear, 0)
}
