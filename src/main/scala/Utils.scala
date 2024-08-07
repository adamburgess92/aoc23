package utils

import scala.io.Source

object Utils {
    def readLines(path:String): List[String] = {
        val source = Source.fromResource(path)
        val lines = try source.getLines().toList
        finally source.close()
        return lines
    }
}