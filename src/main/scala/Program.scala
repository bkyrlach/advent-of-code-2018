import scala.annotation.tailrec
import scala.io.Source

object Program {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines.toList.map { _.toInt }
    // Part 1
    println(input.sum)

    // Part 2
    println(findFrequency(input))
  }

  def findFrequency(xs: List[Int]): Int = {
    @tailrec
    def go(sum: Int, ys: List[Int], seen: Set[Int]): Int = ys match {
      case Nil => go(sum,xs,seen)
      case h::t =>
        val newSum = sum + h
        if(seen.contains(newSum)) {
          newSum
        } else {
          go(newSum, t, seen + newSum)
        }
    }
    go(0, xs, Set(0))
  }
}
