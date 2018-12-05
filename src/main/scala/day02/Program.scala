package day02

import scala.io.Source

object Program {

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("./day02/input.txt").getLines.toList


    // Part 1
    val (x,y) = input
      .map(boxId => boxId.groupBy(c => boxId.count(_ == c)).keys.toList)
      .map(counts => counts.filter(x => x > 1 && x < 4))
      .map(counts => counts.product)
      .foldLeft(0 -> 0) { case ((l,r),p) => p match {
        case n if n % 6 == 0 => (l + 1) -> (r + 1)
        case n if n % 3 == 0 => l -> (r + 1)
        case n if n % 2 == 0 => (l + 1) -> r
      }}

    println(x * y)

    //Part 2

    def shared(id1: String, id2: String): String = id1.zip(id2).foldLeft("") { (str,pair) =>
      if(pair._1 == pair._2) {
        str + pair._1
      } else {
        str
      }
    }

    println(input.flatMap(i => input.map(i2 => shared(i,i2)).find(i.length - _.length()  == 1).toList).head)
  }
}
