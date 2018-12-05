package day03

import scala.io.Source

case class Claim(number: Int, x: Int, y: Int, x1: Int, y1: Int)

object Program {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("./day03/input.txt").getLines.toList

    val claims = input.map { s =>
      val parts = s.split(' ')
      val claimNum = parts(0).substring(1).toInt
      val coords = parts(2).substring(0, parts(2).length - 1).split(',').map(_.toInt)
      val x = coords(0)
      val y = coords(1)
      val dims = parts(3).split('x').map(_.toInt)
      val width = dims(0)
      val height = dims(1)
      Claim(claimNum,x,y,x + width ,y + height)
    }

    //part 1
    val overlap = claims.foldLeft(Map.empty[(Int,Int),Set[Int]])((m, c) =>
      (c.x until c.x1).foldLeft(m)((m1,x) =>
        (c.y until c.y1).foldLeft(m1)((m2,y) =>
          m2.updated((x,y), m.getOrElse((x,y),Set.empty[Int]) + c.number))))

    println(overlap.count(_._2.size > 1))

    //part 2
    val claimIds = claims.map(_.number).toSet
    println(claimIds -- overlap.values.filter(_.size > 1).foldLeft(Set.empty[Int])(_.union(_)))
  }
}