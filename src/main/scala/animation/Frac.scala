package animation

import scala.util.Random

object Frac {

  sealed trait CenterType
  case object Water extends CenterType
  case object Terrain extends CenterType

  def randomAngle(seed: Int): Int = Random.nextInt().abs % 360

  def genPolygon(c: Vec, corners: Int, radius: Int, seed: Int): List[Vec] = {
    val angles = (0 to corners).map(randomAngle(_))
      .sorted
    //println(angles)
    angles.map(c.nextPoint(radius, _))
      .toList

  }

  def breakLine(st: Vec, end: Vec, dots: Int, seed: Int): List[Vec] = {
    val ratio = st.length(end) / 3
    val centerPoint = st.pointOnLine(end, ratio)

    def nextP(i: Int): Vec = {
      val nextLen = Random.nextInt().abs % ratio
      val nextRatio = randomAngle(seed)
      centerPoint.nextPoint(nextLen, nextRatio)
    }

    val imdt: List[Vec] = 0 to dots map nextP toList

    def sorter(a: Vec, b: Vec): Boolean
      = st.length(a) < st.length(b)

    val x = st +: imdt.sortWith(sorter) :+ end
    x
  }

  def splitByLines(l: List[Vec]): List[(Vec, Vec)] = {
    val sides = for(idx <- l.indices.dropRight(1)) yield {
      (l(idx), l(idx + 1))
    }
    sides.toList
  }

  def fracPiece(c: Vec, radius: Int, breakLevel: Int): List[Vec] = {
    val polygon = genPolygon(c, 5, radius, 1)

    def brk(l: (Vec, Vec)): List[Vec] = {
      breakLine(l._1, l._2, 2, 0)
    }

    def breakRec(inp: List[Vec], level: Int): List[Vec] = {
      val res = splitByLines(inp).flatMap(brk)
      if(level > 0){
        breakRec(res, level - 1)
      }else {
        res
      }
    }

    val res = breakRec(polygon :+ polygon.head, breakLevel)

    res :+ res.head
    //polygon :+ polygon.head

  }


}
