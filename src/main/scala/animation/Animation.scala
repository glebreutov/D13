package animation

import java.lang.Math._

object Animation extends App {


  def nextPoint(c: Vec, w: Double, angle: Int): Vec = {
    val radAngle = toRadians(angle)
    val x = c.x + w * cos(radAngle)
    val y = c.y + w * sin(radAngle)
    Vec(x, y)
  }

  def angleForSide(side: Int) = side % 3 match{
    case 0 => 0
    case 1 => 120
    case _ => -120
  }

  def superThree(start: Vec, sideLen: Int): List[Vec] = {
    val short = sideLen
    val long = sideLen + sideLen / 6
    var pp = start
    val units = for {i <- 0 until 5} yield {
      val angle = angleForSide(i % 3)
      val len = if (i % 2 == 0) short else long
      val np = nextPoint(pp, len, angle)
      pp = np
      np
    }
    start :: units.toList
  }

  def cframe(offI: Int, p: List[Vec]): List[Vec] = {
    val List(p0, p1) = p.take(2)
    val List(p25, p3) = p.takeRight(2)
    val off  = offI / 100.0
    //val List(p0, p1, p2, p25, p3)  = p
    val np0  = p0 + ((p1 - p0) / p1.length(p0)) * off * p1.length(p0)
    val np3 = p25 + ((p3 - p25) / p3.length(p25)) * p3.length(p25) * off
    List(np0, p1) ::: p.drop(2).dropRight(2) ::: List(p25, np3)
  }

  def superThreeFrame(start: Vec, sideLen: Int, frame: Int): List[Vec] ={
    val xies = superThree(start, sideLen)
    val frameRelative = frame % xies.size
    val parts = 6
    (xies ::: xies).slice(frameRelative, frameRelative + parts)
  }

  def circleRatio(sideLen: Double): Double =
    sqrt(pow(sideLen, 2) - pow(sideLen / 2, 2)) * 2/3

  def startingPointCenter(center: Vec, sideLen: Double): Vec = {
    val supportVectorLen = circleRatio(sideLen)
    nextPoint(center, supportVectorLen, 360-150)
  }

  def frac1(st: Vec, end: Vec, a: Int): List[Vec] = {
    val len = st.length(end)
    val p1 = st.pointOnLine(end, len / 3.0)
    val p2 = st.pointOnLine(end, 2 * len / 3.0)

    val angle = toRadians(a)
    val triSide = (len / 6) / cos(angle)
    val top: Vec = p1.pointOnLine(end, triSide)
    val top1 = top.rotate(p1, a)
    val list = st :: p1 :: top1 :: p2 :: end :: Nil
    list
  }

  def fracN(iter: Int, angle: Int, points: List[Vec]): List[Vec] = {
    val vectors: List[Vec] = for {
      i <- (0 until points.size - 1).toList
      p <- frac1(points(i), points(i + 1), angle)
    } yield p

    if(iter - 1 > 0){
      fracN(iter -1, angle, vectors)
    }else {
      vectors
    }

  }

  def buildScene(height: Double, width: Double, compl: SceneState): List[Vec] = {
    val center = Vec(height / 2, width / 2)
    val sideLen = 0.8 * height
    val medianLen = (sideLen / 2) / cos(toRadians(30))
    val p1 = center.copy(y = center.y - medianLen).rotate(center, -60)
    val p2 = center.copy(y = center.y - medianLen).rotate(center, 60)
    val p3 = center.copy(y = center.y - medianLen).rotate(center, 180)
    val tri = p1 :: p2 :: p3 :: p1 :: Nil
    fracN(compl.compl, compl.angle, tri)
  }


  //println(fracN(4, frac1(Vec(100, 100), Vec(500, 100))))



}
