package animation

import java.lang.Math._

case class Vec(x: Double, y: Double){

  def +(xy: Vec): Vec = {
    Vec(x + xy.x, y + xy.y)
  }

  def -(xy: Vec): Vec = {
    Vec(x - xy.x, y- xy.y)
  }

  def / (i: Double): Vec = {
    Vec(x/i, y/i)
  }

  def *(i: Double): Vec = {
    Vec(x*i, y*i)
  }

  def length(p1: Vec): Double = {
    sqrt(pow(x - p1.x, 2) + pow(y - p1.y, 2))
  }

  def abs: Vec = {
    Vec(x.abs, y.abs)
  }

  def slope: Double = x / y

  def rotate(origin: Vec, angle: Double): Vec = {
    val angleRad = toRadians(angle)
    //x_rotated = ((x - x_origin) * cos(angle)) - ((y_origin - y) * sin(angle)) + x_origin
    //y_rotated = ((y_origin - y) * cos(angle)) - ((x - x_origin) * sin(angle)) + y_origin
    val x2 = cos(angleRad) * (x - origin.x) - sin(angleRad) * (y - origin.y) + origin.x
    val y2 = sin(angleRad) * (x - origin.x) + cos(angleRad) * (y - origin.y) + origin.y
    Vec(x2, y2)
  }

  def pointOnLine(to: Vec, len: Double): Vec = {
    val step = (to - this) / length(to)
    this + step * len
  }

  def nextPoint(w: Double, angle: Int): Vec = {
    val radAngle = toRadians(angle)
    val x = this.x + w * cos(radAngle)
    val y = this.y + w * sin(radAngle)
    Vec(x, y)
  }
}

object Vec{
  def normVector(v1: Vec, v2: Vec): Vec = {
    val vd = v2 - v1
    val penpd= Vec(vd.y, -vd.x)
    penpd / vd.length(penpd)
  }
}
