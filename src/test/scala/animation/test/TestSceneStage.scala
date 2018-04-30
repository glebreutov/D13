package animation.test

import animation.Frac.genPolygon
import animation.{SceneState, Vec}

object TestSceneStage extends App{
  val polygon = genPolygon(Vec(100, 100), 5, 100, 0)
  println(polygon)
}
