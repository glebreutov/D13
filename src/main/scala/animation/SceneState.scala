package animation

case class SceneState(frameCounter: Int, compl: Int = 4) {
  private val maxAngle = 40
  //private val maxCompl = 4

  def angle: Int  = if((frameCounter / maxAngle) % 2 == 0) {
    frameCounter % maxAngle
  } else {
    maxAngle - frameCounter % maxAngle
  }

  //def angle: Int = frameCounter % (2 * maxAngle)  - maxAngle
  //def compl: Int = (frameCounter / 2 * maxAngle) % maxCompl

  //def compl = maxCompl

  def next: SceneState = SceneState(frameCounter + 1)
}
