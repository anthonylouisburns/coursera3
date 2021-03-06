import scalashop._

val w = 4
val h = 3
val src = new Img(w, h)
val dst = new Img(w, h)
src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

def f{

  VerticalBoxBlur.blur(src, dst, 0, 4, 2)

  def check(x: Int, y: Int, expected: Int) =
    assert(dst(x, y) == expected,
      s"(destination($x, $y) should be $expected)")

  check(0, 0, 4)
  check(1, 0, 5)
  check(2, 0, 5)
  check(3, 0, 6)
  check(0, 1, 4)
  check(1, 1, 5)
  check(2, 1, 5)
  check(3, 1, 6)
  check(0, 2, 4)
  check(1, 2, 5)
  check(2, 2, 5)
  check(3, 2, 6)
}

//boxBlurKernel(src, 0, 1, 2)

scalashop.boxBlurKernel(src, 0, 0, 2)


val stepSize = (10 + 1 - 1) / 1

val r = Range(0, 10).by(stepSize)

















