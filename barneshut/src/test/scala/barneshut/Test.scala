package barneshut

import org.scalatest.FunSuite

import scala.collection._


class Test extends FunSuite {
// https://www.coursera.org/learn/parprog1/programming/xGkV0/barnes-hut-simulation

  import FloatOps._

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val b1 = new Body(10f, 1f, 1f, 0f, 0f)
    val b2 = new Body(20f, 2f, 2f, 0f, 0f)
    val b3 = new Body(30f, 3f, 3f, 0f, 0f)
    val l = List(b1,b2,b3)
    val bounderies = new Boundaries
    l.foreach(updateBoundaries(bounderies, _))


    val b11 = new Body(10f, 11f, 1f, 0f, 0f)
    val b21 = new Body(20f, 12f, 2f, 0f, 0f)
    val b31 = new Body(30f, 13f, 3f, 0f, 0f)
    val l1 = List(b1,b2,b3)
    l1.foreach(updateBoundaries(bounderies, _))
    println(bounderies)

    val sm = new SectorMatrix(bounderies, SECTOR_PRECISION)
    l.foreach(sm += _)
    println(sm)
    val sm1 = new SectorMatrix(bounderies, SECTOR_PRECISION)
    l1.foreach(sm1 += _)
    println(sm1)


    println(sm.combine(sm1))
  }

  def updateBoundaries(boundaries: Boundaries, body: Body): Boundaries = {
    boundaries.minX = Math.min(body.x, boundaries.minX)
    boundaries.minY = Math.min(body.y, boundaries.minY)
    boundaries.maxX = Math.max(body.x, boundaries.maxX)
    boundaries.maxY = Math.max(body.y, boundaries.maxY)
    boundaries
  }
}
