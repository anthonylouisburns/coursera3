package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory


@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    val input = Array[Float](0f, 7f, 8f, 9f)
    downsweep(input, output, 0, Leaf(0, 4, 0))
    println(output.toList)
  }

}