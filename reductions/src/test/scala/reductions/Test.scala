package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {
  println(countChange(0,(List())))
  println(countChange(0,(List(1, 2, 3))))
  println(countChange(0,(List.range(1, 100))))
}