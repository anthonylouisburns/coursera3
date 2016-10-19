package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    balance(chars, 0, 0)
  }

  def balance(chars: Array[Char],i:Int, arg:Int): Boolean = {
    if(arg < 0) false
    else if(i == chars.size){
      if(arg == 0) true
      else false
    }
    else{
      val j = i + 1
      if(chars(i) == '(') return balance(chars, j, arg + 1)
      else if(chars(i) == ')') return balance(chars, j, arg - 1)
      else balance(chars, j, arg)
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverseSerial(idx: Int, until: Int, arg1: Int, arg2: Int):(Int, Int)  = {
      if(idx == until) (arg1, arg2)
      else if(chars(idx) == '(') (arg1 + 1, arg2)
      else if(chars(idx) == ')') (arg1 - 1, math.min(arg1-1, arg2))
      else traverseSerial(idx + 1, until, arg1, arg2)
    }

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int):(Int, Int) = {
      val span: Int = until - idx
      if(span <= Math.max(threshold,1)) traverseSerial(idx, until, arg1, arg2)
      else{
        val half = idx + (span/2)
        val (a1, b1) = traverse(idx, half, 0, 0)
        val (a2, b2) = traverse(half, until, 0, 0)
        (a1 + a2, Math.min(a1+b2, b1))
      }
    }

    def p[A](x:A):A={
      println(x)
      x
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      traverse(from, until, 0, 0)
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
