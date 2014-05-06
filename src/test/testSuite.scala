package test

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import worksheet._

@RunWith(classOf[JUnitRunner])
class testSuite extends FunSuite {

  test("1=1") {
    assert(1 === 1)
  }

} 
