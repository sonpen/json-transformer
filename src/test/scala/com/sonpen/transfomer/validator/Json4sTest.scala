package com.sonpen.transfomer.validator

import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods
import org.scalatest.funsuite.AnyFunSuite

/**
  * Created by 1109806 on 2020/12/11.
  */
class Json4sTest extends AnyFunSuite{

  val jsonString: String =
    """
      |{
      | "foo":1,
      | "bar":null
      | }
    """.stripMargin

  val jValue = JsonMethods.parse(jsonString)

  test("Value of foo should be JInt(1)") {
    assert(jValue \ "foo" == JInt(1))
  }
  test("Value of bar should be JNull") {
    assert(jValue \ "bar" == JNull)
  }
  test("Value of baz should be JNothing") {
    assert(jValue \ "foobar" == JNothing)
  }

  test("Multi condition case statement should return JInt(1)") {
    val foo = jValue \ "foo"
    val v = foo match {
      case intOrDouble @ (_: JInt | _: JDouble) =>
        intOrDouble
      case _ =>
        jValue
    }

    assert(v == JInt(1))
  }

}
