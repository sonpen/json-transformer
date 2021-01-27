package com.sonpen.transfomer.validator

import com.sonpen.transfomer.validator.JsonValidatorMeta._
import org.json4s.jackson.JsonMethods
import org.scalatest.funsuite.AnyFunSuite

class JsonValidatorTest extends AnyFunSuite {

  val jsonString =
    """
      |{
      | "first_name" : "first",
      | "last_name" : "last",
      | "age" : 25,
      | "weight" : 70.5,
      | "family" : [
      |   {
      |     "first_name" : "f",
      |     "last_name" : "l",
      |     "age" : 15,
      |     "weight" : 71.0
      |   },
      |   {
      |     "first_name" : "ff",
      |     "last_name" : null,
      |     "age" : 20,
      |     "weight" : 48.5
      |   }
      | ],
      | "str_real" : "12.5",
      | "int_array" : [1,2,3,4]
      |}
      """.stripMargin

  val jValue = JsonMethods.parse(jsonString)

  test("Sequence of error should have size 0") {
    val validator = JsonValidator(
      JsonSchema(
        (REQ("first_name"), NOT_NULL(STR)),
        (REQ("last_name"), NOT_NULL(STR_LEN(0, 30))),
        (REQ("age"), NOT_NULL(INT_RANGE(0L,100L))),
        (REQ("weight"), NOT_NULL(DBL_RANGE(0.0,1000.0))),
        (REQ("family"), NOT_NULL(ARRAY(null, null, JsonSchema(
          (REQ("first_name"), NOT_NULL(STR)),
          (REQ("last_name"), NULLABLE(STR))
        )))),
        (REQ("str_real"), NOT_NULL(STR_REAL_RANGE(0.0, 100.0))),
        (REQ("int_array"), NOT_NULL(ARRAY(null, null, INT)))
      )
    )

    val errors = validator.check(jsonString)
    assert(errors.size == 0)
  }

  test("It should produce KeyNotFoundException") {
    val validator = JsonValidator(
      JsonSchema(
        (REQ("key"), NOT_NULL(STR))
      )
    )

    val errors = validator.check(jsonString)
    assert(errors.size == 1)
    assertThrows[KeyNotFoundException] {
      println(JsonValidator.dumpError(errors, jsonString))
      throw errors.head
    }
  }

  test("It should produce RedundantKeyException") {
    val validator = JsonValidator(
      JsonSchema(
        (REQ_ONLY_ONE("first_name", "last_name" ), NOT_NULL(STR))
      )
    )

    val errors = validator.check(jsonString)
    assert(errors.size == 1)
    assertThrows[RedundantKeyException] {
      println(JsonValidator.dumpError(errors, jsonString))
      throw errors.head
    }
  }

  test("It should produce TypeErrorException") {
    val validator = JsonValidator(
      JsonSchema(
        (REQ("first_name"), NOT_NULL(STR_REAL_RANGE(0.0, 100.0)))
      )
    )

    val errors = validator.check(jsonString)
    assert(errors.size == 1)
    assertThrows[TypeErrorException] {
      println(JsonValidator.dumpError(errors, jsonString))
      throw errors.head
    }
  }

}
