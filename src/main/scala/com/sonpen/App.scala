package com.sonpen

import com.sonpen.transfomer.validator.JsonValidator
import com.sonpen.transfomer.validator.JsonValidatorMeta._
import org.json4s.JValue
import org.json4s.JsonAST.JArray
import org.json4s.jackson.JsonMethods

object App {

  def main(args: Array[String]) = {

    val jsonString =
      """
        |{
        | "name" : "son",
        | "age" : 125,
        | "weight" : 70.5,
        | "family" : [
        |   {
        |     "name" : "hyun",
        |     "age" : 15,
        |     "weight" : 71.0
        |   },
        |   {
        |     "name" : "eun",
        |     "age" : 20,
        |     "weight" : 48.5
        |   }
        | ],
        | "foo" : []
        |}
        |
        |
        |
      """.stripMargin

    val jValue = JsonMethods.parse(jsonString)

    val validator = JsonValidator(JsonSchema(
      (REQ("name"), NOT_NULL(STR_LEN(0, 30))),
      (REQ("age"), NOT_NULL(INT_RANGE(0L,100L)))
    ))

    val errors = validator.check(jsonString)

    val errorStrings = JsonValidator.dumpError(errors, jsonString)
    println(errorStrings)

    (jValue \ "family") match {
      case JArray(values: List[JValue]) =>
        println(values.view.zipWithIndex)
      case _ =>
        println("not matched")
    }
  }

}
