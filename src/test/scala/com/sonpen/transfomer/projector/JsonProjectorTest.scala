package com.sonpen.transfomer.projector

import org.json4s._
import org.json4s.jackson.JsonMethods
import org.scalatest.funsuite.AnyFunSuite

class JsonProjectorTest extends AnyFunSuite{

  val jsonString =
    """
      |{
      | "first_name" : "first",
      | "last_name" : "last",
      | "age" : 25,
      | "weight" : 70.5,
      | "family" : [
      |   {
      |     "family_first_name" : "f",
      |     "family_last_name" : "l",
      |     "family_age" : 15,
      |     "family_weight" : 71.0
      |   },
      |   {
      |     "family_first_name" : "ff",
      |     "family_last_name" : null,
      |     "family_age" : 20,
      |     "family_weight" : 48.5
      |   }
      | ],
      | "str_real" : "12.5",
      | "int_array" : [1,2,3,4]
      |}
    """.stripMargin

  val jValue = JsonMethods.parse(jsonString)

  test("Projector should return Map") {

    import com.sonpen.transfomer.validator.JsonValidatorMeta._

    def copyData(key: String): String = (jValue \ key).toStringSafe

    val dataMap = (new JsonProjector()).project(
      Map(("first_name" -> copyData),
        ("last_name" -> copyData),
        ("age" -> copyData),
        ("weight" -> copyData)
      )
    )

    assert(dataMap.get("first_name") == Option("first"))
    assert(dataMap.get("last_name") == Option("last"))
    assert(dataMap.get("age") == Option("25"))
    assert(dataMap.get("weight") == Option("70.5"))
  }

  test("Projector should return Map about family") {

    import com.sonpen.transfomer.validator.JsonValidatorMeta._

    def copyData(key: String): String = (jValue \ key).toStringSafe

    val dataMaps = (jValue \ "family").children.map{ familys =>
      def copyFamily (key: String): String = (familys \ key).toStringSafe

      (new JsonProjector()).project(
        Map(("first_name" -> copyData),
          ("last_name" -> copyData),
          ("age" -> copyData),
          ("weight" -> copyData),
          ("family_first_name" -> copyFamily),
          ("family_last_name" -> copyFamily),
          ("family_age" -> copyFamily),
          ("family_weight" -> copyFamily)
        )
      )
    }

    dataMaps.foreach(dataMap => println(dataMap))
    assert(dataMaps.size == 2)

  }

}
