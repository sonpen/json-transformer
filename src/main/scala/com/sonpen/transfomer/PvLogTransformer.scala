package com.sonpen.transfomer

import com.sonpen.transfomer.projector.JsonProjector
import com.sonpen.transfomer.validator.JsonValidator
import com.sonpen.transfomer.validator.JsonValidatorMeta._
import org.json4s.jackson.JsonMethods

class PvLogTransformer {
  val sampleLog : String =
    """
      |{
      | "HEADER" : {
      |   "USER_ID" : 1234,
      |   "OS_TYPE" : "android",
      |   "CLIENT_VER" : "1.0.0",
      |   "DEVICE_MODEL" : "sm-g900s"
      | },
      | "BODY" : {
      |   "PV" : [
      |     {
      |       "PV_CD":"main",
      |       "START_TIME":"2020-12-22T10:12:34.888+09:00",
      |       "END_TIME":"2020-12-22T10:12:54.888+09:00"
      |     },
      |     {
      |       "PV_CD":"menu",
      |       "START_TIME":"2020-12-22T10:12:54.888+09:00",
      |       "END_TIME":"2020-12-22T10:13:54.888+09:00"
      |     }
      |   ]
      | }
      |}
    """.stripMargin

  val pvSchema = JsonSchema(
    (REQ("HEADER"), NOT_NULL(JsonSchema(
      (REQ("USER_ID"), NOT_NULL(INT)),
      (REQ("OS_TYPE"), NOT_NULL(STR)),
      (REQ("CLIENT_VER"), NOT_NULL(STR_REGEX("[0-9]{1,3}.[0-9]{1,3}.[0-9]{1,3}".r))),
      (REQ("DEVICE_MODEL"), NOT_NULL(STR_LEN(0, 32)))
    ))),
    (REQ("BODY"), NOT_NULL(JsonSchema(
      (REQ("PV"), NOT_NULL(ARRAY(null, null, JsonSchema(
        (REQ("PV_CD"), NOT_NULL(STR_LEN(0, 128))),
        (REQ("START_TIME"), NOT_NULL(STR)),
        (REQ("END_TIME"), NOT_NULL(STR)))))
      )
    )))
  )

  def transform(line: String): (Seq[Map[String, String]], Seq[Exception]) = {

    val errors: Seq[Exception] = JsonValidator(pvSchema).check(line)

    val datas = if (errors.isEmpty == false )
      Seq[Map[String, String]]()
    else {
      val jValue = JsonMethods.parse(line)

      def getInHeader(key: String) : String = (jValue \ "HEADER" \ key).toStringSafe

      (jValue \ "BODY" \ "PV").children.map{ pv =>
        def getInPv(key: String) : String = (pv \ key).toStringSafe

        JsonProjector().project(Map(
          ("USER_ID" -> getInHeader),
          ("OS_TYPE" -> getInHeader),
          ("CLIENT_VER" -> getInHeader),
          ("DEVICE_MODEL" -> getInHeader),
          ("PV_CD" -> getInPv),
          ("START_TIME" -> getInPv),
          ("END_TIME" -> getInPv)
        ))
      }
    }

    (datas, errors)
  }
}
