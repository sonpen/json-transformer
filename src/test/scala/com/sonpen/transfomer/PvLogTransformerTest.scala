package com.sonpen.transfomer

import com.sonpen.model.PvLogData
import com.sonpen.transfomer.validator.JsonValidatorMeta.{JsonSchema, KeyNotFoundException, NullConstraintException, REQ}
import org.scalatest.funsuite.AnyFunSuite

class PvLogTransformerTest extends AnyFunSuite{

  test("PvLogTransformer should return empty errors") {

    val transformer = new PvLogTransformer

    val (datas, errors) = transformer.transform(transformer.sampleLog)

    assert(errors.isEmpty)

    datas.foreach(data => println(data))

    assert(datas(0).get("USER_ID") == Some("1234"))
    assert(datas(0).get("OS_TYPE") == Some("android"))
    assert(datas(0).get("CLIENT_VER") == Some("1.0.0"))
    assert(datas(0).get("DEVICE_MODEL") == Some("sm-g900s"))
    assert(datas(0).get("PV_CD") == Some("main"))

    val v = PvLogData.unapply(datas(0))
  }

  test("PvLogTransformer should return NullConstraintException") {
    val log =
      """
        |{
        | "HEADER" : {
        |   "USER_ID" : null,
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

    val transformer = new PvLogTransformer

    val (datas, errors) = transformer.transform(log)

    assert(errors(0).isInstanceOf[NullConstraintException])
  }
}
