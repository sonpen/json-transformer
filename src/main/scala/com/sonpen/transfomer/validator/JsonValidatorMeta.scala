package com.sonpen.transfomer.validator

import org.json4s._

import scala.util.matching.Regex

/**
  * JsonValidator 에서 사용하는 클래스나 헬퍼를 정의한다.
  */
object JsonValidatorMeta {

  /***********************/
  /** 1. Key Constraints */
  /***********************/

  sealed trait KeyConstraint {
    def keys: Seq[String]
  }

  /**
    * Key Constraints
    *
    * - REQ              : Required. 나열된 Key 가 존재해야 한다.
    * - OPT              : Optional. 나열된 Key 가 존재하지 않아도 된다.
    * - REQ_AT_LEAST_ONE : 나열된 Key 중 적어도 하나 이상 존재해야 한다.
    * - REQ_ONLY_ONE     : 나열된 Key 중 오직 하나만 존재해야 한다.
    */
  sealed case class REQ(keys: String*) extends KeyConstraint
  sealed case class OPT(keys: String*) extends KeyConstraint
  sealed case class REQ_AT_LEAST_ONE(keys: String*) extends KeyConstraint
  sealed case class REQ_ONLY_ONE(keys: String*) extends KeyConstraint


  /************************/
  /** 2. Null Constraints */
  /************************/

  sealed trait NullConstraint {
    def valueType: ValueType
  }

  /**
    * Null Constraints
    *
    * - NULLABLE : null 을 허용한다.
    * - NOT_NULL : null 을 허용하지 않는다.
    */
  sealed case class NULLABLE(valueType: ValueType) extends NullConstraint
  sealed case class NOT_NULL(valueType: ValueType) extends NullConstraint


  /***********************/
  /** 3. Value Types     */
  /***********************/

  sealed trait ValueType

  /***************************************/
  /** 3-1. Value Types - Primitive Types */
  /***************************************/
  sealed trait PrimitiveType extends ValueType

  /**
    * Primitive Types
    *
    * - STR  : 문자열 타입    (예시: "abc")
    * - INT  : 정수형 타입    (예시: 123)
    * - DBL  : 부동소수점 타입 (예시: 12.34)
    * - BOOL : Boolean 타입  (예시: true, false)
    * - ANY  : 타입 체크를 하지 않는다
    *
    * - STR_LEN : 길이 제약 조건을 추가할 수 있는 문자열 타입. 제약 조건이 없을 경우 null 을 사용한다.
    * - INT_RANGE : 범위 제약 조건을 추가할 수 있는 정수형 타입. 제약 조건이 없을 경우 null 을 사용한다.
    * - DBL_RANGE : 범위 제약 조건을 추가할 수 있는 부동소수점 타입 제약 조건이 없을 경우 null 을 사용한다.
    */
  case object STR extends PrimitiveType
  case object INT extends PrimitiveType
  case object DBL extends PrimitiveType
  case object BOOL extends PrimitiveType
  case object ANY extends PrimitiveType

  sealed case class STR_LEN(min: java.lang.Integer, max: java.lang.Integer) extends PrimitiveType
  sealed case class INT_RANGE(min: java.lang.Long, max: java.lang.Long) extends PrimitiveType
  sealed case class DBL_RANGE(min: java.lang.Double, max: java.lang.Double) extends PrimitiveType

  /***************************************/
  /** 3-2. Value Types - Complex Types */
  /***************************************/
  sealed trait ComplexType extends ValueType

  /**
    * Complex Types
    *
    * - REAL     : 실수(정수 + 부동소수점) 타입    (예시: 123, 123.456)
    * - STR_INT  : 문자열 형식을 포함한 정수형 타입 (예시: 123, "123")
    * - STR_REAL : 문자열 형식을 포함한 실수 타입   (예시: 123, 123.456, "123", "123.456")
    *
    * - REAL_RANGE     : 범위 제약 조건을 추가할 수 있는 실수 타입. 제약 조건이 없을 경우 null 을 사용한다.
    * - STR_INT_RANGE  : 범위 제약 조건을 추가할 수 있는 문자열 형식을 포함한 정수형 타입. 제약 조건이 없을 경우 null 을 사용한다.
    * - STR_REAL_RANGE : 범위 제약 조건을 추가할 수 있는 문자열 형식을 포함한 실수 타입. 제약 조건이 없을 경우 null 을 사용한다.
    * - STR_REGEX      : 정규 표현식 제약 조건을 추가할 수 있는 문자열 타입
    * - ARRAY          : 배열 타입
    */
  case object REAL extends ComplexType
  case object STR_INT extends ComplexType
  case object STR_REAL extends ComplexType

  sealed case class REAL_RANGE(min: java.lang.Double, max: java.lang.Double) extends ComplexType
  sealed case class STR_INT_RANGE(min: java.lang.Long, max: java.lang.Long) extends ComplexType
  sealed case class STR_REAL_RANGE(min: java.lang.Double, max: java.lang.Double) extends ComplexType
  sealed case class STR_REGEX(regex: Regex) extends ComplexType
  sealed case class ARRAY(min: java.lang.Integer, max: java.lang.Integer, valueType: ValueType) extends ComplexType

  /******************************************/
  /** 3-3. Value Types - Domain Constraints */
  /******************************************/
  sealed trait DomainConstraint extends ValueType

  /**
    * Domain Constraints
    *
    * - ALLOW_VAL : 나열된 값만 허용
    * - Y_OR_N    : Y, N 만 허용
    * - ANY_BOOL
    */
  sealed case class ALLOW_VAL(values: Any*) extends DomainConstraint
  case object Y_OR_N extends DomainConstraint
  case object ANY_BOOL extends DomainConstraint

  /*********************************/
  /** 3-4. Value Types - Validator */
  /*********************************/
  sealed trait Validator extends ValueType

  /**
    * Validator
    * 주어진 값에 validator 함수를 적용하여 true, false 를 판별할 수 있는 타입
    *
    * - STR_CHECK    : validator 함수 제약 조건을 추가할 수 있는 문자열 타입
    * - INT_CHECK    : validator 함수 제약 조건을 추가할 수 있는 정수형 타입
    * - DBL_CHECK    : validator 함수 제약 조건을 추가할 수 있는 부동소수점 타입
    * - BOOL_CHECK   : validator 함수 제약 조건을 추가할 수 있는 Boolean 타입
    * - JVALUE_CHECK : validator 함수 제약 조건을 추가할 수 있는 JValue 타입
    */
  sealed case class STR_CHECK(validator: String => Boolean) extends Validator
  sealed case class INT_CHECK(validator: BigInt => Boolean) extends Validator
  sealed case class DBL_CHECK(validator: Double => Boolean) extends Validator
  sealed case class BOOL_CHECK(validator: Boolean => Boolean) extends Validator
  sealed case class JVALUE_CHECK(validator: JValue => Boolean) extends Validator

  /***********************************/
  /** 3-5. Value Types - Json Schema */
  /***********************************/

  /**
    * JSON schema 를 정의하여 아래와 같이 사용한다.
    *
    * JsonSchema(
    *   (KeyConstraint(...), NullConstraint(ValueType)),
    *   ...
    * )
    */
  sealed case class JsonSchema(rules: (KeyConstraint, NullConstraint)*) extends ValueType


  /***********************/
  /** 4. Exceptions      */
  /***********************/
  /**
    * JsonValidator 에서 발생하는 모든 예외의 Supertype
    */
  sealed abstract class JsonValidatorException extends Exception {
    def depth: Seq[String]
  }

  /**
    * Required 로 지정된 Key 가 존재하지 않는 경우 발생하는 Exception
    * @param depth JSON 위치
    * @param expectedKeys 지정된 Key 리스트
    * @param actualKeys 실제 존재하는 Key 리스트
    */
  sealed case class KeyNotFoundException(depth: Seq[String], expectedKeys: Seq[String], actualKeys: Seq[String]) extends JsonValidatorException

  /**
    * REQ_ONLY_ONE 으로 지정된 Key 가 2개 이상 존재하는 경우 발생하는 Exception
    * @param depth JSON 위치
    * @param expectedKeys 지정된 Key 리스트
    * @param actualKeys 실제 존재하는 Key 리스트
    */
  sealed case class RedundantKeyException(depth: Seq[String], expectedKeys: Seq[String], actualKeys: Seq[String]) extends JsonValidatorException

  /**
    * NOT_NULL 로 지정된 값이 null 일 경우 발생하는 Exception
    * @param depth JSON 위치
    */
  sealed case class NullConstraintException(depth: Seq[String]) extends JsonValidatorException

  /**
    * 값의 타입이 맞지 않는 경우 발생하는 Exception
    * @param depth JSON 위치
    * @param expectedType 기대했던 타입
    * @param jValue 실제 값
    */
  sealed case class TypeErrorException(depth: Seq[String], expectedType: String, jValue: JValue) extends JsonValidatorException

  /**
    * 문자열 길이 제약 조건을 위배하는 경우 발생하는 Exception
    * @param depth JSON 위치
    * @param expectedLength 길이 제약 조건
    * @param jString 실제 값
    */
  sealed case class StringLengthException(depth: Seq[String], expectedLength: String, jString: JString) extends JsonValidatorException

  /**
    * 정수 혹은 부동소수점 범위 제약 조건을 위배하는 경우 발생하는 Exception
    * @param depth JSON 위치
    * @param expectedRange 범위 제약 조건
    * @param jValue 실제 값
    */
  sealed case class NumberRangeException(depth: Seq[String], expectedRange: String, jValue: JValue) extends JsonValidatorException

  /**
    * 배열 길이 제약 조건을 위배하는 경우 발생하는 Exception
    * @param depth JSON 위치
    * @param expectedSize 길이 제약 조건
    * @param jArray 실제 값
    */
  sealed case class ArraySizeException(depth: Seq[String], expectedSize: String, jArray: JArray) extends JsonValidatorException

  /**
    * 나열된 값 이외의 값이 존재하는 경우 발생하는 Exception
    * @param depth JSON 위치
    * @param expectedValues 나열된 값
    * @param jValue 실제 값
    */
  sealed case class DomainConstraintException(depth: Seq[String], expectedValues: Seq[Any], jValue: JValue) extends JsonValidatorException

  /**
    * validator 함수를 적용했을 때 false 인 경우 발생하는 Exception
    * @param depth JSON 위치
    * @param jValue 실제 값
    */
  sealed case class ValidationFailedException(depth: Seq[String], jValue: JValue) extends JsonValidatorException


  /***********************/
  /** 5. Toolkit      */
  /***********************/

  /**
    * JValue Toolkit
    * @param jValue
    */
  sealed implicit class JValueToolkit(jValue: JValue) {
    def toStringSafe = jValue match {
      case JNothing => ""
      case JNull => ""
      case jValue => jValue.values.toString
    }

    def getOneOf(keys: String*) = keys.map(jValue \ _).find {
      case JArray(values: List[JValue]) => values.nonEmpty
      case JObject(values: List[(String, JValue)]) => values.nonEmpty
      case jValue => jValue != JNothing && jValue != JNull
    }.get
  }


}
