package com.sonpen.transfomer.validator

import com.sonpen.transfomer.validator.JsonValidatorMeta._
import org.json4s._
import org.json4s.jackson.JsonMethods

object JsonValidator {

  def apply(valueType: ValueType, collectMultipleError: Boolean = false): JsonValidator = new JsonValidator(valueType, collectMultipleError)

  /**
    * 발생한 Exception 을 문자열로 변환한다.
    *
    * @param errors 발생한 Exception
    * @param jsonString 원본 JSON 문자열
    * @return 변환된 예러 문자열
    */
  def dumpError(errors: Seq[Exception], jsonString: String): Seq[String] = {

    errors.map(error => (error, error.getClass.getSimpleName)).map {
      // KeyNotFoundException
      case (error: KeyNotFoundException, name: String) =>
        val expected = error.expectedKeys.mkString("[", ",", "]")
        val actual = error.actualKeys.mkString("[", ",", "]")
        s"${name} (depth=${error.depth.mkString(".")} expected=${expected} actual=${actual})"

      // RedundantKeyException
      case (error: RedundantKeyException, name: String) =>
        val expected = error.expectedKeys.mkString("[", ",", "]")
        val actual = error.actualKeys.mkString("[", ",", "]")
        s"${name} (depth=${error.depth.mkString(".")} expected=${expected} actual=${actual})"

      // NullConstraintException
      case (error: NullConstraintException, name: String) =>
        s"${name} (depth=${error.depth.mkString(".")}"

      // TypeErrorException
      case (error: TypeErrorException, name: String) =>
        s"${name} (depth=${error.depth.mkString(".")} expected=${error.expectedType} actual=${error.jValue})"

      // StringLengthException
      case (error: StringLengthException, name: String) =>
        s"${name} (depth=${error.depth.mkString(".")} expected=${error.expectedLength} actual=[${error.jString.values.size}]${error.jString})"

      // NumberRangeException
      case (error: NumberRangeException, name: String) =>
        s"${name} (depth=${error.depth.mkString(".")} expected=${error.expectedRange} actual=${error.jValue})"

      // ArraySizeException
      case (error: ArraySizeException, name: String) =>
        s"${name} (depth=${error.depth.mkString(".")} expected=${error.expectedSize} actual=${error.jArray.values.size})"

      // DomainConstraintException
      case (error: DomainConstraintException, name: String) =>
        val expected = error.expectedValues.mkString("[", ",", "]")
        s"${name} (depth=${error.depth.mkString(".")} expected=${expected} actual=${error.jValue})"

      // ValidationFailedException
      case (error: ValidationFailedException, name: String) =>
        s"${name} (depth=${error.depth.mkString(".")} actual=${error.jValue})"

      // JsonValidatorException
      case (error: JsonValidatorException, name: String) =>
        s"${name} (depth=${error.depth.mkString(".")})"

      case (error: Exception, name: String) =>
        s"${name} "
    }.map(errorStr => s"${errorStr} @ ${jsonString}")
  }
}

/**
  * JsonValidator
  *
  * @param valueType 유효성 검증 수행하는 기준이 되는 ValueType
  * @param collectMultipleError 에러 탐지시 유효성 검증을 계속하여 모든 에러를 수집할지 여부
  */
class JsonValidator(valueType: ValueType, collectMultipleError: Boolean) {

  /**
    * 검증을 통과했음을 의미하는 상수
    */
  protected final val pass: Seq[JsonValidatorException] = Seq()

  /**
    * json 형식 로그의 유효성을 검증한다
    *
    * @param logString 검증할 json 로그
    * @return 발생한 예외들
    */
  def check(logString: String): Seq[Exception] = {
    try {
      val jsonString = logString.substring(logString.indexOf('{'))

      val jValue = JsonMethods.parse(jsonString)

      checkValueType(valueType, jValue, Seq[String]())
    }
    catch {
      case error: Exception =>
        Seq(error)
    }
  }

  /**
    * 발생한 예외를 collectMultipleError 에 따라서 처리한다.
    *
    * collectionMultipleError 가
    * true 인 경우, 발생한 예외를 Seq 로 담아서 리턴하여, 동일 line 의 json 로그의 유효성을 검증하는 반면
    * false 인 경우 즉시 예외를 throw 하여 다음 line 의 json 로그의 유효성을 검증한다.
    *
    * @param error 발생한 예외
    * @return 발생한 예외
    */
  protected final def raiseError(error: JsonValidatorException): Seq[JsonValidatorException] = {
    if( collectMultipleError )
      Seq(error)
    else
      throw error
  }

  /**
    * valueType 에 따라 jValue 의 유효성을 검증한다.
    *
    * @param valueType 유효성 검증의 기준이 되는 ValueType
    * @param jValue 유효성 검증 대상이 되는 JValue
    * @param depth 현재 탐색중인 JSON 위치
    * @return 발생한 예외들
    */
  protected final def checkValueType(valueType: ValueType, jValue: JValue, depth: Seq[String]): Seq[JsonValidatorException] = {

    valueType match {

      // JSON Schema
      case jsonSchema: JsonSchema =>
        jValue match {
          case jObject: JObject => checkJsonSchema(jsonSchema, jObject, depth)
          case _ => raiseError(TypeErrorException(depth, valueType.toString.takeWhile(_ != '('), jValue))
        }

      // String. 문자열 타입
      case STR =>
        jValue match {
          case JString(_) => pass
          case _ => raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // Integer. 정수형 타입
      case INT =>
        jValue match {
          case JInt(_) => pass
          case _ => raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // Double. 부동소수점 타입
      case DBL =>
        jValue match {
          case JDouble(_) => pass
          case _ => raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // Boolean
      case BOOL =>
        jValue match {
          case JBool(_) => pass
          case _ => raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // 타입 체크를 하지 않는다
      case ANY =>
        pass

      // 길이 제약 조건을 추가할 수 있는 문자열 타입
      case STR_LEN(min, max) =>
        jValue match {
          case JString(value) =>
            if( (min == null || value.size >= min )&& (max == null || value.size <= max))
              pass
            else
              raiseError(StringLengthException(depth, s"$min ~ $max", jValue.asInstanceOf[JString]))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // 범위 제약 조건을 추가할 수 있는 정수형 타입
      case INT_RANGE(min, max) =>
        jValue match {
          case JInt(value) =>
            if ((min == null || value >= BigInt(min)) && (max == null || value <= BigInt(max)))
              pass
            else
              raiseError(NumberRangeException(depth, s"$min ~ $max", jValue))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // 범위 제약 조건을 추가할 수 있는 부동소수점 타입
      case DBL_RANGE(min, max) =>
        jValue match {
          case JDouble(value) =>
            if ((min == null || value >= min) && (max == null || value <= max))
              pass
            else
              raiseError(NumberRangeException(depth, s"$min ~ $max", jValue))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // 실수(정수 + 부동소수점) 타입
      case REAL =>
        checkValueType(REAL_RANGE(null, null), jValue, depth)

      // 문자열 형식을 포함한 정수형 타입
      case STR_INT =>
        checkValueType(STR_INT_RANGE(null, null), jValue, depth)

      // 문자열 형식을 포함한 실수 타입
      case STR_REAL =>
        checkValueType(STR_REAL_RANGE(null, null), jValue, depth)

      // 범위 제약 조건을 추가할 수 있는 실수 타입
      case REAL_RANGE(min, max) =>
        jValue match {
          case JInt(value) =>
            checkValueType(DBL_RANGE(min, max), JDouble(value.toDouble), depth)
          case JDouble(_) =>
            checkValueType(DBL_RANGE(min, max), jValue, depth)
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))

        }

      // 범위 제약 조건을 추가할 수 있는 문자열 형식을 포함한 정수형 타입
      case STR_INT_RANGE(min, max) =>
        jValue match {
          case JInt(_) =>
            checkValueType(INT_RANGE(min, max), jValue, depth)
          case JString(value) =>
            try {
              checkValueType(INT_RANGE(min, max), JInt(BigInt(value)), depth)
            }
            catch {
              case _: Exception =>
                raiseError(TypeErrorException(depth, valueType.toString, jValue))
            }
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // 범위 제약 조건을 추가할 수 있는 문자열 형식을 포함한 실수 타입
      case STR_REAL_RANGE(min, max) =>
        jValue match {
          case JInt(_) | JDouble(_) =>
            checkValueType(REAL_RANGE(min, max), jValue, depth)
          case JString(value) =>
            try {
              checkValueType(REAL_RANGE(min, max), JDouble(value.toDouble), depth)
            }
            catch {
              case _: Exception =>
                raiseError(TypeErrorException(depth, valueType.toString, jValue))
            }
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // 정규 표현식 제약 조건을 추가할 수 있는 문자열 타입
      case STR_REGEX(regex) =>
        jValue match {
          case JString(value) =>
            if( regex.unapplySeq(value).isDefined )
              pass
            else
              raiseError(TypeErrorException(depth, valueType.toString, jValue))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // 배열 타입
      case ARRAY(min, max, valueType) =>
        jValue match {
          case JArray(values: List[JValue]) =>
            if ((min == null || values.size >= min) && (max == null || values.size <= max)) {
              val lastDepth: String = if( depth.isEmpty ) "" else depth.last
              val depthDropLast: Seq[String] = depth.dropRight(1)
              values.zipWithIndex.flatMap{ case (subJValue: JValue, index: Int) =>
                val lastDepthWithIndex: String = s"${lastDepth}[${index}]"

                checkValueType(valueType, subJValue, depthDropLast :+ lastDepthWithIndex)
              }
            }
            else
              raiseError(ArraySizeException(depth, s"${min} ~ ${max}", jValue.asInstanceOf[JArray]))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // 나열된 값만 허용
      case ALLOW_VAL(values @ _*) =>
        if (values.contains(jValue.values))
          pass
        else
          raiseError(DomainConstraintException(depth, values, jValue))

      // Y, N 만 허용
      case Y_OR_N =>
        checkValueType(ALLOW_VAL("Y", "N"), jValue, depth)

      //
      case ANY_BOOL =>
        checkValueType(ALLOW_VAL(true, false, 1, 0, "1", "0"), jValue, depth)

      // validator 함수 제약 조건을 추가할 수 있는 문자열 타입
      case STR_CHECK(func) =>
        jValue match {
          case JString(value) =>
            if (func(value))
              pass
            else
              raiseError(ValidationFailedException(depth, jValue))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // validator 함수 제약 조건을 추가할 수 있는 정수형 타입
      case INT_CHECK(func) =>
        jValue match {
          case JInt(value) =>
            if (func(value))
              pass
            else
              raiseError(ValidationFailedException(depth, jValue))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // validator 함수 제약 조건을 추가할 수 있는 부동소수점 타입
      case DBL_CHECK(func) =>
        jValue match {
          case JDouble(value) =>
            if (func(value))
              pass
            else
              raiseError(ValidationFailedException(depth, jValue))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // validator 함수 제약 조건을 추가할 수 있는 Boolean 타입
      case BOOL_CHECK(func) =>
        jValue match {
          case JBool(value) =>
            if (func(value))
              pass
            else
              raiseError(ValidationFailedException(depth, jValue))
          case _ =>
            raiseError(TypeErrorException(depth, valueType.toString, jValue))
        }

      // validator 함수 제약 조건을 추가할 수 있는 JValue 타입
      case JVALUE_CHECK(func) =>
        if (func(jValue))
          pass
        else
          raiseError(ValidationFailedException(depth, jValue))

      //
      case _ =>
        raiseError(TypeErrorException(depth, valueType.toString, jValue))
    }
  }

  /**
    * jsonSchema 기반으로 jObject 의 유효성을 검증한다.
    *
    * 유효성은 다음과 같은 순서로 진행한다
    * 1. KeyConstraint 확인
    * 2. NullConstraint 확인
    * 3. ValueType 확인
    *
    * @param jsonSchema 유효성 검사의 기준이 되는 JsonSchema
    * @param jObject 유효성 검사 대상이 되는 JObject
    * @param depth 현재 탐색중인 JSON 위치
    * @return 발생한 예외들
    */
  protected final def checkJsonSchema(jsonSchema: JsonSchema, jObject: JObject, depth: Seq[String]): Seq[JsonValidatorException] = {

    jsonSchema.rules.flatMap {

      case (keyConstraint, nullConstraint) =>
        val expectedKeys = keyConstraint.keys
        val actualKeys = expectedKeys.filter(key => jObject \ key != JNothing )

        // 1. KeyConstraint 확인
        val keyConstraintErrors = checkKeyConstraint(keyConstraint, expectedKeys, actualKeys, depth)

        keyConstraintErrors ++ actualKeys.flatMap { key =>
          val jValue: JValue = jObject \ key
          val nextDepth = depth :+ key

          // 2. NullConstraint 확인
          val nullConstraintErrors = checkNullConstraint(nullConstraint, jValue, nextDepth)

          // 3. ValueType 확인
          val valueTypeErrors = if(jValue != JNull) checkValueType(nullConstraint.valueType, jValue, depth) else pass

          nullConstraintErrors ++ valueTypeErrors
        }
    }
  }

  /**
    * KeyConstraint 확인
    *
    * @param keyConstraint 유효성 검사의 기준이 되는 KeyConstraint
    * @param expectedKeys 유효성 검사의 대상이 되는 Key
    * @param actualKeys 실제 존재하는 Key
    * @param depth 현재 탐색중인 JSON 위치
    * @return 발생한 예외들
    */
  protected final def checkKeyConstraint(keyConstraint: KeyConstraint, expectedKeys: Seq[String], actualKeys: Seq[String], depth: Seq[String]): Seq[JsonValidatorException] = {

    keyConstraint match {

      // Required. 나열된 모든 키가 존재해야 함.
      case REQ(_*) =>
        if(actualKeys.size == expectedKeys.size)
          pass
        else
          raiseError(KeyNotFoundException(depth, expectedKeys, actualKeys))

      // Optional
      case OPT(_*) =>
        pass

      // 나열된 키 중 적어도 하나 이상 존재해야 함.
      case REQ_AT_LEAST_ONE(_*) =>
        if( actualKeys.size > 0 )
          pass
        else
          raiseError(KeyNotFoundException(depth, expectedKeys, actualKeys))

      // 나열된 키 중 오직 하나만 존재해야 함.
      case REQ_ONLY_ONE(_*) =>
        if( actualKeys.size == 0 )
          raiseError(KeyNotFoundException(depth, expectedKeys, actualKeys))
        else if( actualKeys.size == 1)
          pass
        else
          raiseError(RedundantKeyException(depth, expectedKeys, actualKeys))
    }
  }

  /**
    * NullConstraint 확인
    *
    * @param nullConstraint 유효성 검사의 기준이 되는 NullConstraint
    * @param jValue 유효성 검사의 대상이되는 JValue
    * @param depth 현재 탐색중인 JSON 위치
    * @return 발생한 예외
    */
  protected final def checkNullConstraint(nullConstraint: NullConstraint, jValue: JValue, depth: Seq[String]): Seq[JsonValidatorException] = {

    nullConstraint match {
      case NOT_NULL(_) =>
        if( jValue == JNull )
          raiseError(NullConstraintException(depth))
        else
          pass
      case NULLABLE(_) =>
        pass
    }
  }

}
