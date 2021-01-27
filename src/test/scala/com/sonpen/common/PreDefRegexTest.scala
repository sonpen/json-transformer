package com.sonpen.common

import org.scalatest.funsuite.AnyFunSuite
import com.sonpen.common.{PreDefRegex => REGEX}

class PreDefRegexTest extends AnyFunSuite {

  test("test regular expression of CLIENT_VER") {

    assert(REGEX.CLIENT_VER_R.unapplySeq("1.0.0").isDefined)

    assert(REGEX.CLIENT_VER_R.unapplySeq("1,0,0").isDefined == false)
    assert(REGEX.CLIENT_VER_R.unapplySeq("1000.2.3").isDefined == false)
  }

  test("test regular expression of DATE_TIME") {

    assert(REGEX.DATE_TIME_R.unapplySeq("2020-01-01T12:00:00.123+09:00").isDefined)

    assert(REGEX.DATE_TIME_R.unapplySeq("2020-01-01T12:00:00.123").isDefined == false)
    assert(REGEX.DATE_TIME_R.unapplySeq("2020-01-01T12:00:00+09:00").isDefined == false)
  }

}
