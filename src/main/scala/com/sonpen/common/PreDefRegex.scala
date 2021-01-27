package com.sonpen.common

object PreDefRegex {

  val CLIENT_VER_R = raw"\d{1,3}\.\d{1,3}\.\d{1,3}".r

  val DATE_TIME_R = raw"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}\+\d{2}:\d{2}".r
}
