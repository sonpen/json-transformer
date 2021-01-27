package com.sonpen.model

object PvLogData {
  def unapply(arg: Map[String, String]): PvLogData = {
    PvLogData(arg("USER_ID").toInt)
  }
}

case class PvLogData(userId: Int)
