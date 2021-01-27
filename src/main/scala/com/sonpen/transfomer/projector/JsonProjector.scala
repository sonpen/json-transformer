package com.sonpen.transfomer.projector

import scala.collection.mutable.HashMap

object JsonProjector {
  def apply() = new JsonProjector
}

class JsonProjector {

  def project(rules: Map[String, String => String]): Map[String, String] = {

    val data = new HashMap[String, String]

    rules.foreach{ case (key, func) => data += (key -> func(key))}

    data.toMap
  }

}
