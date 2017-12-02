package com.antonfagerberg

import scala.io.{Codec, Source}

object Input {
  def getLines(url: String): List[String] = {
    val resource =
      getClass
        .getClassLoader
        .getResource(url)

    Source.fromURL(resource)(Codec.UTF8).getLines().toList
  }
}
