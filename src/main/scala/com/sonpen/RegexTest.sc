raw"H\w*".r.findAllIn("Hello World. H Hi Everyone").mkString(",")
raw"H\w*".r.replaceAllIn("Hello World. H Hi Everyone", "*")


"3.0.0".matches(raw"\d{1,3}\.\d{1,3}\.\d{1,3}")
raw"\d{1,3}\.\d{1,3}\.\d{1,3}".r.unapplySeq("3.0.0").isDefined

val pattern = raw"(\d{1,3})\.(\d{1,3})\.(\d{1,3})".r
pattern.unapplySeq("3.0.0")
"3.0.0" match {
  case pattern(majorNo, minorNo, patchNo) =>
    println(s"$majorNo, $minorNo")
}


