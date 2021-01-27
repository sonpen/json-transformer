val l = List(1,2,3)
val a = Array(1,2,3)
//l(1) = 2    // Error!!
a(1) = 200    // Success


val m = Map(1->"one", 2->"two", 3->"three")

m.foreach{x => println(s"key: ${x._1}, value: ${x._2}")}
m.foreach{case (k,v) => println(s"key: $k, value: $v")}

val list = List((4, "four"), (5, "five"))
list.foreach{x => println(s"key: ${x._1}, value: ${x._2}")}
list.foreach{case (k,v) => println(s"key: $k, value: $v")}
