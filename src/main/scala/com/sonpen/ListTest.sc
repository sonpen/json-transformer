1 :: 2 :: 3 :: Nil

// immutable List
var list = List[Int]()
list = 2 :: list
list = list :+ 3
list = 1 +: list


import scala.collection.mutable.ListBuffer

val lb = ListBuffer[Int]()

lb.append(5)
lb += 6

4 +=: lb
lb.prepend(3)

lb



val listBuffer = ListBuffer[Int](1,2,3,4,5,6,7,8,9)
val itemToDel = 4
val listToDel = List(1,2,3)
// -, -- 는 immutable 한 operator 이므로 원래 객체가 변경되지 않는다는 것을 보여줌
println(s"listBuffer=$listBuffer, After delete => ${listBuffer - itemToDel}, $listBuffer")
println(s"listBuffer=$listBuffer, After delete => ${listBuffer -- listToDel}, $listBuffer")
// -=, --= 는 mutable 한 operator 이므로 원래 객체가 변경됨
println(s"listBuffer=$listBuffer, After delete => ${listBuffer -= itemToDel}, $listBuffer")
println(s"listBuffer=$listBuffer, After delete => ${listBuffer --= listToDel}, $listBuffer")



