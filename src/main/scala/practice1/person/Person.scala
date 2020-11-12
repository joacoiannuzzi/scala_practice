package practice1.person

case class Person(name: String, age: Int, children: List[Person] = Nil) {

  def descendants: List[Person] = this :: children.flatMap(_.descendants)

  def olderAndYounger: (List[Person], List[Person]) = descendants.partition(_.age >= 18)

  def peopleWithoutChildren: Seq[Person] = descendants.filter(_.children.isEmpty)

  def twins =
    descendants
      // lista de los hijos
      .map(person => person.children)
      // filtramos por que tengan mas de 1 hijo
      .filter(list => list.size > 1)
      // agrupamos por edad
      .map(list => list.groupBy(person => person.age))
      // pasamos del map a una lista
      .flatMap(hashmap => hashmap.values)
      // filtramos los grupos por edades iguales, para que queden los mayores a 1 elem
      .filter(list => list.size > 1)

  //  def identicalBrothers: List[(Person, Person)] = {
  //  for {
  //  i <- children
  //  j <- children
  //      if (i.age == j.age && i != j)
  //  } yield (i, j)
  //  }

}


object PersonApp extends App {

  private val john =
    Person("john", 18,
      List(
        Person("peter", 34),
        Person("juan", 34,
          List(
            Person("mike", 45),
            Person("sad", 45,
              List(
                Person("loco", 45)
              )
            )
          )),
        Person("menor", 4),
        Person("otro menor", 12),
        Person("jota", 2,
          List(
            Person("kiosko", 53)
          )
        )
      ))


  //  john.descendants.foreach(println)
  //  john.olderAndYounger._1.foreach(println)
  //  println()
  //  john.olderAndYounger._2.foreach(println)

  //  john.personsWithoutChildren.foreach(println)

  john.twins.foreach(println)

}
