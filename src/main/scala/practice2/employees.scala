package practice2

import scala.::


/*

(2) Dado un árbol con la jerarquía de trabajadores de una empresa:

case class Employee(name: String,
                    sinceYear: Int,
                    dependents: List[Employee])

val president = Employee(“...”, dependents = List(...))

Implementar funciones para obtener:

  a) Cantidad de empleados de la empresa
  b) Lista de empleados que tienen gente a cargo
  c) Jefes que hayan dejado pasar más de 3 años entre la contratación de 2 de sus empleados

 */

case class Employee(name: String,
                    sinceYear: Int,
                    dependents: List[Employee] = Nil
                   ) {

  def allEmployees: List[Employee] = this :: dependents.flatMap(_.allEmployees)

  def quantityOfEmployees = allEmployees.size

}


object EmployeeFunctions extends App {


}


object EmployeeTest extends App {

  val president =
    Employee("Jorge", 2007,
      List(
        Employee("pepe", 1227),
        Employee("johnny", 2014,
          List(
            Employee("johnny", 2045)
          )
        )
      )
    )

//  president.allEmployees.foreach(println)
  println(president.quantityOfEmployees)


}
