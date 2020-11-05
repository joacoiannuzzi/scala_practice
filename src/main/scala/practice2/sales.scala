package practice2


/*

(1) Dada una lista con datos de ventas:

case class Sales(date: String, amounts: List[Double])

val all = List(
  Sales("2015-05-05", List(1200, 233)),
  Sales("2015-05-06", List(3400, 24, 43)),
...
)

Implementar funciones para obtener:

  a) La lista de días en que las ventas superaron una cantidad “x” de dinero.

  b) Los días en los que se realizaron el doble de ventas que el día anterior.

  c) Los N días consecutivos en donde se obtuvieron las mejores ventas. N debe ser un parámetro de la función.

 */

case class Sales(date: String, amounts: List[Double])


object SalesFunctions {

  def salesGreaterThan(list: List[Sales], x: Double) =
    list
      .filter(_.amounts.sum > x)
      .map(_.date)

  def doubleSalesThanDayBefore(list: List[Sales]) =
    list
      .sliding(2, 1)
      .toList
      .filter(_.size > 1)
      .filter {
        case List(salesDay1, salesDay2) => salesDay1.amounts.sum * 2 == salesDay2.amounts.sum
      }
      .map {
        case List(_, salesDay2) => salesDay2.date
      }

  def bestConsecutiveSales(list: List[Sales], n: Int) =
    list
      .sliding(n, 1)
      .toList
      .map(sales => (sales, sales.foldRight(0.0)((sale, acc) => sale.amounts.sum + acc)))
      .maxBy {
        case (_, amount) => amount
      }
      ._1

}


object SalesTest extends App {

  import SalesFunctions._

  val all = List(
    Sales("2015-05-05", List(1200, 233)), // no
    Sales("2015-05-06", List(3400, 24, 43)), // si
    Sales("2015-05-07", List(100, 24, 43)), // no
    Sales("2015-05-08", List(300, 24, 43)), // no
    Sales("2015-05-09", List(6400, 24, 43)), // si
    Sales("2015-05-10", List(5400, 24, 43)), // si
    Sales("2015-05-11", List(32323, 24, 43)), // si
    Sales("2015-05-12", List(1000, 100)),
    Sales("2015-05-13", List(2000, 200)),
    Sales("2015-05-14", List(2000, 100)),
    Sales("2015-05-15", List(4000, 200))
  )

//  private val result: List[String] = salesGreaterThan(all, 3400)
//    println(result)

//  private val res2 = doubleSalesThanDayBefore(all)
//  res2.foreach(println)


  private val value = bestConsecutiveSales(all, 3)
  value.foreach(println)

}
