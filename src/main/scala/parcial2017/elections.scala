package parcial2017

import scala.io.Source

case class ElectionResult(province: String, district: String, candidate: String, votes: Int)

object ElectionResultOps {

  def totalVotes(electionResults: List[ElectionResult]): Int = {

    electionResults.map(_.votes).sum
  }

  def winner(electionResults: List[ElectionResult]) = {
    electionResults
      .groupBy(_.candidate)
      .maxBy {
        case (_, list) => totalVotes(list)
      }
      ._1
  }

  // el distrito en el cual obtuvo mejores resultados un candidato
  def bestResultByDistrict(candidate: String, electionResults: List[ElectionResult]) = {
    electionResults
      .filter(_.candidate == candidate) //  List[ElectionResult]
      .maxBy {
        case ElectionResult(_, _, _, votes) => votes
      }.district
  }


}

object Load {

  def loadCSV(file: String): List[ElectionResult] = {
    val bufferSource = Source.fromFile(file)
    val source = bufferSource.getLines().toList.map(_.split(',').toList)
    bufferSource.close()

    source.map {
      case List(province, district, candidate, votes) =>
        ElectionResult(province, district, candidate, votes.toInt)
    }

  }
}

object elections extends App {

  import Load._

  private val results: List[ElectionResult] = loadCSV("/Users/joacoiannuzzi/test/scala_practice/src/main/scala/parcial2017/elections.csv")

  import ElectionResultOps._

  println(totalVotes(results))

  println(winner(results))

  println(bestResultByDistrict("A", results))


}
