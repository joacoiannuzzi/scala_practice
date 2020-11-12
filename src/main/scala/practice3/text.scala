package practice3

/*

1) Implementar una función que reciba un texto
 (por ejemplo el contenido de un libro) como parámetro y devuelva
 como resultado una lista con las frases más populares del libro.
 Debe considerarse como una frase cualquier secuencia de 3 palabras
 dentro de una oración (separadas por punto).


*/


object Text extends App {

  def threeWordsSequence(text: String) = {
    val phrases = text.split('.')
      .toList
      .map(str => str.trim().split(' ').toList)
      .flatMap(_.sliding(3))
      .filter(_.size == 3)
      .map(_.mkString(" "))

    phrases
      .groupBy(identity)
      .map({
        case (key, value) => (key, value.size)
      })
      .toList
      .sortBy({
        case (_, value) => -value
      })
      .take(5)
      .tapEach(println)

  }

  val text = "una palabra. Implementar una función que reciba un texto. Debe considerarse como una frase cualquier. Implementar una función que  considerarse Implementar una función."

  threeWordsSequence(text)

}

/*

2) Implementar una función que reciba un texto como parámetro y devuelva
un mapa que permita dado dos palabras, obtener la siguiente palabra (la más frecuente).
El mapa puede tener la siguiente parametrización Map[(String, String), String]

 */

object text2 extends App {

  val text = "Esto es un ejemplo. Esto es una prueba. Esto es un ejercicio"

//    TODO falta terminar
  def nextWord(text: String) = {

    val phrases = text.split('.')
      .toList
      .map(str => str.trim().split(' ').toList)
      .flatMap(_.sliding(3))
      .filter(_.size == 3)

    val map =
      phrases.groupBy(_.take(2))
        .map({
          case (key, list) => (key, list.map(_.last))
        })
        .tapEach(println)

    map
      .groupBy({
        case (key, listOfvalues) =>
          (key,
            listOfvalues
              .groupBy(identity)
//              .map({
//                case (key, value) => (key, value.size)
//              })
          )
      })
      .tapEach(println)

  }

  nextWord(text)


}