import annotation.tailrec
import io.Source

object Anagrams extends App {

  val start = System.nanoTime()
  val wordDb = Source.fromFile("./brit-a-z.txt").getLines().map {_.replaceAll("[^a-z]+", "")}.toIterable.groupBy(_.sorted)
  //  val wordDb = Source.fromFile("./brit-a-z.txt").getLines().map {_.replaceAll("[^a-z]+", "")}.toIterable.par.groupBy(_.sorted).seq
  val grep = "grep ([a-z]+)".r

  printf("Dictionary loaded in %dms\n", (System.nanoTime() - start) / 1000000)

  @tailrec
  def handleInput(input: String, words: Iterable[String] = Nil) {
    val start = System.nanoTime()
    val newWords: Iterable[String] = input match {
      case grep(letters) =>
        val letterSet = letters.toSet
        words.filter(word => letterSet.subsetOf(word.toSet))
      case letters =>
        for {
          length <- (letters.length to 2 by -1).view
          signature <- letters.combinations(length)
          // _ = println("Checking signature " + signature) // This is the part that takes long
          word <- wordDb.get(signature.sorted).flatten
        } yield word
    }

    println({
      val start = newWords.take(3).toList
      if (start.isEmpty) "No matches" else start
    })
    printf("Command handled in %dms\n", (System.nanoTime() - start) / 1000000)

    handleInput(readLine(), newWords)
  }

  handleInput(readLine())
}
