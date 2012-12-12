import annotation.tailrec
import collection.immutable.Stack
import io.Source

object Anagrams extends App {

  val start = System.nanoTime()
  val wordDb = Source.fromFile("./brit-a-z.txt").getLines().map {_.replaceAll("[^a-z]+", "")}.toIterable.groupBy(_.sorted)
  //  val wordDb = Source.fromFile("./brit-a-z.txt").getLines().map {_.replaceAll("[^a-z]+", "")}.toIterable.par.groupBy(_.sorted).seq
  printf("Dictionary loaded in %dms\n", (System.nanoTime() - start) / 1000000)

  val grepCommand = "grep ([a-z]+)".r
  val setCommand = "set ([a-z]+)".r
  val limitCommand = "limit ([a-z]+)".r

  @tailrec
  def handleInput(input: String, wordStack: Stack[Iterable[String]] = Stack.empty) {
    def currentWords: Iterable[String] = wordStack.headOption.flatten

    val start = System.nanoTime()
    val newStack: Stack[Iterable[String]] = input match {
      case "pop" =>
        if (wordStack.isEmpty) {
          println("Can't pop - empty stack")
          wordStack
        }
        else wordStack.pop
      case setCommand(letters) =>
        val newLetters = for {
          length <- (letters.length to 2 by -1).view
          signature <- letters.combinations(length)
          // _ = println("Checking signature " + signature) // This is the part that takes long
          word <- wordDb.get(signature.sorted).flatten
        } yield word
        Stack(newLetters.toStream)
      case grepCommand(letters) =>
        val letterSet = letters.toSet
        val filteredWords = currentWords.filter(word => letterSet.subsetOf(word.toSet))
        wordStack.push(filteredWords)
      case limitCommand(letters) =>
        val letterSet = letters.toSet
        val filteredWords = currentWords.filter(_.toSet.subsetOf(letterSet))
        wordStack.push(filteredWords)
      case _ =>
        println("Please use set <letters>, grep <letters> or pop")
        wordStack
    }

    println({
      val start = newStack.headOption.flatten.take(25).toList
      if (start.isEmpty) "No matches" else start
    })
    printf("Command handled in %dms\n", (System.nanoTime() - start) / 1000000)

    handleInput(readLine(), newStack)
  }

  handleInput(readLine())
}
