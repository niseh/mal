package mal.scalapony.step1

object REPL {
  val USER_PROMPT = "user> "

  def read(in: String): String         = in

  def eval(statement: String): String  = statement

  def print(out: String): Unit = {
    println(out)
  }

  def loop() {
    import scala.io.StdIn

    val readLine = StdIn.readLine(USER_PROMPT)

    if (readLine != null) {
      rep(readLine)

      loop()
    }
  }

  def rep(in: String) = {
    val readLine    = read(in)
    val evaluation  = eval(readLine)

    print(evaluation)
  }

  def main(args: Array[String]) {
    // read line
    println("\nWelcome to MAL interpreter v0.0.1")
    println("Copyright (c) 2016, Nizar SEHLI <nisehl@gmail.com>\n")

    loop()
  }
}
