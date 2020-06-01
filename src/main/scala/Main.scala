package all

import scala.collection.mutable
import scala.io.StdIn
import scala.util.parsing.combinator.RegexParsers

object Main {

  abstract class Statement

  case class Command (val commandName: String,
                      val args: List[String]) extends Statement

  case class ChatRequest(val text: String) extends Statement

  object BotParser extends RegexParsers {

    def commandPrefix: Parser[String] = "!"

    def literal: Parser[String] = "['\"]".r ~ """([^'"]|(?<=\\)'")*""".r ~ "['\"]".r ^^ {
      case _ ~ value ~ _ => value
    }

    def commandAdd: Parser[Command] = "add" ~ literal ~ literal ^^ {
      case _ ~ key ~ value  => Command("add", List(key, value))
    }

    def commandList: Parser[Command] = "list" ^^ (_ => Command("list", List()))
    def commandExit: Parser[Command] = "exit" ^^ (_ => Command("exit", List()))

    def command: Parser[Command] = commandPrefix ~ (commandAdd | commandList | commandExit) ^^ {case _ ~ c => c }

    def chatRequest: Parser[ChatRequest] = ".*".r ^^ ChatRequest

    def chat: Parser[Statement] = command | chatRequest
  }

  def parse(str: String): Statement = BotParser.parse(BotParser.chat, str) match {
    case _: BotParser.Failure => throw new RuntimeException("Failed to parse " + str)
    case result => result.get
  }

  val responses: mutable.Map[String, String] = mutable.Map[String, String]()

  def addResponse(k: String, v: String): Unit = responses.get(k) match {
    case Some(_) => println("Key '" + k + "' already exists.")
    case None =>
      println("Adding '" + k + "' to '" + v + "'")
      responses.addOne((k.toLowerCase(), v))
      println("Success")
  }


  def listResponses(): Unit = {
    println("Responses: ")
    if (responses.nonEmpty)
      println(responses.map(x => x._1 + ": " + x._2).reduce((x, y) => x + "\n" + y))
    println("Total: " + responses.size)
  }

  def evalChat(key: String): Unit = responses.get(key.toLowerCase) match {
    case Some(response) => println("< " + response)
    case None => println("< What?")
  }

  def exit(): Unit = System.exit(0)

  def eval(statement: Statement) {
    println(statement)
    statement match {
      case Command(commandName, args) => commandName match {
        case "add" => addResponse(args.head, args(1))
        case "list" => listResponses()
        case "exit" => exit()
        case _ => throw new RuntimeException("Invalid command " + commandName)
      }
      case ChatRequest(text) => evalChat(text)
    }
  }

  def main(args: Array[String]): Unit = {
    while (true) eval(parse(StdIn.readLine("> ")))
  }

}