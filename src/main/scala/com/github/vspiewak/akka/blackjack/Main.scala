package com.github.vspiewak.akka.blackjack

import akka.actor._
import com.github.vspiewak.akka.blackjack.Deck.{new52, shuffle}
import com.github.vspiewak.akka.blackjack.Shoe.Take

import scala.util.Random

sealed abstract class Suite

case object ♠ extends Suite

case object ♥ extends Suite

case object ♣ extends Suite

case object ♦ extends Suite

sealed abstract class Rank

case object `2` extends Rank

case object `3` extends Rank

case object `4` extends Rank

case object `5` extends Rank

case object `6` extends Rank

case object `7` extends Rank

case object `8` extends Rank

case object `9` extends Rank

case object `10` extends Rank

case object Jack extends Rank

case object Queen extends Rank

case object King extends Rank

case object Ace extends Rank

sealed abstract class Card

final case class PlayingCard(rank: Rank, suite: Suite) extends Card {
  override def toString: String = s"$rank of $suite"
}

case object CutCard extends Card

object Deck {

  def new52(): List[Card] = {
    val suites = List(♠, ♥, ♣, ♦)
    val ranks = List(`2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, Jack, Queen, King, Ace)

    for {
      s <- suites
      r <- ranks
    } yield PlayingCard(r, s)

  }

  def shuffle(deck: List[Card]): List[Card] = Random.shuffle(deck)

}

object Shoe {

  sealed trait Message

  final case class Take(n: Int) extends Message

}

class Shoe extends Actor {

  val decks: List[Card] = (1 to 6).flatMap(_ => new52()).toList
  var cards: List[Card] = Deck.shuffle(CutCard :: decks)

  def receive() = {
    case Take(n) =>

      println(cards.length)
      println(cards.head)

      cards = cards.drop(n)

      println(cards.length)

      println(cards.zipWithIndex.find(_._1 == CutCard).map(t => s"Cut Card is in position ${t._2}").getOrElse("No Cut Card"))

  }

}

object Main {

  def main(args: Array[String]): Unit = {

    println("Hello, world!")

    val cOne = PlayingCard(Ace, ♠)
    val cTwo = PlayingCard(King, ♠)
    println(cOne)
    println(cTwo)

    val deckOne: List[Card] = List(cOne, cTwo, CutCard)
    val deckTwo: List[Card] = shuffle(new52())

    println(deckOne)
    println(deckTwo.length)
    println(deckTwo)

    val system: ActorSystem = ActorSystem("helloAkka")
    val shoe = system.actorOf(Props[Shoe], name = "shoe")

    shoe ! Take(3)
    system.terminate()

  }

}
