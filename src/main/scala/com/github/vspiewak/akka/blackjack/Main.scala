package com.github.vspiewak.akka.blackjack

import akka.actor._

import scala.runtime.BoxedUnit
import scala.util.Random

sealed abstract class Suite
case object ♠ extends Suite
case object ♥ extends Suite
case object ♣ extends Suite
case object ♦ extends Suite

sealed abstract class Rank
case object Two extends Rank
case object Three extends Rank
case object Four extends Rank
case object Five extends Rank
case object Six extends Rank
case object Seven extends Rank
case object Eight extends Rank
case object Nine extends Rank
case object Ten extends Rank
case object Jack extends Rank
case object Queen extends Rank
case object King extends Rank
case object Ace extends Rank

sealed abstract class Card
case class PlayingCard(rank: Rank, suite: Suite) extends Card {
  override def toString: String = s"$rank of $suite"
}

case object CutCard extends Card

object Deck {

  def new52(): Seq[Card] = {
    val suites = Seq(♠, ♥, ♣, ♦)
    val ranks = Seq(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

      for {
        s <- suites
        r <- ranks
      } yield PlayingCard(r, s)

  }

  def shuffle(deck: Seq[Card]): Seq[Card] = Random.shuffle(deck)

}

class Shoe extends Actor {

   val decks: Seq[Card] = (for (i <- 1 to 6) yield Deck.shuffle(Deck.new52())).flatten
   var cards: Seq[Card] = decks.take(260) ++ Seq(CutCard) ++ decks.drop(260)

   def receive() = {
     case "Take" => {

       println(cards.length)
       println(cards.head)

       cards = cards.drop(1)

       println(cards.length)

     }
   }

}

object Main {

  def main(args: Array[String]): Unit = {

    println("Hello, world!")

    val cOne = PlayingCard(Ace, ♠)
    val cTwo = PlayingCard(King, ♠)
    println(cOne)
    println(cTwo)

    val deckOne: Seq[Card] = Seq(cOne, cTwo, CutCard)
    val deckTwo: Seq[Card] = Deck.shuffle(Deck.new52())

    println(deckOne)
    println(deckTwo.length)
    println(deckTwo)

    val system: ActorSystem = ActorSystem("helloAkka")
    val shoe = system.actorOf(Props[Shoe], name = "shoe")

    shoe ! "Take"
    system.terminate()

  }

}
