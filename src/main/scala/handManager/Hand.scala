package handManager

object Hand {
}

class Hand(cards: String) {
  var cardStr = cards
}

case class HighCard(cards: String) extends Hand(cards: String)
case class OnePair(cards: String) extends Hand(cards: String)
case class TwoPair(cards: String) extends Hand(cards: String)
case class ThreeOfAKind(cards: String) extends Hand(cards: String)
case class Straight(cards: String) extends Hand(cards: String)
case class Flush(cards: String) extends Hand(cards: String)
case class FullHouse(cards: String) extends Hand(cards: String)
case class FourOfAKind(cards: String) extends Hand(cards: String)
case class StraightFlush(cards: String) extends Hand(cards: String)
