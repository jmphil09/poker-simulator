package handManager

object Hand {
}

class Hand(cards: String) {
  val getCards = cards
  val handValue = 0
}

case class HighCard(cards: String) extends Hand(cards: String) {
	override val handValue = 1
}
case class OnePair(cards: String) extends Hand(cards: String) {
	override val handValue = 2
}
case class TwoPair(cards: String) extends Hand(cards: String) {
	override val handValue = 3
}
case class ThreeOfAKind(cards: String) extends Hand(cards: String) {
	override val handValue = 4
}
case class Straight(cards: String) extends Hand(cards: String) {
	override val handValue = 5
}
case class Flush(cards: String) extends Hand(cards: String) {
	override val handValue = 6
}
case class FullHouse(cards: String) extends Hand(cards: String) {
	override val handValue = 7
}
case class FourOfAKind(cards: String) extends Hand(cards: String) {
	override val handValue = 8
}
case class StraightFlush(cards: String) extends Hand(cards: String) {
	override val handValue = 9
}
