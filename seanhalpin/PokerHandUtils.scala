/**
  * Created by shalpin on 26/09/2017.
  */
object PokerHandUtils {
  def main(args: Array[String]) {
    assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")) == "Invalid hand: Too few cards")
    assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) == "Invalid hand: Too many cards")
    assert(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")) == "Invalid hand: Three of Hearts appears 2 times")
    assert(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")) == "High card: Ace of Clubs")
    assert(PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D")) == "Straight Flush: Two to Six")

    //
    println(PokerApp.classifyHand(List("AC", "4D", "QC", "3H")))
    println(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S", "6D")) )
    println(PokerApp.classifyHand(List("3H", "4D", "QC", "3H", "10S")))
    println(PokerApp.classifyHand(List("AC", "4D", "QC", "3H", "10S")))
    println(PokerApp.classifyHand(List("6D", "5D", "3D", "4D", "2D")))
  }
}

object PokerApp {
  val NumberOfCardsInHand = 5

  def classifyHand(cards: List[String]): String ={
    val validationResult : String = Validate(cards)
    if(validationResult != "Valid"){ return validationResult}

    val domainCards = cards.map(card => new Card().Create(card))
    val bestClassification = domainCards match {
      case hand if StraightFlush.evaluate(hand) => StraightFlush.classify(hand)
      case hand if HighCard.evaluate(hand) => HighCard.classify(hand)
    }

    bestClassification
  }

  private def Validate(cards: List[String]): String = {
    val countValidationResult = countValidation(cards)
    if(!countValidationResult.equals("Valid")){return countValidationResult}
    val cardValidationResult = cardValidation(cards)
    cardValidationResult
  }

  private def countValidation(cards: List[String]): String = {
    cards match{
      case none if none == null => "Invalid hand: List Null"
      case below if below.length < NumberOfCardsInHand => "Invalid hand: Too few cards"
      case above if above.length > NumberOfCardsInHand => "Invalid hand: Too many cards"
      case _ => "Valid"
    }
  }

  private def cardValidation(cards: List[String]): String = {
    val allCardsValid = cards.forall{ cardIsValid(_) }
    if(!allCardsValid){
      return "Invalid hand: " + Utils.cardShortHandToFullString(cards.filter(!cardIsValid(_)).head)
    }
    val duplicateCards = cards.diff(cards.distinct)
    if(duplicateCards.length > 0){
      return "Invalid hand: " + duplicateCards.distinct.map(distinctDuplicateCard =>
        Utils.cardShortHandToFullString(distinctDuplicateCard) + " appears " + cards.count(dupe => dupe == distinctDuplicateCard) + " times").head
    }
    "Valid"
  }

  private def cardIsValid(s: String): Boolean = {
    val number = s.splitAt(s.length - 1)._1
    val suit = s.splitAt(s.length - 1)._2
    val numberPattern = "[1-9]|10|[JQKA]"
    val suitPattern = "[CDHS]"
    return number.matches(numberPattern) && suit.matches(suitPattern)
  }
}

trait PlayingCard {
  protected var _number = 0
  protected var _suit = 0
  def number = _number
  def suit = _suit
  def Create(cardString: String) : (Card)
}

class Card extends PlayingCard{
  override def Create(cardString: String): Card = {
    val numberString = cardString.splitAt(cardString.length - 1)._1
    val suitString = cardString.splitAt(cardString.length - 1)._2
    _number = Utils.numberShortHandToDomainValue(numberString)
    _suit = Utils.suitShortHandToDomainValue(suitString)
    this
  }
}

trait Hand {
  def evaluate(cards: List[Card]): (Boolean)
  def classify(cards: List[Card]): (String)
}

object StraightFlush extends Hand{
  override def evaluate(cards: List[Card]): (Boolean) =
    (cards.sortBy(c=>c.number).sliding(2)
    .forall(cards => cards.head.number + 1 == cards.last.number)
    && cards.forall(_.suit == cards.head.suit))
  override def classify(cards: List[Card]): String = {
    val sortedCards = cards.sortBy(c=>c.number)
    "Straight Flush: "+ Utils.numberDomainValueToFullString(sortedCards.head.number) +
      " to "+ Utils.numberDomainValueToFullString(sortedCards.last.number)
  }
}

object HighCard extends Hand{
  override def evaluate(cards: List[Card]): (Boolean) = {
    true
  }
  override def classify(cards: List[Card]): String = {
    "High card: " + Utils.cardDomainToFullString(cards.sortBy(card=>card.number).last)
  }
}

object Utils {
  def numberDomainValueToFullString(number: Int): String = {
    number match {
      case 0 => "Two"
      case 1 => "Three"
      case 2 => "Four"
      case 3 => "Five"
      case 4 => "Six"
      case 5 => "Seven"
      case 6 => "Eight"
      case 7 => "Nine"
      case 8 => "Ten"
      case 9 => "Jack"
      case 10 => "Queen"
      case 11 => "King"
      case 12 => "Ace"
    }
  }

  def suitDomainValueToFullString(suit: Int): String = {
    suit match {
      case 0 => "Diamonds"
      case 1 => "Clubs"
      case 2 => "Spades"
      case 3 => "Hearts"
    }
  }

  def numberShortHandToFullString(number: String): String = {
    number match {
      case "2" => "Two"
      case "3" => "Three"
      case "4" => "Four"
      case "5" => "Five"
      case "6" => "Six"
      case "7" => "Seven"
      case "8" => "Eight"
      case "9" => "Nine"
      case "10" => "Ten"
      case "J" => "Jack"
      case "Q" => "Queen"
      case "K" => "King"
      case "A" => "Ace"
    }
  }

  def suitShortHandToFullString(suit: String): String = {
    suit match {
      case "D" => "Diamonds"
      case "C" => "Clubs"
      case "S" => "Spades"
      case "H" => "Hearts"
    }
  }

  def numberShortHandToDomainValue(number: String): Int = {
    number match {
      case "2" => 0
      case "3" => 1
      case "4" => 2
      case "5" => 3
      case "6" => 4
      case "7" => 5
      case "8" => 6
      case "9" => 7
      case "10" => 8
      case "J" => 9
      case "Q" => 10
      case "K" => 11
      case "A" => 12
    }
  }

  def suitShortHandToDomainValue(suit: String): Int = {
    suit match {
      case "D" => 0
      case "C" => 1
      case "S" => 2
      case "H" => 3
    }
  }

  def cardShortHandToFullString(card: String): String = {
    val number = card.splitAt(card.length - 1)._1
    val suit = card.splitAt(card.length - 1)._2
    val numberFull = numberShortHandToFullString(number)
    val suitFull = suitShortHandToFullString(suit)
    numberFull + " of " + suitFull
  }

  def cardDomainToFullString(card: Card): String = {
    val numberFull = numberDomainValueToFullString(card.number)
    val suitFull = suitDomainValueToFullString(card.suit)
    numberFull + " of " + suitFull
  }
}