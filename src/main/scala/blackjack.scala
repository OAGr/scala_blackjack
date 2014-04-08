import scala.collection.mutable.ArrayBuffer
val rnd = new scala.util.Random

class Hand{
  val cards = new ArrayBuffer[Card]()
    init()

  def draw() = {
    cards += new Card
  }

  def total: Int = {
    //handle case with 0 cards
    var total = cards.map{c => c.points}.reduceLeft[Int](_+_)
      // Also make sure it's not the dealer with under 17 points

      if (cards.map{c => c.points}.contains(1) && total < 12){
      total + 10
    }
    else{
      total
    }
  }

  def show = {
    cards.foreach(c => println(c))
    println("total points: " + total) 
  }

  def show_from_house {
    for (i <- 1 until cards.length){
      println(cards(i))
    }
  }

  def init() = {
    draw()
    draw()
  }


  class Card{

    val kind = new Kind
    val suite = new Suite

    def points: Int = {
      kind.points
    }

    override def toString: String = {
      kind.name + " of " + suite.value
    }

    class Kind{
      val value: Int = find_value

      def find_value: Int = {
        val range = 1 to 13
        val value: Int = range(rnd.nextInt(range length))
          value
      }  

      def points = value match { 
        case x if x > 10 => 10
        case x => x
      }

      def name = value match {
        case 1 => "Ace"
        case 11 => "Jack"
        case 12 => "Queen"
        case 13 => "King"
        case (x:Int) => x.toString()
      }
    }

    class Suite{
      val value: String = find_value

      def find_value: String = {
        val options = Array("Hearts", "Spades", "Diamonds", "Clubs")
          val range = 0 to 3
        val index: Int = rnd.nextInt(range length)
          options(index)
      }
    }
  }
}

class Player{
  var hand:Hand = _
  var gold:Int = 100

  def reset = {hand = new Hand}

  def show_hand{
    println("\n\n ---- Your Cards ---- ")
    hand.show
  }

}

class House{
  var hand:Hand = _

  def reset = {hand = new Hand}

  def show_hand = {
    println("\n\n ---- House's Cards ---- ")
    println(" (Card Facedown) ")
    hand.show_from_house
  }

  def show_full_hand = {
    println("\n\n ---- House's Full Cards ---- ")
    hand.show
  }

  def turn = {
    if (hand.total < 17){
      hand.draw()
    }
    println("The House's new hand is")
    show_hand
  }
}



class Round( player: Player, house: House, bid: Int){
  player.reset
  house.reset
  var result: Int = _

  def end{
    find_outcome()
    exchange_money()
    print_outcome
  }

  def find_outcome() = {
    result = new Result(player, house).multiply
  }

  def outcome_message():String = result match {
    case 1 => "Success!  You made " + bid + " gold!"
    case -1 => "Lost! You lost " + bid + " gold!"
    case 2 => "Blackjack! You won " + (bid * 2) + "gold!"
  }

  def exchange_money() = {
    player.gold += (bid * result)
  }

  def print_outcome{
    println(" \n\n\n ---------------- END OF ROUND ---------------- \n ")
    house.show_full_hand
    player.show_hand
    println(" \n ")
    println(outcome_message)
  }

  class Result( player: Player, house: House){
    var multiply: Int = _
    val house_points = house.hand.total
    val player_points = player.hand.total

    if (player_points > 21){
      lose()
    }
    else if (player_points == 21){
      blackjack()
    }
    else if (house_points > 21 || house_points < player_points){
      win()
    }
    else{
      lose()
    }

    def win() = {
      multiply = 1
    }

    def blackjack() = {
      multiply = 2
    }

    def lose() = {
      multiply = -1
    }
  }
}

class Game{
  val player = new Player
  val house = new House
  var round: Round = _

  def setup_round{
    println("You have " + player.gold + " gold")
    println("How much would you like to spend?")
    // validate that bid is at most equal to gold amount
    var bid:Int = Console.readInt()
      println("You have bid " + bid + " gold.  Starting Game!")
    round = new Round(player,house,bid)
    turn
  }

  def turn{
    println(" \n New Turn \n")
    house.show_hand
    player.show_hand
    println("\n\n 1: hold   2: draw ")
    choose
  }

  def choose{
    if (defeat == true){
      early_loss
    }
    var choice = Console.readInt()
      choice match {
      case 1 => player_hold
      case 2 => player_draw
      case _ => redo
    }
  }

  def player_hold{
    house.turn
    round.end
    setup_round
    // TODO: allow dealer to go more times after player holds
  }

  def player_draw{
    player.hand.draw()
    house.turn
    turn
  }

  def redo{
    println("didn't understand input.  Try again")
    choose
  }

  def defeat = {
    player.hand.total > 21
  }

  def early_loss{ 
    println("You went over!")
    round.end
    setup_round
  }

}

var game = new Game
game.setup_round
