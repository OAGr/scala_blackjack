import scala.collection.mutable.ArrayBuffer
val rnd = new scala.util.Random

class Hand{
  val cards = new ArrayBuffer[Card]()
  init

  def draw() = {
    cards += new Card
  }

  def points: Int = cards match{
    case none if none.isEmpty => 0
    case with_ace if (with_ace.map{c => c.points}.contains(1) && count < 12) => count + 10
    case cards => count
  }

  def count:Int = {
    cards.map{c => c.points}.reduceLeft[Int](_+_)
  }

  def show = {
    cards.foreach(c => println(c))
    println("total points: " + points) 
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

  def select_bid:Int = {
    println("How much would you like to spend? Please bid a discrete postive amount")
    var bid:Int = 0
    while (bid == 0){
      var bid_input:Int = Console.readInt()
      bid_input match{
        case lessthan1 if (lessthan1 < 1) => println("You need to bid a value above 0")
        case overamount if (overamount > gold ) => println("You don't have that much money.")
        case works if (works > 0 && works <= gold) => {bid = bid_input}
        case _ => println("Didn't understand input.  Try again")
      }
    }
    bid
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
    if (hand.count < 17){
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
    val house_points = house.hand.points
    val player_points = player.hand.points

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
    if (player.gold > 0){
    println("You have " + player.gold + " gold")
    // validate that bid is at most equal to gold amount and is greater
    // than 1
    var bid:Int = player.select_bid
    println("You have bid " + bid + " gold.  Starting Game!")
    round = new Round(player,house,bid)
    turn
    }   
    else {
      end_game
    }
  }

  def turn{
    println(" \n New Turn \n")
    house.show_hand
    player.show_hand
    println("\n\n 1: hold 2: draw ")
    choose
  }

  def choose{
    if (defeat){
      early_loss
    }
    var choice = Console.readInt()
      choice match {
      case 1 => player_hold
      case 2 => player_draw
      case _ => { println("didn't understand input.  Try again"); choose }
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

  def defeat = {
    player.hand.points > 21
  }

  def early_loss{ 
    println("You went over!")
    round.end
    setup_round
  }

  def end_game{
    println("You have used up all of your gold.  You have lost the game")
    Thread.sleep(2000)
    println("\n\nPlease don't take it personally though.  ")
    Thread.sleep(1200)
    print("\nTo be honest, you really shouldn't be playing blackjack anyway. ")
    Thread.sleep(1500)
    println("\nIf you're looking for some income, I suggest learning data science or data engineering.")
    Thread.sleep(2200)
    println("\n\nThat is all.  I'm getting back to work now.  Good day sir. \n\n\n")
    }

}

var game = new Game
game.setup_round
