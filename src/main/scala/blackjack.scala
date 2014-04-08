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

  def show_from_dealer {
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
  var standing:Boolean = false
  var gold:Int = 100

  def reset = {
    standing = false
    hand = new Hand
  }

  def show_hand{
    println("\n-- Your Cards -- ")
    hand.show
  }

  def select_bid:Int = {
    println("How much would you like to bet?")
    var bid:Int = 0
    while (bid == 0){
      var bid_input:Int = Console.readInt()
        bid_input match{
        case lessthan1 if (lessthan1 < 1) => println("You need to bid a value above 0.  Enter number again.")
        case overamount if (overamount > gold ) => println("You don't have that much money. Enter number again")
        case works if (works > 0 && works <= gold) => {bid = bid_input}
        case _ => println("Didn't understand input.  Try again")
      }
    }
    bid
  }

  def turn{
    show_hand

    println("\n\n 1: stand 2: draw ")
    var choice = Console.readInt()
      choice match {
      case 1 => stand
      case 2 => draw
      case _ => { println("didn't understand input.  Try again"); turn }
    }
  }

  def stand{
    standing = true
    println("\n ---<<<<< You STAND >>>>>---  \n")
    pause()
  }

  def draw{
    hand.draw()
    println("\n ---<<<<< You DRAW >>>>>---  \n")
    pause()
    show_hand
    if (defeat){
      early_loss
    }
    pause()
  }

  def defeat = {
    hand.points > 21
  }

  def early_loss{ 
    standing = true
    println("You went over!")
  }
}

class Dealer{
  var hand:Hand = _
  var standing:Boolean = false

  def reset = {
    standing = false
    hand = new Hand
  }

  def show_hand = {
    println("\n-- Dealer's Cards -- ")
    println("(Card Facedown) ")
    hand.show_from_dealer
  }

  def show_full_hand = {
    println("\n-- Dealer's Cards -- ")
    hand.show
  }

  def turn = {
    if (hand.points < 17){ 
      draw
    }
    else{
      stand
    }
  }

  def stand{
    println("\n ----- The Dealer STANDS ----- \n")
    pause()
    standing = true
  }

  def draw{
    println("\n ----- The Dealer DRAWS ----- \n")
    hand.draw()
    pause(2)
    println("The Dealers's new hand is")
    pause()
    show_hand
  }
}



class Round( player: Player, dealer: Dealer){
  var result: Int = _
  var bid: Int = _
  player.reset
  dealer.reset

  def play{
    setup
    while(!dealer.standing || !player.standing){
      turn
    }
    end
  }

  def setup{
    println("\n\n\n<=============== NEW ROUND ===============> \n ")
    println("You have " + player.gold + " gold")
    bid = player.select_bid
    println("You have bid " + bid + " gold.  Starting Game!")
    println("\n ---<<<<< Initial Draw >>>>>---  \n")
    pause()
    dealer.show_hand
    player.show_hand
    pause()
  }

  def turn{
    pause()
    if (!player.standing) { 
      println("\n========== Your Turn ==========")
      player.turn 
      pause()
    }
    if (!dealer.standing) { 
      println("\n========== Dealer's Turn ========== \n")
      pause()
      dealer.turn 
    }
  }

  def end{
    find_outcome()
    exchange_money()
    print_outcome
  }

  def find_outcome() = {
    result = new Result(player, dealer).multiply
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
    pause(2)
    println(" \n\n\n <=============== END OF ROUND ===============> \n ")
    pause(2)
    dealer.show_full_hand
    player.show_hand
    println(" \n ")
    println(outcome_message)
    pause(6)
  }

  class Result( player: Player, dealer: Dealer){
    var multiply: Int = _
    val dealer_points = dealer.hand.points
    val player_points = player.hand.points

    if (player_points > 21){
      lose()
    }
    else if (player_points == 21){
      blackjack()
    }
    else if (dealer_points > 21 || dealer_points < player_points){
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
  val dealer = new Dealer
  var round: Round = _

  def setup{
    println("Welcome to Hacker Blackjack! \n\n")
    println("You play it like Blackjack")
    main
  }

  def main{
    while (player.gold > 0){
      round = new Round(player,dealer)
      round.play
    }   
    end_game
  }

  def end_game{
    pause(3)
    println("\nYou have used up all of your gold.  You have completely lost.  You are now bankrupt.  Your family is devestated that you blew all 100 gold coins on Blackjack.")
    pause(8)
    println("\nIf you're looking for some income, I suggest learning data science or data engineering.")
    pause(4)
    println("\n\nThat is all.  I'm getting back to work now.  Good day.")
    pause(3)
  }

}

def pause( periods: Int = 1){
    var time:Int = periods * 500
    Thread.sleep(time)
  }

var game = new Game
game.main
