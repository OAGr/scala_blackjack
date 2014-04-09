import scala.collection.mutable.ArrayBuffer
val rnd = new scala.util.Random

class Hand{
  val cards = new ArrayBuffer[Card]()
    init

  def init() = {
    draw()
    draw()
  }

  def draw() = {
    cards += new Card
  }

  def show = {
    cards.foreach(c => println(c))
    println("total points: " + points) 
  }

  def show_opponent_hand {
    println("(Card Facedown) ")
    for (i <- 1 until cards.length){
      println(cards(i))
    }
  }

  def points:Int = cards match{
    case none if none.isEmpty => 0
    case use_ace if ( has_ace && count < 12) => count + 10
    case cards => count
  }

  //counts hand total points with Aces as 1
  def count:Int = {
    cards.map{c => c.points}.reduceLeft[Int](_+_)
  }

  def has_ace:Boolean = {
    cards.map{c => c.points}.contains(1)
  }

  class Card{

    // Gives card a random kind (2,3,King, etc),
    // and a random suite ("Hearts", "Spades")
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
        case face_card if face_card > 10 => 10
        case number_card => number_card
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

abstract class Person(var name:String){
  var hand:Hand = _
  var standing:Boolean = false

  def reset = {
    standing = false
    hand = new Hand
  }

  def turn{
    println("\n========== " + name + "'s Turn ==========")
    make_choice 
    pause()
  }

  def make_choice{
    choose match {
      case "stand" => stand
      case "draw" => draw
    }
  }

  def choose: String

  def stand{
    standing = true
    println("\n ---<<<<< " + name + " STANDS >>>>>---  \n")
    pause()
  }

  def draw{
    println("\n ---<<<<< " + name + " DRAWS >>>>>---  \n")
    hand.draw()
    pause(2)
    println(name + "'s new hand is")
    show_hand
  }

  def show_hand = {
    println("\n-- " + name + "'s Cards -- ")
    hand.show_opponent_hand
  }

  def show_full_hand = {
    println("\n-- " + name + "'s Cards -- ")
    hand.show
  }


}

class Dealer(name:String = "The Dealer") extends Person(name){
  def choose:String = {
    val point_limit = 17
    if (hand.points < point_limit){ 
      "draw"
    }
    else{
      "stand"
    }
  }
}

class Player(name:String = "The Player") extends Person(name){
  var gold:Int = 100
  var current_bid: Int = _

  override def make_choice{
    show_hand
    choose match {
      case "stand" => stand
      case "draw" => {draw; check_if_lost}
    }
  }

  def choose:String = {
    println("\n\n 1: stand 2: draw ")
    var choice:String = ""

    while (choice == ""){
      choice = Console.readInt() match{
        case 1 => "stand"
        case 2 => "draw"
      }
      if (choice==""){println("didn't understand input.  Try again")}
    }

    choice
  }

  def check_if_lost{
    if (hand.points > 21){
      standing = true
      println("You went over!")
    }
  }

  override def show_hand{
    show_full_hand
  }

  def set_round_bid{
    println("You have " + gold + " gold")
    current_bid = select_bid
    println("You have bid " + current_bid + " gold.  Starting Game!")

    def select_bid:Int = {
      println("How much would you like to bet?")
      var bid:Int = 0
      while (bid == 0){
        var bid_input:Int = Console.readInt()
          bid_input match{
          case lessthan1 if (lessthan1 < 1) => println("You need to bid a value above 0.  Enter again.")
          case overamount if (overamount > gold ) => println("You don't have that much money. Enter again")
          case works if (works > 0 && works <= gold) => {bid = bid_input}
          case _ => println("Didn't understand input.  Enter again")
        }
      }
      bid
    }
  }

  def complete_round(dealer:Dealer){
    var multiplier:Int = round_multiplier(dealer:Dealer)
    exchange_money(multiplier)
    println(outcome_message(multiplier))
  }

  def exchange_money(multiplier:Int) = {
    gold += (current_bid * multiplier)
  }

  def outcome_message(multiplier:Int):String = multiplier match {
    case 1 => "Success!  You made " + current_bid + " gold!"
    case -1 => "Lost! You lost " + current_bid + " gold!"
    case 2 => "Blackjack! You won " + (current_bid * 2) + "gold!"
  }

  def round_multiplier( dealer: Dealer):Int = {
    var multiplier: Int = 0
    val dealer_points = dealer.hand.points
    val person_points = hand.points

    if (person_points > 21){
      lose()
    }
    else if (person_points == 21){
      blackjack()
    }
    else if (dealer_points > 21 || dealer_points < person_points){
      win()
    }
    else{
      lose()
    }

    def win() = {
      multiplier = 1
    }

    def blackjack() = {
      multiplier = 2
    }

    def lose() = {
      multiplier = -1
    }

    multiplier
  }
}





class Round( player: Player, dealer: Dealer){
  var result: Int = _
  player.reset
  dealer.reset

  def play{
    introduce
    player.set_round_bid
    opening_draw
    while(!dealer.standing || !player.standing){
      turn
    }
    round_end
  }

  def introduce{
    println("\n\n\n<=============== NEW ROUND ===============> \n ")
    pause()
  }

  def opening_draw{
    println("\n ---<<<<< Initial Draw >>>>>---  \n")
    dealer.show_hand
    pause()
    player.show_hand
    pause()
  }

  def turn{
    pause()
    if (!player.standing) { player.turn }
    if (!dealer.standing) { dealer.turn }
  }

  def round_end{
    print_outcome
    player.complete_round(dealer)
    pause(3)
  }


  def print_outcome{
    pause(2)
    println(" \n\n\n <=============== END OF ROUND ===============> \n ")
    pause(2)
    dealer.show_full_hand
    player.show_full_hand
    pause(2)
  }


}

class Game{
  val player = new Player()
    val dealer = new Dealer()
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
  var time:Int = periods * 1000
  Thread.sleep(time)
}

var game = new Game
game.main
