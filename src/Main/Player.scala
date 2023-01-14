package Main
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.io.StdIn.*
import scala.util.Random
import scala.math.*

object player:
  //checks if player has won the game
  var gameWon = false
  // stores the one are where player is currently located at
  var location: Area = null
  // all the items held by the player
  val inventory = Map[String,Item]()
  // players healthpoints in case of combat
  val fullHp = 200
  var currentHp = fullHp
  //damage the player does in combat
  val defaultDamage = 20
  var realDamage = this.defaultDamage //this is modified if the players picks up a weapon

  var didRun = false

  //determines if player has a fight going on
  var inCombat = false
  // determines what action is called calls that action if it exists
  
  def action(command:String) =
    var availableActions = Vector("go","pick","drop","examine","inventory","rest","hp")
    if location.areaActionName.isDefined then
      availableActions = availableActions :+ location.areaActionName.get
    val lowercasecommand = command.toLowerCase
    val action        = lowercasecommand.takeWhile( _ != ' ' )
    val parameter   = lowercasecommand.drop(action.length).trim
    if location.areaActionName.isDefined && action == location.areaActionName.get then 
      location.areaAction
    else
      action match
        case "go"        => this.go(parameter)
        case "pick"      => this.pickUp(parameter)
        case "drop"      => this.drop(parameter)
        case "examine"   => this.examine(parameter)
        case "inventory" => this.inventoryDesc
        case "rest"      => this.rest()
        case "hp"        => this.hpAmount
        case "help"      => this.help()
        case "use"       => this.use(parameter)
        
        case _           => s"please enter a valid command, available commands: ${availableActions.mkString(", ")}"

  // changes the location of the player to the location located in given direction. If there is no location in that direction, nothing happends and it returns a
  // request for valid location
  
  def help() =
    println("Your objective is to get to the castle king and fight him. List of commands:\n")
    println("go - needs a directional part after the main command. Lets you move to a new area. Example: go north\n")
    println("inventory - Shows you all the items in your inventory. Command inventory doesn't take another part\n")
    println("pick - pick up any object that is located in the same room. Command needs a second part that is the name of the item. Example: pick apple\n")
    println("examine - examine any item that you are carrying. Command needs a second part that is the name of the item. Example: examine apple\n")
    println("use - use any item that you are carrying. Command needs a second part that is the name of the item. Example: use apple\n")
    println("rest - resting restores all of your hitpoits. Command rest doesn't take another part\n")
    println("hp - Shows you how many hitpoints you have left. Command hp doesn't take another part\n")
    println("cook - Special command that only works in the kitchen. Command cook doesn't take another part\n")
    
    ""
  def go(direction: String):String =
    if this.location.neighbors.keys.toVector.contains(direction) then
      var notFound = false
      val nextLocation = this.location.neighbors(direction)
      if nextLocation.itemRequirements.isDefined then
        for itemName <- nextLocation.itemRequirements.get do
          if !this.inventory.contains(itemName) then
            notFound = true

      if notFound then
          val text = s"You need: {${nextLocation.itemRequirements.get.mkString(", ")}} to acces this room"
          s"${"-"*text.length}\n$text\n${"-"*text.length}"
      else
        location = location.neighbors(direction)
        s"You went to ${this.location}"
    else
      "Please enter a valid direction"

  def hpAmount = s"You have ${this.currentHp} HP"

  def rest() =
    if this.currentHp == this.fullHp then
      "You rested"
    else
      val lastHp = this.currentHp
      this.currentHp = this.fullHp
      s"You rested and regenerated ${this.currentHp - lastHp} HP"

  // helper method that can be used to browse any Map-type object to check if it contains a specific value and if it does, then a given action will be excecuted
  def browseInventory(itemName:String, inv:Map[String,Item], action:(item:String) => Unit):Boolean =
    val found = inv.get(itemName) match
      case Some(kama) =>
        action(kama.name)
        true

      case None => false

    found

  def inventoryDesc =
    var itemTexts = ""
    if this.inventory.nonEmpty then
      var description = "You are carrying:"
      for key <- inventory.keys do
        itemTexts = itemTexts + "\n" + key

      description + itemTexts
    else
      "You are empty-handed."

  // Tries to examine the given item, if the player has it in his inventory
  def examine(itemName:String) =
    inventory.get(itemName) match
      case Some(item) => s"You examine the ${item.name}.\n" + item.description
      case None       => "You don't have that item!"

  // Tries to drop a given item if the player has it in his inventory
  def drop(itemName:String) =
    def droppaa(name:String) =
      this.location.addItem(this.inventory(name))
      this.inventory -= name

    val dropped = browseInventory(itemName,this.inventory,droppaa)

    if !dropped then
      s"You don't have that!"
    else
      s"You drop the $itemName."

  // Tries to pick up a given item and add it to the players inventory
  def pickUp(itemName:String):String =
    def tryPickUp(name:String) =
      this.inventory += (name -> this.location.items(name))
      this.location.removeItem(name)

    val items = this.location.items
    val picked = browseInventory(itemName,items,tryPickUp)

    if !picked then
      s"There is no $itemName here to pick up."
    else
      s"You pick up the $itemName."
      
  
  def use(item: String) =
    if inventory.keys.toVector.contains(item) then 
      this.inventory(item).use
      if item == "shitburger" then
        this.gameWon = true
    else
      "You have no "+item+" to use here!"
      



  var attacks = Vector("kick", "hit", "oneshot", "run")

  def attack(enemy:Enemy, move:String) =
    move match
      case "kick"      => this.kick(enemy)
      case "hit"       => this.hit(enemy)
      case "sandthrow" => this.sandThrow(enemy)
      case "oneshot"   => this.oneshot(enemy)
      case "run"       => this.run(enemy)

  def run(enemy: Enemy) =
    this.location = this.location.defeatArea.get
    enemy.break = true
    this.didRun = true
    "You ran away from the battle!"

  def oneshot(enemy: Enemy):String =
    val damageDone = if enemy.name == "The King of the Castle" then 210 else enemy.fullHp

    enemy.currentHp -= damageDone
    s"You somehow did ${damageDone} damage to the enemy! 'Wow, I wish I could do that more often...'"

  def sandThrow(enemy: Enemy) =
    val enemyDodgeChanceBefore = enemy.dodgeChance
    enemy.dodgeChance = max(enemy.dodgeChance - 0.4,0.0)
    s"You threw sand at your enemy and its evasiveness fell by ${((enemyDodgeChanceBefore-enemy.dodgeChance)*100).toInt}%!"

  def kick(enemy: Enemy):String =
    val kickOptions = Vector("knee","thigh","side")

    println("Where do I kick?")
    kickOptions.foreach(println(_))
    var input = readLine()

    //kysy niin pitkään, että saadaan kelpaava input
    while !kickOptions.contains(input) do
      println("I can't do that!")
      println("Choose again!")
      input = readLine()


    if Random.nextInt(100) < enemy.dodgeChance*100 then
      "The enemy dodged your kick!"

    else
      input match
        case "knee" =>
          val damageDone = this.realDamage - Random.nextInt(5)
          enemy.currentHp -= damageDone
          val enemyDodgeChanceBefore = enemy.dodgeChance
          enemy.dodgeChance = max(0,enemy.dodgeChance-0.1)
          s"Damage done: $damageDone, enemys evasiveness fell by ${((enemyDodgeChanceBefore-enemy.dodgeChance)*100).toInt}%!"

        case "thigh" =>
          val damageDone = this.realDamage + Random.nextInt(10)
          enemy.currentHp -= damageDone
          val enemyDamageBefore = enemy.damage
          enemy.damage = max(5,enemy.damage-2) //enemys damage can't go lower than 5
          s"Damage done: $damageDone, enemys damage fell by ${enemyDamageBefore-enemy.damage}!"

        case "side" =>
          val damageDone = this.realDamage + Random.nextInt(20)
          enemy.currentHp -= damageDone
          s"Damage done: $damageDone!"



  def hit(enemy: Enemy) =
    val punchOptions = Vector("head","arm")

    println("Where do I hit it?")
    punchOptions.foreach(println(_))
    var input = readLine()

    //kysy niin pitkään, että saadaan kelpaava input
    while !punchOptions.contains(input) do
      println("I can't do that!")
      println("Choose again!")
      input = readLine()

    if Random.nextInt(100) < enemy.dodgeChance*100 then
      "The enemy dodged your attack!"

    else
      input match
        case "head" =>
          val damageDone = this.realDamage-5 + Random.nextInt(35)
          enemy.currentHp -= damageDone
          s"Damage done: $damageDone!"

        case "arm" =>
          val damageDone = (this.realDamage/2) + Random.nextInt(10)
          enemy.currentHp -= damageDone
          val enemyDamageBefore = enemy.damage
          val enemyMissChanceBefore = enemy.missChance

          enemy.missChance = min(0.75,enemy.missChance+0.05)
          enemy.damage = max(0,enemy.damage-2)

          s"Damage done: $damageDone, enemys damage fell by ${enemyDamageBefore-enemy.damage} and it's chance of missing grew by ${((enemy.missChance-enemyMissChanceBefore)*100).round.toInt}%!"
