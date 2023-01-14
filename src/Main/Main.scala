package Main
import scala.collection.mutable.Buffer
import scala.io.StdIn.*

def combat(enemy:Enemy):String =
  player.didRun = false
  if !player.inCombat then
    player.inCombat = true
    Thread.sleep(1000)
    println("\n")
    println(enemy.description)
    Thread.sleep(3000)
    println(s"You got into combat against ${enemy.name}!\n")
    Thread.sleep(1000)
  else if enemy.name == "The King of the Castle" then
    enemy.break = false
    Thread.sleep(1000)
    println("You see a wounded looking King kneeling in the middle of the room. As you start to approach him, he stands and puts on his crown. The crown starts to glow creating an aura around the King...")
    Thread.sleep(4000)
    println("I SUMMON THE POWER OF MY ANCESTORS!!!")
    Thread.sleep(2000)
    println("YOU ARE NOT GOING TO TAKE MY THRONE!!!")
    Thread.sleep(2000)
    println(s"You are now in combat against ${enemy.name}!\n")
  var turn = 0

  //if player has a sword in their inventory, they get +10 damage
  if player.inventory.contains("sword") then
    player.realDamage = player.defaultDamage + 10
  else
    player.realDamage = player.defaultDamage

  //if player has sandbag in their inventory, they get the attack sandthrow
  if player.inventory.contains("sandbag") then
    if !player.attacks.contains("sandthrow") then
      player.attacks = player.attacks :+ "sandthrow"
  else
    if player.attacks.contains("sandthrow") then
      player.attacks = (player.attacks.toBuffer -= "sandthrow").toVector


  while (player.currentHp > 0 && enemy.currentHp > 0) && !enemy.break do
    if turn%2 == 0 then
      println("What should I do?")
      player.attacks.foreach(println(_))
      var input = readLine()

      //kysy niin pitkään, että saadaan kelpaava input
       while !player.attacks.contains(input) do
        println("I can't do that!")
        println("Choose again!")
        input = readLine()

      println(player.attack(enemy, input))
      Thread.sleep(500)

      turn += 1

    else
      Thread.sleep(1000)
      println(enemy.attack())
      Thread.sleep(1500)

      turn += 1

    if !enemy.break then
      println(s"\nYour HP: ${player.currentHp}")
      println(s"The enemys HP: ${enemy.currentHp}\n")
    Thread.sleep(1000)

  if !enemy.break then
    if player.currentHp > 0 then
      if enemy.name == "The King of the Castle" then
        player.gameWon = true
      player.location.areaEnemy = None
      player.inCombat = false
      "You won the battle!"
    else
      //bosses stats need to be reset for a new fight
      enemy.currentHp = enemy.fullHp
      enemy.ultUsed = false
      enemy.dodgeChance = enemy.originalDodgeChance
      enemy.missChance = enemy.originalMissChance
      enemy.damage = enemy.originalDamage
      enemy.break = false
      enemy.mindControlPower = 0

      player.currentHp = 1
      player.inCombat = false
      player.location = player.location.defeatArea.get

      "You got defeated!"
  else
    if player.didRun then
        enemy.currentHp = enemy.fullHp
        enemy.ultUsed = false
        enemy.dodgeChance = enemy.originalDodgeChance
        enemy.missChance = enemy.originalMissChance
        enemy.damage = enemy.originalDamage
        enemy.break = false
        enemy.mindControlPower = 0

        player.inCombat = false
        ""
    else
      ""


object kitchen extends Area("Kitchen","Oh what a lovely smell, I must be in the kitchen. \n\nCastle chef: -The King is angry and I need to satisfy him with my meal! Help me! If you do, I will reward you with something.\n\nYou can cook in the kitchen with the command: cook"):
  override val areaActionName = Some("cook")
  // a boolean value for tracking if cooking is being complete again for fun
  private var firsTime = true

  //Cook eaction that works in kitchen
  override def areaAction:String =


    if !player.inventory.values.exists(_.isInstanceOf[Food]) then
      "You cannot cook without any food!"
    else
      println("You started cooking! Add ingredients from your inventory by typing their name or finish cooking by typing ready")
      var cooking = true
      //the food that is being cooked is just list of the ingredients
      val theFood = Buffer[String]()

      //Cookin main loop
      while cooking do
        //display all food items in players inventory
        println("You may add: "+player.inventory.filter(a => a._2 match
          case b:Food => true
          case _ => false).keys.toVector.mkString(", "))
        //take input
        var input = readLine()

        //end loop if player desiresso
        if input == "ready" then
          cooking = false
        else if  player.inventory.keys.toVector.contains(input) && player.inventory(input).isInstanceOf[Food] then
          theFood += input
          player.inventory.remove(input)
          println("You added " +input+ " to your food")
        else
          println("You can't add this to your food")

          //End loop if player is out of food
        val foodInventory = player.inventory.filter(a => a._2 match
          case b:Food => true
          case _ => false)

        if foodInventory.isEmpty then
          println("well that's all your food!")
          cooking = false

      //after exiting the cooking loop evalueate cooked meal. The value of a food item depends on the chef and this way of coding it emphasises that. Having the foodscore stored
      // in the food itself makes the score not depend on the chef but on the food. This is bad if for example another chef was added.
      var foodScore = -5
      if theFood.contains("crusty potato") then
        foodScore += -50
      if theFood.contains("smelly onion") then
        foodScore += -100
      if theFood.contains("milk") then
        foodScore += -10
      if theFood.contains("ham") then
        foodScore += 30
      if theFood.contains("quality cheese") then
        foodScore += 50
      if theFood.contains("mushrooms") then
        foodScore += 10

      //outcome depends on the cooked food. Gives different reactions after completing it

      //if no food is added
      if foodScore == -5 then
        if firsTime then
          "Castle chef: What exatcly are we serving? This soup tastes like water to me!"
        else
          "Castle chef: Haha you are funny when you are pretending that you cook, my friend!"

      //if food is good
      else if foodScore > 0 then

        if firsTime then
          kitchen.addItem(BossKey("westkey", "A cool looking key, I wonder what this opens","Hmm how does one use a key without a lock"))
          kitchen.description = "This is a nice kitchen but i think my job has been done here."
          firsTime = false
          "Castle chef: Thank you this soup is delicious! Oh I dropped my key. Whatever, I have multiple of those, keep it if you want."
        else
          "Castle chef: Wow so good! You really seem to enjoy cooking!"



      //if food is good
      else
        if firsTime then
          "Castle chef: You this one tastes bad! We would be killed for serving this!"
        else
          "Castle chef: Your latest meal was amazing... what happened?!"






// nopee testilooppi, myöhemmin varmaan truen voi korvata jollain gameNotOver jne
@main
def run() =
  val castleEntrance = Area("Castle entrance","What an intimidating castle. I must be brave and enter!")
  val hallway = Area("Hallway","What a scary place, I wonder if all of these weapons still work.")
  hallway.areaEnemy = Some(GateKeeper)
  hallway.defeatArea = Some(castleEntrance)
  val eastCorridor = Area("Corridor","There are some nice paintings on this damn long corrdor.")
  val westCorridor = Area("Corridor","What a long corridor. Why don't they have windows here?")

  kitchen.addItem(Food("smelly onion", "Ew I would never eat this onion!","You took a bite of the onion, Jeez this is bad"))
  kitchen.addItem(Food("crusty potato", "This potato is suspicious.","You throw the potato to a wall and it bounces back. I never knew potatoes could do that"))
  kitchen.addItem(Food("milk", "This milk seems at least a couple weeks old.", "You pour some mil to the floor. He he he I wonder if they have cleaners here"))


  val foodCellar = Area("Food Cellar", "Ew what a smell, you must be in the food cellar")
  foodCellar.addItem(Food("ham", "This ham makes me hungry, maybe I should just eat it!","You took a bite of the ham. This was really good. I want to eat te whole thing"))
  foodCellar.addItem(Food("quality cheese", "This cheese looks expensive.","You see a mouse and offer some cheese to your new friend. Atleast I can trust someone in this place."))
  foodCellar.addItem(Food("mushrooms", "Wow at least something is fresh here. I worder who picked these.","You try one, sadly no magic is happending"))



  val bottomOfTheTower = Area("Bottom of the tower", "Those are really steep stairs. If someone lives up there, he must be in shape.")
  bottomOfTheTower.addItem(Item("sword","a sharp looking sword, this will come in handy","You give yourself a new haircut. Now im confident"))

  val topOfTheTower = Area("Top of the tower", "Those stairs were a though climb, I wonder how they are on the way down.")
  topOfTheTower.addItem(BossKey("eastkey", "A cool looking key, I wonder what this opens","Hmm how does one use a key without a lock"))
  topOfTheTower.areaEnemy = Some(MonkOfTheTower)
  topOfTheTower.defeatArea = Some(bottomOfTheTower)

  val mainCorridor = Area("Wide corridor","This has to be the widest corridor that I have ever seen!")
  mainCorridor.addItem(Item("sandbag", "Hmm, maybe this will prove useful","You pour some sand to the floor. He he what a nice prank!"))
  mainCorridor.itemRequirements = Some(Vector("eastkey","westkey")) //the player needs these items to acces the boss area

  val mainHall = Area("Main hall", "This room is massive! Bulding this place has taken some effort.") //final boss area
  mainHall.defeatArea = Some(mainCorridor)
  mainHall.areaEnemy = Some(KingOfTheCastle)

  castleEntrance.addNeighbors(Vector("north" -> hallway))
  hallway.addNeighbors(Vector(("south" -> castleEntrance),("east" -> eastCorridor),("west" -> westCorridor),("north" -> mainCorridor)))
  eastCorridor.addNeighbors(Vector(("west" -> hallway), ("east" -> bottomOfTheTower)))
  westCorridor.addNeighbors(Vector(("east" -> hallway),("west" -> kitchen)))
  kitchen.addNeighbors(Vector(("east" -> westCorridor),("down" -> foodCellar)))
  foodCellar.addNeighbors(Vector("up" -> kitchen))
  bottomOfTheTower.addNeighbors(Vector(("west" -> eastCorridor),("up" -> topOfTheTower)))
  topOfTheTower.addNeighbors(Vector(("down" -> bottomOfTheTower)))
  mainHall.addNeighbors(Vector(("south" -> mainCorridor)))
  mainCorridor.addNeighbors(Vector(("south" -> hallway),("north" -> mainHall)))



  val startingMessage = "The evil King has begun taking slaves from a remote village that you live in. You knew what you had to do \n when you saw your father taken by the soldiers. You are here to end this injustice and kill the king. \n\n You can see all the commands that you can use by typing help as a command"


  player.location = castleEntrance


  println("*"*100)
  println(startingMessage)

  while !player.gameWon do




    //location desription
    println(player.location.fullDescription)

    player.location.areaEnemy match
      case Some(enemy) =>
        println(combat(enemy))
        Thread.sleep(2000)



        println(player.location.fullDescription)
      case None =>
    val input = readLine()
    println(player.action(input))
    println()
    Thread.sleep(1000)
  // game end message
  println("You have killed the evil King and saved your people! Good job! You truly made the world a better place!")




