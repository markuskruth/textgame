package Main

import scala.util.Random
import scala.math.*

trait Enemy(val name:String, val fullHp:Int, var damage:Int, var dodgeChance:Double, var missChance:Double, val description:String):
  val originalDamage = this.damage
  val originalDodgeChance = this.dodgeChance
  val originalMissChance = this.missChance

  var currentHp = fullHp

  var break = false //if a fight needs to be stopped (for example King of the Castle)

  var ultUsed = false //some enemies have ults

  var mindControlPower = 0 //the monk needs this



  //each enemy has its own set of attacks and a ways of using them
  val attacks:Vector[String]
  def attack():String




object DefaultEnemy extends Enemy("a thug",100, 20, 0.3, 0.3, "You got attacked by a thug!"):
  override val attacks = Vector("punch","punch","heal") //chance of punching is higher than healing

  override def attack(): String =
    if Random.nextInt(100) < this.missChance*100 then
      "The enemy missed!"
    else
      Random.shuffle(attacks).head match //choose an attack by random
        case "punch" =>
          val damageDone = damage + Random.nextInt(5)
          player.currentHp -= damageDone
          s"The enemy punched you doing $damageDone damage!"

        case "heal" =>
          val lastHp = this.currentHp
          this.currentHp =  min(this.fullHp, this.currentHp + 10)
          s"The enemy drank a mysterious potion which regenerated ${this.currentHp - lastHp}Hp!"


object GateKeeper extends Enemy("The Castle Gatekeeper",200,30,0.3,0.2,"You enter through the door, but you are stopped early on. Someone has grabbed you from your clothes..."):
  override val attacks = Vector("keythrow","keychainswing","powerpotion")

  override def attack(): String =
    if Random.nextInt(100) < this.missChance*100 then
      "The Gatekeeper missed!"
    else
      Random.shuffle(attacks).head match //choose an attack by random
        case "keythrow" =>
          var damageDone = (3*damage/4) + Random.nextInt(5)
          var criticalHit = false
          if Random.nextInt(10) < 5 then
            damageDone = (damageDone*1.5).toInt
            criticalHit = true

          player.currentHp -= damageDone
          if criticalHit then
            s"The Gatekeeper threw a key at you landing a critical hit doing $damageDone damage!"
          else
            s"The Gatekeeper threw a key at you doing $damageDone damage!"

        case "keychainswing" =>
          var damageDone = damage + Random.nextInt(5)
          player.currentHp -= damageDone

          s"The Gatekeeper swung you with its keychain doing $damageDone damage!"

        case "powerpotion" =>
          this.damage += 5
          s"The Gatekeeper drank a mysterious potion which raised its damage by 5!"

object MonkOfTheTower extends Enemy("The Monk of the Tower", 300, 0, 0.0, 0.0, "You see a big looking creature sitting in the middle of the room with its back to you. Suddenly it spins around to face you..."):

  override val attacks = Vector("mindcontrol","meditate")

  override def attack(): String =
    if !ultUsed && this.currentHp <= 200 then
      this.currentHp = this.fullHp
      ultUsed = true
      Thread.sleep(2000)
      "The monk found the peace of mind and regenerated all of its HP back!"

    else if Random.nextInt(100) < this.missChance*100 then
      "The monks attack failed"
    else
      Random.shuffle(attacks).head match //choose an attack by random
        case "mindcontrol" =>
          val damageDone = player.realDamage + this.mindControlPower
          player.currentHp -= damageDone
          s"The monk took control of your mind making you hit yourself doing $damageDone damage!"

        case "meditate" =>
          val hpRegenerated = 10 + mindControlPower
          this.currentHp += hpRegenerated
          this.mindControlPower += 3
          s"The monk medidated and regenerated ${hpRegenerated}HP and its mind control grew stronger"

object KingOfTheCastle extends Enemy("The King of the Castle", 300, 20, 1.0, 0.1, "In the back of the room you see a big and strong looking man sitting on a throne.\n" +
  "You notice he isn't wearing a crown though... The King stands up and unseathes his sword..."):
  val secondPhaseAttacks = Vector("fireball","swordthrow","teleport")

  override val attacks = Vector("swordswing","kick")

  override def attack(): String =
    if !ultUsed && this.currentHp <= 100 then
      ultUsed = true
      this.currentHp = this.fullHp/2
      player.location = player.location.defeatArea.get
      this.break = true
      Thread.sleep(2000)
      "The King grabbed you and threw you with incredible power against the Main Hall door making you go straight through it!"

    else if Random.nextInt(100) < this.missChance*100 then
      "The King missed!"

    else
      if !ultUsed then
        Random.shuffle(attacks).head match //choose an attack by random
          case "swordswing" =>
            val damageDone = this.damage + Random.nextInt(10)
            player.currentHp -= damageDone
            s"The King swung you with his sword doing $damageDone damage!"

          case "kick" =>
            val damageDone = this.damage + Random.nextInt(10)
            player.currentHp -= damageDone
            s"The King landed a powerful kick on you doing $damageDone damage!"

      else
        Random.shuffle(secondPhaseAttacks).head match
          case "fireball" =>
            val damageDone = this.damage + 30 + Random.nextInt(5)
            player.currentHp -= damageDone
            s"The King summoned a fireball and threw it at you doing $damageDone damage!"

          case "swordthrow" =>
            val damageDone = this.damage + 30 - Random.nextInt(5)
            player.currentHp -= damageDone
            s"The King threw his sword at you doing $damageDone damage! The sword flew back to his hand."

          case "teleport" =>
            val damageDone = this.damage + 30 - Random.nextInt(10)
            player.currentHp -= damageDone
            s"The King teleported behind you and hit you making you fly accross the room doing $damageDone damage!"