package Main

class Item(val name: String, val description: String, val use: String):
  override def toString = this.name

class Food(val foodName: String, val foodDescription: String, val foodUse: String) extends Item(foodName,foodDescription,foodUse):
  override def toString = "a delicious "+this.name

class BossKey(val keyName:String, val keyDescription:String, val keyUse: String) extends  Item(keyName,keyDescription, keyUse):
  override def toString: String = this.keyName

