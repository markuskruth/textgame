package Main
import scala.collection.mutable.Map


class Area(val name:String, var description:String):
  private val items_ = Map[String,Item]()
  val neighbors = Map[String,Area]() //Map that contains all the areas accesible from this area (the keys are directions, like North,East,Up)

  def items = this.items_

  val areaActionName:Option[String]= None
  def areaAction:String = null
  var areaEnemy:Option[Enemy] = None
  var defeatArea:Option[Area] = None
  var itemRequirements:Option[Vector[String]] = None

  def fullDescription =
    val exitList = "\n\nExits available: " + this.neighbors.keys.mkString(", ")
    //top banner for area name
    println("="*40)
    println(" "*(20-this.name.length/2)+this.name)
    println("="*40)
    
    if items_.nonEmpty then
      val itemList = "\nYou see here: " + this.items_.keys.mkString(", ") 
      this.description + itemList + exitList
    else
      this.description + exitList

  // removes a given item from this area (for example used when player picks up an item)
  def removeItem(itemName:String):Option[Item] =
    val poisto = items_.get(itemName)
    items_ -= itemName
    poisto
  
  //adds an item to the are
  def addItem(item:Item) = this.items_ += (item.name -> item)
  
  //Adds neighbors to this area by adding the direction of the neighbor as key and the neigbor area as area to a map. Key can be any string and this allows for
  //unusual directions such as up down or even teleportation.
  def addNeighbors(neighbors: Vector[(String, Area)]) =
    neighbors.foreach(neighbor => this.neighbors+=(neighbor._1 -> neighbor._2))

  //Checks if there is a neighbor in given direction and returns None if there isn't
  def neighborInDirection(direction: String):Option[Area] =
    this.neighbors.get(direction)
  override def toString: String = name
