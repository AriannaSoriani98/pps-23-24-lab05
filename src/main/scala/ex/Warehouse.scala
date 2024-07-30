package ex

import util.Optionals.Optional
import util.Sequences.*
trait Item:
  def code: Int
  def name: String
  def tags: Sequence[String]

object Item: /* COMPANION OBJ of trait item*/
  //def apply(code: Int, name: String, tags: Sequence[String] = Sequence.empty): Item = ItemImpl(code,name,tags)
  /* .apply accepts a variable number of tags */
  def apply(code: Int, name: String, tags: String*): Item =
    var varTags: Sequence[String] = Sequence.empty
    for i <- tags do varTags = varTags.concat(Sequence(i))
    ItemImpl(code, name, varTags)
case class ItemImpl(override val code: Int,
                    override val name: String,
                    override val tags: Sequence[String]) extends Item

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
end Warehouse

object Warehouse: /* COMPANION OBJ of trait Warehouse*/
  def apply(): Warehouse = WarehouseImpl(Sequence()) //initially empty

case class WarehouseImpl(private var items: Sequence[Item]) extends Warehouse:
  override def store(item: ex.Item): Unit = items = items.concat(Sequence(item))
  override def contains(itemCode: Int): Boolean = !items.find(_.code == itemCode).isEmpty
 // override def contains(itemCode: Int): Boolean = items.contains(itemCode)
  override def remove(item: ex.Item): Unit = items = items.filter(_.code != item.code)
  override def retrieve(code: Int): util.Optionals.Optional[ex.Item] = items.find(_.code==code)
  override def searchItems(tag: String): util.Sequences.Sequence[ex.Item] = items.filter(_.tags.contains(tag))

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()
/*
  val dellXps = Item(33, "Dell XPS 15", Sequence("notebook"))
  val dellInspiron = Item(34, "Dell Inspiron 13", Sequence("notebook"))
  val xiaomiMoped = Item(35, "Xiaomi S1", Sequence("moped", "mobility"))
*/
/* con tag variabile: */
  val dellXps = Item(33, "Dell XPS 15", "notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")
  println(warehouse.contains(dellXps.code)) // false
  println(warehouse.store(dellXps)) // side effect, add dell xps to the warehouse
  println(warehouse.contains(dellXps.code)) // true
  println(warehouse.store(dellInspiron)) // side effect, add dell Inspiron to the warehouse
  println(warehouse.store(xiaomiMoped)) // side effect, add xiaomi moped to the warehouse
  println(warehouse.searchItems("mobility")) // Sequence(xiaomiMoped)
  println(warehouse.searchItems("notebook")) // Sequence(dellXps, dell Inspiron)
  println(warehouse.retrieve(11)) // None
  println(warehouse.retrieve(dellXps.code)) // Just(dellXps)
  println(warehouse.remove(dellXps)) // side effect, remove dell xps from the warehouse
  println(warehouse.retrieve(dellXps.code)) // None

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private Sequence of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/