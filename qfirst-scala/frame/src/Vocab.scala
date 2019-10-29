package qfirst.frame

class Vocab[A] private (
  indexToItem: Vector[A],
  itemToIndex: Map[A, Int]
) {
  def items = indexToItem
  def indices = indexToItem.indices.toVector
  def getItem(index: Int) = indexToItem(index)
  def getIndex(item: A) = itemToIndex(item)
  def size = indexToItem.size
}
object Vocab {
  def make[A](items: Set[A]): Vocab[A] = {
    val itemsVec = items.toVector
    new Vocab(itemsVec, itemsVec.zipWithIndex.toMap)
  }
}
