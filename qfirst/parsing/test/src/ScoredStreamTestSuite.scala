package qfirst.parsing

class ScoredStreamTestSuite extends munit.CatsEffectSuite {
  import ScoredStreamExamples._
  val nats = intsFrom(0)
  val ten = nats.take(10)

  def time(compute: => Unit): Long = {
    val begin = System.nanoTime
    compute
    val end = System.nanoTime
    end - begin
  }

  // TODO need way more tests lol

  test("ten first natural numbers") {
    assertEquals(ten.toList, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  test("insert") {
    assertEquals(ten.insert(Scored(-1, 0.0)).headOption.get.item, -1)
    assertEquals(ten.insert(Scored(-1, 0.0)).tailOption.get.toList, ten.toList)
  }

  test("toStream") {
    assertEquals(nats.toStream.toString, "Stream(0, ?)")
  }

  test("remove") {
    assertEquals(nats.removeFirst(_ == 0).headOption.get.item, 1)
  }

  test("mapScored") {
    val multiplesOf4 = nats.mapScored {
      case i => Scored(4 * i, i)
    }.take(4).toList
    assertEquals(multiplesOf4, List(0, 4, 8, 12))
  }

  test("flatMap") {
    val grid = nats.flatMap(intsFrom)
    assertEquals(grid.take(15).toList, List(0, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4))

    // val bigly = nats.flatMap {
    //   case i => ScoredStream.unit(Scored(i, i))
    // }
  }

  test("filter") {
    assertEquals(nats.filter(_ % 2 == 0).take(3).toList, List(0, 2, 4))
  }

  test("takeWhile") {
    assertEquals(nats.takeWhile(_ < 10).toList, nats.take(10).toList)
  }

  test("collect") {
    val multiplesOf4 = nats.collect {
      case i if i % 2 == 0 => Scored(2 * i, i)
    }.take(4).toList
    assertEquals(multiplesOf4, List(0, 4, 8, 12))
  }

    // "merge" - {

    // }

  test("fromIndexedSeq") {
    val someVector = Vector.fill(7000)(util.Random.nextInt(200))
    val someVectorScored = someVector.map(x => Scored(x, x.toDouble))

    assertEquals(
      ScoredStream.fromIndexedSeq(someVectorScored).toList.toVector,
      someVector.sorted
    )
  }
}
