package dogs
package bench

import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, Setup, State}
import scala.collection.immutable.{List => SList, Nil => SNil}
import scalaz.{DList => SZDList, IList}

trait Data {
  @Param(Array("10", "100", "1000", "10000"))
  var n: Int = _

  var list: List[Int] = _
  var dlist: DList[Int] = _
  var dequeue: Dequeue[Int] = _
  var szdlist: SZDList[Int] = _

  @Setup
  def setup: Unit = {
    val grouped = (1 to n).toList.grouped(10)
    list = List.fromIterable(1 to n)
    dlist = grouped.foldLeft(DList.empty[Int]) { (d, g) =>
      d ++ DList(List.fromIterable(g))
    }
    dequeue = Dequeue((1 to n):_*)
    szdlist = grouped.foldLeft(SZDList[Int]()) { (d, g) =>
      d ++ SZDList(g:_*)
    }
  }
}

@State(Scope.Thread)
class Append extends Data {

  @Benchmark def dogsListAppend(): Unit = {
    list ++ List(0)
  }

  @Benchmark def dogsDListAppend(): Unit = {
    dlist ++ DList(List(0))
  }

  @Benchmark def scalazDListAppend(): Unit = {
    szdlist ++ SZDList(0)
  }

  @Benchmark def dequeueAppend(): Unit = {
    dequeue :+ 0
  }
}

@State(Scope.Thread)
class ToList extends Data {

  @Benchmark def dogsList(): Unit = {
    list // is already a List
  }

  @Benchmark def dogsDListToList(): Unit = {
    dlist.toList
  }

  @Benchmark def scalazDListToList(): Unit = {
    szdlist.toList
  }

  @Benchmark def dequeueToList(): Unit = {
    dequeue.foldRight(List.empty[Int])(_ :: _)
  }
}

@State(Scope.Thread)
class HeadOption extends Data {

  @Benchmark def dogsDListHeadOption(): Unit = {
    dlist.headOption
  }

  @Benchmark def scalazDListHeadOption(): Unit = {
    szdlist.headOption
  }

  @Benchmark def dequeueHeadOption(): Unit = {
    dequeue.frontOption
  }
}
