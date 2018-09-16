package qfirst

import cats.Functor
import cats.Monoid
import cats.Show
import cats.implicits._

import HasMetrics.ops._

object Metrics {

  case class Bucketed[MetricData](
    data: Map[Map[String, String], MetricData]
  ) {
    def mapBucketValues(key: String, mapper: String => String)(implicit M: Monoid[MetricData]) = Bucketed(
      data.toList.map {
        case (buckets, m) => buckets.get(key).fold(buckets)(v => buckets + (key -> mapper(v))) -> m
      }.groupBy(_._1).map {
        case (newBuckets, allMetrics) => newBuckets -> allMetrics.map(_._2).combineAll
      }
    )

    def collapseBuckets(keys: String*)(implicit M: Monoid[MetricData]): Bucketed[MetricData] = collapseBuckets(keys.toList)

    def collapseBuckets(keys: List[String])(implicit M: Monoid[MetricData]): Bucketed[MetricData] = Bucketed(
      data.toList.map {
        case (buckets, m) => keys.foldLeft(buckets)(_ - _) -> m
      }.groupBy(_._1).map {
        case (newBuckets, allMetrics) => newBuckets -> allMetrics.map(_._2).combineAll
      }
    )

    def filter(p: MetricData => Boolean): Bucketed[MetricData] = Bucketed(
      data.collect { case (k, v) if p(v) => k -> v }
    )

    def collapsed(implicit M : Monoid[MetricData]) = data.values.toList.combineAll
  }
  object Bucketed {
    implicit val bucketedFunctor: Functor[Bucketed] = new Functor[Bucketed] {
      def map[A, B](fa: Bucketed[A])(f: A => B): Bucketed[B] = Bucketed(
        fa.data.map { case (k, v) => k -> f(v) }
      )
    }
    implicit def bucketedMonoid[A : Monoid]: Monoid[Bucketed[A]] = {
      import cats.derived.auto.monoid._
      cats.derived.semi.monoid
    }
    // TODO perhaps draw out the proportion calculation into a separate method so user can decide when to do it
    implicit def bucketedHasMetrics[A : HasMetrics : Monoid]: HasMetrics[Bucketed[A]] = new HasMetrics[Bucketed[A]] {
      def getMetrics(ba: Bucketed[A]): MapTree[String, MetricValue] = {
        val collapsedMetrics = ba.collapsed.getMetrics
        val computeIntsOfTotal = (x: MetricValue, y: MetricValue) => (x, y) match {
          case (MetricValue.MetricInt(xi), MetricValue.MetricInt(yi)) => MetricValue.MetricIntOfTotal(xi, yi)
          case (x, _) => x
        }
        MapTree.fork(
          ba.data.map { case (bucketSpec, bucketData) =>
            val keyStr = "{ " + bucketSpec.toList.sortBy(_._1).map { case (k, v) => s"$k: $v"}.mkString(", ") + " }"
            keyStr -> bucketData.getMetrics.merge(collapsedMetrics, computeIntsOfTotal)
          }
        )
      }
    }
  }

  def bucket[Instance, MetricData](
    bucketers: Map[String, Instance => String])(
    metric: Instance => MetricData
  ): (Instance => Bucketed[MetricData]) = (instance: Instance) => {
    val bucket = bucketers.map { case (key, bucketer) => key -> bucketer(instance) }
    Bucketed(Map(bucket -> metric(instance)))
  }

  case class Chosen[Param, MetricData](
    data: Map[Param, MetricData]
  )
  object Chosen {
    implicit def chosenFunctor[Param]: Functor[Chosen[Param, ?]] = new Functor[Chosen[Param, ?]] {
      def map[A, B](fa: Chosen[Param, A])(f: A => B): Chosen[Param, B] = Chosen(
        fa.data.map { case (k, v) => k -> f(v) }
      )
    }
    implicit def chosenMonoid[Param, A: Monoid]: Monoid[Chosen[Param, A]] = {
      import cats.derived.auto.monoid._
      cats.derived.semi.monoid
    }
    implicit def chosenHasMetrics[Param: Show, A: HasMetrics]: HasMetrics[Chosen[Param, A]] = new HasMetrics[Chosen[Param, A]] {
      def getMetrics(ba: Chosen[Param, A]): MapTree[String, MetricValue] = {
        MapTree.fork(
          ba.data.map { case (param, bucketData) =>
            param.show -> bucketData.getMetrics
          }
        )
      }
    }
  }

  def choose[Param, Instance, MetricData](
    params: List[Param])(
    metric: Param => Instance => MetricData
  ): (Instance => Chosen[Param, MetricData]) = (instance: Instance) => {
    Chosen(params.map(p => p -> metric(p)(instance)).toMap)
  }

  // def choose[Param, Instance, MetricData](
  //   choices: (Param, Instance => MetricData)*
  // ): (Instance => Chosen[Param, MetricData]) = (instance: Instance) => {
  //   Chosen(choices.map { case (p, metric) => p -> metric(instance) }.toMap)
  // }

  def split[BigInstance, SmallInstance, MetricData: Monoid](
    splittingFn: BigInstance => List[SmallInstance])(
    metric: SmallInstance => MetricData
  ) = (bigInstance: BigInstance) => {
    splittingFn(bigInstance).foldMap(metric)
  }

  import shapeless.HList
  import shapeless.::
  import shapeless.HNil
  import shapeless.Witness
  import shapeless.labelled.field
  import shapeless.labelled.FieldType

  sealed trait InstanceMapper[Instance, Fn <: HList] {
    type Out <: HList
    def apply(fn: Fn): Instance => Out
  }
  object InstanceMapper {
    type Aux[I, Fn <: HList, Out0 <: HList] = InstanceMapper[I, Fn] { type Out = Out0 }

    implicit def hnilInstanceMapper[I]: Aux[I, HNil, HNil] =
      new InstanceMapper[I, HNil] {
        type Out = HNil
        def apply(fn: HNil) = (i: I) => HNil
      }

    implicit def hconsInstanceMapper[I, O, K, Tail <: HList, TailOut <: HList](
      implicit tailMapper: Aux[I, Tail, TailOut]
    ): Aux[I, FieldType[K, I => O] :: Tail, FieldType[K, O] :: TailOut] =
      new InstanceMapper[I, FieldType[K, I => O] :: Tail] {
        type Out = FieldType[K, O] :: TailOut
        def apply(fn: FieldType[K, I => O] :: Tail) = (i: I) => field[K](fn.head(i)) :: tailMapper(fn.tail)(i)
    }
  }

  def hchoose[Instance, Mappers <: HList, Rec <: HList](
    choices: Mappers)(
    implicit instanceMapper: InstanceMapper.Aux[Instance, Mappers, Rec]
  ): (Instance => Rec) = (instance: Instance) => {
    instanceMapper(choices)(instance)
  }
}
