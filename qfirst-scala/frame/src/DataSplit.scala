package qfirst.frame

sealed trait DataSplit
object DataSplit {
  case object Train extends DataSplit
  case object Dev extends DataSplit
  case object Test extends DataSplit
}
