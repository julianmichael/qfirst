package qfirst.datasets.ptb2

import io.circe.generic.JsonCodec

@JsonCodec case class PTB2FilePath(
  section: Int,
  fileName: String) { // with .mrg suffix
  def pathSuffix = f"$section%02d/$fileName%s"
  override def toString = pathSuffix
}

@JsonCodec case class PTB2SentenceId(filePath: PTB2FilePath, sentenceNum: Int) {
  override def toString = s"$filePath:$sentenceNum"
}

case class PTB2File(
  path: PTB2FilePath,
  sentences: Vector[PTB2Sentence])

case class PTB2Sentence(
  id: PTB2SentenceId,
  tokens: Vector[PTB2Token],
  syntaxTree: SyntaxTree[PTB2Token])
