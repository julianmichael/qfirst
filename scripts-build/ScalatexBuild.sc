import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import ammonite.ops._

trait ScalatexModule extends ScalaModule {

  def thisScalatexVersion = "0.3.12"
  def thisScalatagsVersion = "0.6.7"

  def scalatexSources = sources

  def scalatexSourceFilesWithRoots = T {
    for {
      root <- scalatexSources()
      if exists(root.path) && root.path.isDir
      path <- ls.rec(root.path)
      if path.isFile && path.ext == "scalatex"
    } yield (root.path, PathRef(path))
  }

  def compileScalatex = T {
    val dest = T.ctx().dest
    val sourceFilesWithRoots = scalatexSourceFilesWithRoots()
    if(sourceFilesWithRoots.size > 0) {
      println(s"Generating ${sourceFilesWithRoots.size} Scalatex sources...")
      ls.rec(dest).foreach(rm)
      sourceFilesWithRoots.map { case (root, pathRef) =>
        val path = pathRef.path
        val objectName = path.name.substring(0, path.name.lastIndexOf('.'))
        val packageName = path.relativeTo(root).segments.dropRight(1).mkString(".")

        val packageSegments = path.relativeTo(root).segments.dropRight(1)
        val intermediatePath = dest / "standin" / packageSegments / path.name
        val targetPath = dest / "target" / packageSegments / s"$objectName.scala"

        val allLines = path.getLines("utf-8")
        val (packagesParamsAndImports, contentLines) = allLines.span(l =>
          l.isEmpty || l.startsWith("@package ") || l.startsWith("@param ") || l.startsWith("@import ")
        )
        val packages = packagesParamsAndImports
          .filter(_.startsWith("@package ")).map(_.substring(1)).mkString("\n")
        // need to extract and put before params in case we need to import param types
        val imports = packagesParamsAndImports
          .filter(_.startsWith("@import ")).map(_.substring(1)).mkString("\n")
        val params = packagesParamsAndImports
          .filter(_.startsWith("@param ")).map(_.substring(7)).mkString(", ")
        // replace all chopped lines here with blanks in the intermediate so that
        // errors are reported at the correct line number
        write(intermediatePath, packagesParamsAndImports.map(_ => "\n").mkString + contentLines.mkString("\n"))

        val reproducedCommentedLines = path.getLines("utf-8").map("//" + _).mkString("\n")
        val fileContents =
          s"""$packages
          |import scalatags.Text.all._
          |$imports
          |
          |object $objectName {
          |  def apply($params): scalatags.Text.all.Frag = _root_.scalatex.twf("$intermediatePath")
          |  def sourcePath = "${path.relativeTo(root)}"
          |}
          |
          |$reproducedCommentedLines""".stripMargin
        write(targetPath, fileContents)
      }
    }
    PathRef(dest / "target")
  }
  override def generatedSources = T { Seq(compileScalatex()) }

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.lihaoyi::scalatex-api:$thisScalatexVersion",
    ivy"com.lihaoyi::scalatags::$thisScalatagsVersion"
  )
}
