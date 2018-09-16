import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import ammonite.ops._

val thisScalaVersion = "2.12.6"
// val thisScalaJSVersion = "0.6.23"

val macroParadiseVersion = "2.1.0"
val kindProjectorVersion = "0.9.4"

val shapelessVersion = "2.3.3"

// cats libs -- maintain version agreement or whatever
val catsVersion = "1.1.0"
val catsEffectVersion = "0.10.1"
val kittensVersion = "1.1.1"
val nlpdataVersion = "0.2.0"
val qasrlVersion = "0.1.0"
val qasrlBankVersion = "0.1.0"
val circeVersion = "0.9.3"
val declineVersion = "0.4.2"
val simulacrumVersion = "0.13.0"
  // val monocleVersion = "1.5.1-cats"

// val catsEffectVersion = "0.10.1"
// val radhocVersion = "0.1.0"
// val http4sVersion = "0.18.14"

// val scalatagsVersion = "0.6.7"
// val scalacssVersion = "0.5.3"

val ammoniteOpsVersion = "1.1.2"
// val logbackVersion = "1.2.3"

// val scalajsDomVersion = "0.9.6"
// val scalajsJqueryVersion = "0.9.3"
// val scalajsReactVersion = "1.1.0"
// val scalajsScalaCSSVersion = "0.5.3"

trait CommonModule extends ScalaModule with ScalafmtModule {

  def scalaVersion = thisScalaVersion

  def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ypartial-unification"
  )
  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"org.scalamacros:::paradise:$macroParadiseVersion",
    ivy"org.spire-math::kind-projector:$kindProjectorVersion"
  )

  // add back in when necessary
  // def repositories = super.repositories ++ Seq(
  //   MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  // )
}

trait CrossPlatformModule extends ScalaModule {
  def platformSegment: String

  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

}

trait JvmPlatform extends CrossPlatformModule {
  def platformSegment = "jvm"

  // for using runMain in commands
  def runMainFn = T.task { (mainClass: String, args: Seq[String]) =>
    import mill.modules.Jvm
    import mill.eval.Result
    try Result.Success(
      Jvm.interactiveSubprocess(
        mainClass,
        runClasspath().map(_.path),
        forkArgs(),
        forkEnv(),
        args,
        workingDir = ammonite.ops.pwd
      )
    ) catch {
      case e: InteractiveShelloutException =>
        Result.Failure("subprocess failed")
    }
  }
}

object qfirst extends CommonModule with JvmPlatform {
  def millSourcePath = build.millSourcePath / "qfirst-scala"

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.typelevel::cats-core::$catsVersion",
    ivy"org.typelevel::cats-effect::$catsEffectVersion",
    ivy"com.chuusai::shapeless::$shapelessVersion",
    ivy"org.typelevel::kittens::$kittensVersion",
    ivy"com.github.mpilquist::simulacrum:$simulacrumVersion",
    ivy"org.julianmichael::nlpdata::$nlpdataVersion",
    ivy"org.julianmichael::qasrl::$qasrlVersion",
    ivy"org.julianmichael::qasrl-bank::$qasrlBankVersion",
    // ivy"org.julianmichael::qasrl-bank-service::$qasrlBankVersion",
    ivy"io.circe::circe-core::$circeVersion",
    ivy"io.circe::circe-generic::$circeVersion",
    ivy"com.monovore::decline::$declineVersion",
    ivy"com.lihaoyi::ammonite-ops::$ammoniteOpsVersion"
    // ivy"com.github.julien-truffaut::monocle-core::$monocleVersion",
    // ivy"com.github.julien-truffaut::monocle-macro::$monocleVersion",
  )

  // def generateDev(port: Int, domain: String = "localhost") = T.command {
  //   val browserJSPath = browser.js.fastOpt().path.toString
  //   val browserJSDepsPath = browser.js.aggregatedJSDeps().path.toString
  //   val runMain = runMainFn()
  //   runMain(
  //     "qasrl.apps.browser.Generate", Seq(
  //       "--qasrl-bank",      qasrlBankLocation,
  //       "--api-url",         s"http://$domain:$port",
  //       "--browser-js",      browserJSPath,
  //       "--browser-jsdeps",  browserJSDepsPath,
  //       "--site-root",       "site/browser/dev",
  //       "--local-links"
  //     )
  //   )
  // }
}
