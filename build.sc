import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import ammonite.ops._

val thisScalaVersion = "2.12.6"
val thisScalaJSVersion = "0.6.23"

val macroParadiseVersion = "2.1.0"
val kindProjectorVersion = "0.9.4"

val shapelessVersion = "2.3.3"

// cats libs -- maintain version agreement or whatever
val catsVersion = "1.1.0"
val catsEffectVersion = "0.10.1"
val kittensVersion = "1.1.1"
val nlpdataVersion = "0.2.1-SNAPSHOT"
val qasrlVersion = "0.1.1-SNAPSHOT"
val qasrlBankVersion = "0.1.0"
val circeVersion = "0.9.3"
val declineVersion = "0.4.2"
val simulacrumVersion = "0.13.0"
val monocleVersion = "1.5.1-cats"
// js cats libs
val radhocVersion = "0.1.1-SNAPSHOT"
// jvm webby cats libs
val http4sVersion = "0.18.14"

// non-cats
val upickleVersion = "0.5.1"
val spacroVersion = "0.2.0"

// jvm webby libs
val scalatagsVersion = "0.6.7"
val scalacssVersion = "0.5.3"

// jvm libs
val ammoniteOpsVersion = "1.1.2"
val logbackVersion = "1.2.3"
// jvm crowd libs
val akkaActorVersion = "2.4.20"
val scalaLoggingVersion = "3.5.0"
val slf4jApiVersion = "1.7.21"

// js libs
val scalajsDomVersion = "0.9.6"
val scalajsJqueryVersion = "0.9.3"
val scalajsReactVersion = "1.2.3"
val scalajsScalaCSSVersion = "0.5.3"

val scalatestVersion = "3.0.5"
val scalacheckVersion = "1.13.5"
val disciplineVersion = "0.9.0"

import $file.`scripts-build`.SimpleJSDepsBuild, SimpleJSDepsBuild.SimpleJSDeps
import $file.`scripts-build`.ScalatexBuild, ScalatexBuild.ScalatexModule

trait CommonModule extends ScalaModule with ScalafmtModule {

  def platformSegment: String

  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  def scalaVersion = thisScalaVersion

  def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ypartial-unification",
  )

  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"org.scalamacros:::paradise:$macroParadiseVersion",
    ivy"org.spire-math::kind-projector:$kindProjectorVersion"
  )

  // add back in when necessary
  // def repositories = super.repositories ++ Seq(
  //   MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  // )

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core::$catsVersion",
    ivy"org.typelevel::cats-effect::$catsEffectVersion",
    ivy"com.github.mpilquist::simulacrum:$simulacrumVersion",
    ivy"com.github.julien-truffaut::monocle-core::$monocleVersion",
    ivy"com.github.julien-truffaut::monocle-macro::$monocleVersion",
    ivy"com.github.julien-truffaut::monocle-generic::$monocleVersion",
    ivy"org.julianmichael::nlpdata::$nlpdataVersion",
    ivy"org.julianmichael::qasrl::$qasrlVersion",
    ivy"org.julianmichael::qasrl-crowd::$qasrlVersion",
    ivy"org.julianmichael::qasrl-bank::$qasrlBankVersion",
    ivy"org.julianmichael::qasrl-bank-service::$qasrlBankVersion",
    ivy"io.circe::circe-core::$circeVersion",
    ivy"io.circe::circe-parser::$circeVersion",
    ivy"io.circe::circe-generic::$circeVersion",
    // crowd stuff
    ivy"org.julianmichael::spacro::$spacroVersion",
    ivy"com.lihaoyi::upickle::$upickleVersion",
  )
}

trait JsPlatform extends CommonModule with ScalaJSModule {
  def scalaJSVersion = T(thisScalaJSVersion)
  def platformSegment = "js"
}

trait JvmPlatform extends CommonModule {
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

trait QfirstModule extends CommonModule {
  def millSourcePath = build.millSourcePath / "qfirst-scala"
}

object qfirst extends Module {
  object js extends QfirstModule with JsPlatform with SimpleJSDeps {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.julianmichael::radhoc::$radhocVersion",
      ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
      ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
      ivy"com.github.japgolly.scalajs-react::core::$scalajsReactVersion",
      ivy"com.github.japgolly.scalajs-react::ext-monocle::$scalajsReactVersion",
      ivy"com.github.japgolly.scalajs-react::ext-cats::$scalajsReactVersion",
      ivy"com.github.japgolly.scalacss::core::$scalajsScalaCSSVersion",
      ivy"com.github.japgolly.scalacss::ext-react::$scalajsScalaCSSVersion",
      // crowd stuff
    )
    def jsDeps = Agg(
      "https://code.jquery.com/jquery-2.1.4.min.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react-dom.js"
    )
    // def mainClass = T(Some("qfirst.frames.crowd.Dispatcher"))
  }
  object jvm extends QfirstModule with JvmPlatform with ScalatexModule {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.chuusai::shapeless::$shapelessVersion",
      ivy"org.typelevel::kittens::$kittensVersion",
      ivy"com.monovore::decline::$declineVersion",
      ivy"com.lihaoyi::ammonite-ops::$ammoniteOpsVersion",
      // webby stuff
      ivy"com.github.japgolly.scalacss::core:$scalacssVersion",
      ivy"com.github.japgolly.scalacss::ext-scalatags:$scalacssVersion",
      ivy"com.monovore::decline::$declineVersion",
      ivy"org.http4s::http4s-dsl::$http4sVersion",
      ivy"org.http4s::http4s-blaze-server::$http4sVersion",
      ivy"ch.qos.logback:logback-classic:$logbackVersion",
      // crowd stuff
      ivy"com.typesafe.akka::akka-actor::$akkaActorVersion",
      ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
      ivy"org.slf4j:slf4j-api:$slf4jApiVersion", // decided to match scala-logging transitive dep
    )

    def resources = T.sources(
      millSourcePath / "resources",
      qfirst.js.fastOpt().path / RelPath.up,
      qfirst.js.aggregatedJSDeps().path / RelPath.up
    )

    def runMetrics(args: String*) = T.command {
      val runMain = runMainFn()
      runMain("qfirst.MetricsApp", args)
    }

    def runLearn(args: String*) = T.command {
      val runMain = runMainFn()
      runMain("qfirst.learn.LearnApp", args)
    }

    def runReprocess(args: String*) = T.command {
      val runMain = runMainFn()
      runMain("qfirst.reprocess.ReprocessApp", args)
    }

    def runFrameLearn(args: String*) = T.command {
      val runMain = runMainFn()
      runMain("qfirst.frames.FrameLearnApp", args)
    }

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

    val qasrlBankLocation = "qasrl-v2_1"

    def runAnnotation(port: Int, domainRestriction: String = "") = T.command {
      val runMain = runMainFn()
      runMain(
        "qfirst.frames.ServeAnnotation", Seq(
          "--qasrl-bank", qasrlBankLocation,
          "--port",       s"$port"
        ) ++ Option(domainRestriction).filter(_.nonEmpty).toSeq.flatMap(d => Seq("--domain", d))
      )
    }

    object test extends Tests with CommonModule {
      def scalaVersion = jvm.this.scalaVersion
      def platformSegment = jvm.this.platformSegment
      def ivyDeps = Agg(
        ivy"org.scalatest::scalatest:$scalatestVersion",
        ivy"org.scalacheck::scalacheck:$scalacheckVersion",
        ivy"org.typelevel::discipline:$disciplineVersion"
      )
      def testFrameworks = Seq("org.scalatest.tools.Framework")
    }
  }
}
