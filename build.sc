import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import ammonite.ops._

val thisScalaVersion = "2.12.8"
val thisScalaJSVersion = "0.6.27"

val macroParadiseVersion = "2.1.0"
val kindProjectorVersion = "0.9.4"
val splainVersion = "0.3.4"
val betterMonadicForVersion = "0.3.1"

val jjmVersion = "0.1.0-SNAPSHOT"
val qasrlVersion = "0.2.0-SNAPSHOT"
val qasrlBankVersion = "0.2.0-SNAPSHOT"
val radhocVersion = "0.3.0-SNAPSHOT"
val spacroVersion = "0.3.0-SNAPSHOT"

// val http4sVersion = "0.20.11"
val kittensVersion = "1.1.1"
val declineVersion = "1.0.0"

val breezeVersion = "0.13.2"
val evilplotVersion = "0.6.3"

// non-cats
// val upickleVersion = "0.5.1"

// jvm webby libs
val scalatagsVersion = "0.6.7"

// jvm libs
val ammoniteOpsVersion = "1.1.2"
val logbackVersion = "1.2.3"
// jvm crowd libs
// val akkaActorVersion = "2.4.20"
val scalaLoggingVersion = "3.5.0"
val slf4jApiVersion = "1.7.21"

// js libs
val scalajsDomVersion = "0.9.6"
val scalajsJqueryVersion = "0.9.3"
// val scalajsReactVersion = "1.2.3"
// val scalajsReactVersion = "1.3.1"
val scalacssVersion = "0.5.3"

val scalatestVersion = "3.0.8"
val scalacheckVersion = "1.14.0"
val disciplineVersion = "1.0.0"

import $file.`scripts-build`.SimpleJSDepsBuild, SimpleJSDepsBuild.SimpleJSDeps
import $file.`scripts-build`.ScalatexBuild, ScalatexBuild.ScalatexModule

trait CommonModule extends ScalaModule with ScalafmtModule {

  def platformSegment: String

  override def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  def scalaVersion = thisScalaVersion

  override def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ypartial-unification",
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    // ivy"io.tryp:::splain:$splainVersion",
    ivy"org.scalamacros:::paradise:$macroParadiseVersion",
    ivy"org.spire-math::kind-projector:$kindProjectorVersion",
    ivy"com.olegpy::better-monadic-for:$betterMonadicForVersion"
  )

  // add back in when necessary
  // def repositories = super.repositories ++ Seq(
  //   MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  // )

  override def ivyDeps = Agg(
    ivy"org.julianmichael::jjm-core::$jjmVersion",
    ivy"org.julianmichael::jjm-io::$jjmVersion",
    // ivy"org.typelevel::alleycats-core::$catsVersion",
    ivy"org.typelevel::kittens::$kittensVersion",
    ivy"org.julianmichael::qasrl::$qasrlVersion",
    ivy"org.julianmichael::qasrl-bank::$qasrlBankVersion",
    ivy"org.julianmichael::qasrl-bank-service::$qasrlBankVersion",
    ivy"com.github.japgolly.scalacss::core::$scalacssVersion"
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
      Jvm.runSubprocess(
        mainClass,
        runClasspath().map(_.path),
        forkArgs(),
        forkEnv() ++ Seq("JAVA_OPTS" -> "-Xmx12g"),
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
  override def millSourcePath = build.millSourcePath / "qfirst-scala"
}

object qfirst extends Module {
  object js extends QfirstModule with JsPlatform with SimpleJSDeps {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.julianmichael::radhoc::$radhocVersion",
      ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
      ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
      ivy"com.github.japgolly.scalacss::ext-react::$scalacssVersion"
    )
    override def jsDeps = Agg(
      "https://code.jquery.com/jquery-2.1.4.min.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react-dom.js"
    )
  }
  object jvm extends QfirstModule with JvmPlatform with ScalatexModule {
    override def repositories = super.repositories ++ Seq(
      coursier.MavenRepository("https://dl.bintray.com/cibotech/public")
    )
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.monovore::decline::$declineVersion",
      ivy"com.monovore::decline-effect::$declineVersion",
      ivy"com.lihaoyi::ammonite-ops::$ammoniteOpsVersion",
      ivy"org.scalanlp::breeze:$breezeVersion",
      ivy"org.scalanlp::breeze-natives:$breezeVersion",
      ivy"com.cibo::evilplot-repl:$evilplotVersion",
      // webby stuff
      // ivy"com.github.japgolly.scalacss::ext-scalatags:$scalacssVersion",
      // ivy"org.http4s::http4s-dsl::$http4sVersion",
      // ivy"org.http4s::http4s-blaze-server::$http4sVersion",
      ivy"ch.qos.logback:logback-classic:$logbackVersion",
      // crowd stuff
      // ivy"com.typesafe.akka::akka-actor::$akkaActorVersion",
      ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
      ivy"org.slf4j:slf4j-api:$slf4jApiVersion", // decided to match scala-logging transitive dep
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

    def runSandbox(args: String*) = T.command {
      val runMain = runMainFn()
      runMain("qfirst.SandboxApp", args)
    }

    def runClauseRank(args: String*) = T.command {
      val runMain = runMainFn()
      runMain("qfirst.ClauseRankApp", args)
    }

    def runHyperparams(args: String*) = T.command {
      val runMain = runMainFn()
      runMain("qfirst.ModelVariants", args)
    }

    def runClauseAnn(args: String*) = T.command {
      val jsPath = qfirst.js.fastOpt().path.toString
      val jsDepsPath = qfirst.js.aggregatedJSDeps().path.toString
      val runMain = runMainFn()
      runMain(
        "qfirst.frames.annotation.Serve",
        List(
          "--jsDeps", jsDepsPath,
          "--js", jsPath
        ) ++ args)
    }

    def runVerbAnn(args: String*) = T.command {
      val jsPath = qfirst.js.fastOpt().path.toString
      val jsDepsPath = qfirst.js.aggregatedJSDeps().path.toString
      val runMain = runMainFn()
      runMain(
        "qfirst.frames.verbal.Serve",
        List(
          "--jsDeps", jsDepsPath,
          "--js", jsPath
        ) ++ args)
    }

    def runBrowse(args: String*) = T.command {
      val jsPath = qfirst.js.fastOpt().path.toString
      val jsDepsPath = qfirst.js.aggregatedJSDeps().path.toString
      val runMain = runMainFn()
      runMain(
        "qfirst.paraphrase.browse.Serve",
        List(
          "--jsDeps", jsDepsPath,
          "--js", jsPath
        ) ++ args)
    }

    def runTopics(args: String*) = T.command {
      val runMain = runMainFn()
      runMain("qfirst.topics.TopicModelingApp", args)
    }

    object test extends Tests with CommonModule {
      override def scalaVersion = jvm.this.scalaVersion
      def platformSegment = jvm.this.platformSegment
      override def ivyDeps = Agg(
        ivy"org.scalatest::scalatest:$scalatestVersion",
        ivy"org.scalacheck::scalacheck:$scalacheckVersion",
        ivy"org.typelevel::discipline-core:$disciplineVersion"
        // ivy"org.typelevel::discipline-scalatest:$disciplineVersion-SNAPSHOT"
      )
      def testFrameworks = Seq("org.scalatest.tools.Framework")
    }
  }
}

trait ClausalDemoModule extends QfirstModule {
  override def millSourcePath = build.millSourcePath / "clausal-demo"
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.julianmichael::spacro::$spacroVersion",
    ivy"org.julianmichael::qasrl-crowd::$qasrlVersion"
  )
}

object `clausal-demo` extends Module {
  object js extends ClausalDemoModule with JsPlatform with SimpleJSDeps {
    def moduleDeps = List(qfirst.js)

    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.julianmichael::radhoc::$radhocVersion",
      ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
      ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
      ivy"com.github.japgolly.scalacss::ext-react::$scalacssVersion"
    )
    override def jsDeps = Agg(
      "https://code.jquery.com/jquery-2.1.4.min.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react-dom.js"
    )
  }
  object jvm extends ClausalDemoModule with JsPlatform with SimpleJSDeps {
    def moduleDeps = List(qfirst.jvm)

    override def resources = T.sources(
      millSourcePath / "resources",
      `clausal-demo`.js.fastOpt().path / RelPath.up,
      `clausal-demo`.js.aggregatedJSDeps().path / RelPath.up
    )

    override def ivyDeps = super.ivyDeps() ++ Agg(
      // ivy"com.monovore::decline::$declineVersion",
      // ivy"com.monovore::decline-effect::$declineVersion",
      ivy"com.lihaoyi::ammonite-ops::$ammoniteOpsVersion",
      // webby stuff
      ivy"com.github.japgolly.scalacss::core:$scalacssVersion",
      // ivy"com.github.japgolly.scalacss::ext-scalatags:$scalacssVersion",
      ivy"ch.qos.logback:logback-classic:$logbackVersion",
      // crowd stuff
      ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
      ivy"org.slf4j:slf4j-api:$slf4jApiVersion" // decided to match scala-logging transitive dep
    )
  }
}
