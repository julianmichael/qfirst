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

val jjmVersion = "0.1.1-SNAPSHOT"
val qasrlVersion = "0.2.1-SNAPSHOT"
val qasrlBankVersion = "0.2.0"
val radhocVersion = "0.3.0"
val spacroVersion = "0.3.0"

val http4sVersion = "0.20.11"
val kittensVersion = "1.1.1"
val declineVersion = "1.0.0"

val breezeVersion = "0.13.2"
val evilplotVersion = "0.6.3"
val fansiVersion = "0.2.7"
val scalaCsvVersion = "1.3.6"

// non-cats
// val upickleVersion = "0.5.1"
val fastparseVersion = "0.4.4"
val macmemoVersion = "0.4"

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

  def millSourcePath = super.millSourcePath / RelPath.up

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

trait JsModule extends CommonModule with ScalaJSModule {
  def scalaJSVersion = T(thisScalaJSVersion)
  def platformSegment = "js"
}

trait FullJsModule extends JsModule with SimpleJSDeps {
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

trait JvmModule extends CommonModule {
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

trait FullJvmModule extends JvmModule {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.monovore::decline::$declineVersion",
    ivy"com.monovore::decline-effect::$declineVersion",
    ivy"ch.qos.logback:logback-classic:$logbackVersion",
    // ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
    // ivy"org.slf4j:slf4j-api:$slf4jApiVersion", // decided to match scala-logging transitive dep
    // ivy"com.lihaoyi::ammonite-ops::$ammoniteOpsVersion"
    // ivy"com.github.japgolly.scalacss::ext-scalatags:$scalacssVersion",
    // ivy"com.typesafe.akka::akka-actor::$akkaActorVersion",
  )
}

object qfirst extends Module {

  override def millSourcePath = build.millSourcePath / "qfirst-scala"

  object freelog extends Module {
    object js extends JsModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"com.lihaoyi::fansi::$fansiVersion"
      )
    }
    object jvm extends FullJvmModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.http4s::http4s-blaze-client::$http4sVersion",
        ivy"org.http4s::http4s-blaze-server::$http4sVersion",
        ivy"com.lihaoyi::fansi::$fansiVersion"
      )
      object test extends Tests {
        override def millSourcePath = freelog.this.millSourcePath / "test"
        override def scalaVersion = jvm.this.scalaVersion
        // def platformSegment = jvm.this.platformSegment
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

  object metrics extends Module {
    object js extends JsModule
    object jvm extends JvmModule
  }

  object clause extends Module {
    object js extends JsModule
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(metrics.jvm)
    }
  }

  object `clause-align` extends Module {
    object js extends FullJsModule {
      def moduleDeps = Seq(clause.js)
    }
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(clause.jvm, metrics.jvm)
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"com.github.tototoshi::scala-csv:$scalaCsvVersion"
      )
    }
  }

  object `clause-ext` extends Module {
    object js extends JsModule
    object jvm extends JvmModule {
      object test extends Tests {
        override def millSourcePath = `clause-ext`.this.millSourcePath / "test"
        override def scalaVersion = jvm.this.scalaVersion
        // def platformSegment = jvm.this.platformSegment
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

  // object `clause-ext-ann` extends Module {
  //   object js extends FullJsModule {
  //     def moduleDeps = Seq(`clause-ext`.js)
  //   }
  //   object jvm extends FullJvmModule {

  //     def moduleDeps = Seq(`clause-ext`.jvm)

  //     def serve(args: String*) = T.command {
  //       val jsPath = `clause-ext-ann`.js.fastOpt().path.toString
  //       val jsDepsPath = `clause-ext-ann`.js.aggregatedJSDeps().path.toString
  //       val runMain = runMainFn()
  //       runMain(
  //         "qfirst.clause.ext.ann.Serve",
  //         List(
  //           "--jsDeps", jsDepsPath,
  //           "--js", jsPath
  //         ) ++ args)
  //     }
  //   }
  // }

  object `clause-ext-demo` extends Module {
    object js extends FullJsModule {

      def moduleDeps = Seq(`clause-ext`.js, `model-eval`.js)

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::spacro::$spacroVersion",
        ivy"org.julianmichael::qasrl-crowd::$qasrlVersion"
      )
    }
    object jvm extends FullJvmModule {

      def moduleDeps = Seq(`clause-ext`.jvm, `model-eval`.jvm)

      override def resources = T.sources(
        millSourcePath / "resources",
        `clause-ext-demo`.js.fastOpt().path / RelPath.up,
        `clause-ext-demo`.js.aggregatedJSDeps().path / RelPath.up
      )

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::spacro::$spacroVersion",
        ivy"org.julianmichael::qasrl-crowd::$qasrlVersion",
        ivy"org.julianmichael::jjm-corenlp::$jjmVersion",
        ivy"com.lihaoyi::ammonite-ops::$ammoniteOpsVersion",
        // not sure if any of these are needed
        // ivy"com.github.japgolly.scalacss::ext-scalatags:$scalacssVersion",
        // ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
        // ivy"org.slf4j:slf4j-api:$slf4jApiVersion" // decided to match scala-logging transitive dep
      )
    }
  }

  object frame extends Module {
    object js extends JsModule {
      def moduleDeps = Seq(clause.js, metrics.js, `model-eval`.js, freelog.js)
    }
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(clause.jvm, metrics.jvm, `model-eval`.jvm, freelog.jvm)

      override def repositories = super.repositories ++ Seq(
        coursier.MavenRepository("https://dl.bintray.com/cibotech/public")
      )

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.scalanlp::breeze:$breezeVersion",
        ivy"org.scalanlp::breeze-natives:$breezeVersion",
        ivy"com.cibo::evilplot-repl:$evilplotVersion"
      )

      // def runTopics(args: String*) = T.command {
      //   val runMain = runMainFn()
      //   runMain("qfirst.topics.TopicModelingApp", args)
      // }
    }
  }

  object `frame-ann` extends Module {
    object js extends FullJsModule {
      def moduleDeps = Seq(frame.js, `clause-ext`.js)
    }
    object jvm extends FullJvmModule with ScalatexModule {

      override def repositories = super.repositories ++ Seq(
        coursier.MavenRepository("https://dl.bintray.com/cibotech/public")
      )

      def moduleDeps = Seq(frame.jvm, `clause-ext`.jvm)

      def serve(args: String*) = T.command {
        val jsPath = `frame-ann`.js.fastOpt().path.toString
        val jsDepsPath = `frame-ann`.js.aggregatedJSDeps().path.toString
        val runMain = runMainFn()
        runMain(
          "qfirst.frame.ann.Serve",
          List(
            "--jsDeps", jsDepsPath,
            "--js", jsPath
          ) ++ args)
      }
    }
  }

  object `frame-browse` extends Module {
    object js extends FullJsModule {
      def moduleDeps = Seq(frame.js)
    }
    object jvm extends FullJvmModule with ScalatexModule {

      def moduleDeps = Seq(frame.jvm)

      override def repositories = super.repositories ++ Seq(
        coursier.MavenRepository("https://dl.bintray.com/cibotech/public")
      )

      def serve(args: String*) = T.command {
        val jsPath = `frame-browse`.js.fastOpt().path.toString
        val jsDepsPath = `frame-browse`.js.aggregatedJSDeps().path.toString
        val runMain = runMainFn()
        runMain(
          "qfirst.frame.browse.Serve",
          List(
            "--jsDeps", jsDepsPath,
            "--js", jsPath
          ) ++ args)
      }
    }
  }

  object `model-gen` extends Module {
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(metrics.jvm)
    }
  }

  object `model-eval` extends Module {
    object js extends JsModule {
      def moduleDeps = Seq(metrics.js, `clause-ext`.js)
    }
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(metrics.jvm, `clause-ext`.js)
    }
  }

  object ontonotes extends Module {
    object js extends JsModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"com.lihaoyi::fastparse::$fastparseVersion"
      )
    }
    object jvm extends JvmModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"com.softwaremill.macmemo::macros::$macmemoVersion",
        ivy"com.lihaoyi::fastparse::$fastparseVersion"
      )
      object test extends Tests {
        def moduleDeps = super.moduleDeps ++ Seq(freelog.jvm)
        override def millSourcePath = ontonotes.this.millSourcePath / "test"
        override def scalaVersion = jvm.this.scalaVersion
        // def platformSegment = jvm.this.platformSegment
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

  object reprocess extends Module {
    object jvm extends FullJvmModule
  }
}
