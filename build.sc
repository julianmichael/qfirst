import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository

import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

import ammonite.ops._

val thisScalaVersion = "2.12.13"
val thisScalaJSVersion = "1.4.0"

// locally published lib
val evilplotVersion = "0.8.1-SNAPSHOT"

// my libs
val jjmVersion = "0.2.0-SNAPSHOT"
val qasrlVersion = "0.3.0-SNAPSHOT"
val qasrlBankVersion = "0.4.0-SNAPSHOT"
val radhocVersion = "0.4.0-SNAPSHOT"
val spacroVersion = "0.4.0-SNAPSHOT"
val freelogVersion = "0.1.0-SNAPSHOT"

// compiler plugins
val macroParadiseVersion = "2.1.1"
val kindProjectorVersion = "0.11.3"
val splainVersion = "0.3.4"
val betterMonadicForVersion = "0.3.1"

// cats libs
val http4sVersion = "0.21.18"
val kittensVersion = "2.2.1"
val declineVersion = "1.3.0"

// non-cats
val breezeVersion = "0.13.2"
val fansiVersion = "0.2.10"
val scalaCsvVersion = "1.3.6"
val fastparseVersion = "2.3.1"
val macmemoVersion = "0.4"

// jvm webby libs
val scalatagsVersion = "0.9.3"

// jvm libs
val ammoniteOpsVersion = "1.1.2"
val logbackVersion = "1.2.3"

// js libs
val scalajsDomVersion = "1.1.0"
val scalajsJqueryVersion = "1.0.0"
// val scalajsReactVersion = "1.2.3"
// val scalajsReactVersion = "1.3.1"
val scalacssVersion = "0.7.0"

// test libs
val scalatestVersion = "3.0.8"
val scalacheckVersion = "1.14.0"
val disciplineVersion = "1.0.0"

import $file.`scripts-build`.SimpleJSDepsBuild, SimpleJSDepsBuild.SimpleJSDeps

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
    ivy"org.typelevel:::kind-projector:$kindProjectorVersion",
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

val jlineVersion = "3.19.0"

object qfirst extends Module {

  override def millSourcePath = build.millSourcePath / "qfirst-scala"

  object clause extends Module {
    object js extends JsModule
    object jvm extends FullJvmModule
  }

  object `clause-align` extends Module {
    object js extends FullJsModule {
      def moduleDeps = Seq(clause.js)
    }
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(clause.jvm)
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"com.github.tototoshi::scala-csv:$scalaCsvVersion",
        ivy"org.julianmichael::qasrl-bank::0.3.0-SNAPSHOT",
        ivy"org.julianmichael::qasrl-bank-service::0.3.0-SNAPSHOT",
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
      def moduleDeps = Seq(clause.js, `model-eval`.js, datasets.js)

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::freelog::$freelogVersion",
        ivy"com.cibo::evilplot::$evilplotVersion"
      )
    }
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(clause.jvm, `model-eval`.jvm, datasets.jvm)

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::freelog::$freelogVersion",
        ivy"com.cibo::evilplot::$evilplotVersion",
        ivy"org.scalanlp::breeze:$breezeVersion",
        ivy"org.scalanlp::breeze-natives:$breezeVersion",
        ivy"com.lihaoyi::scalatags:$scalatagsVersion",
        ivy"com.github.tototoshi::scala-csv:$scalaCsvVersion"
      )
    }
  }

  object `frame-ann` extends Module {
    object js extends FullJsModule {
      def moduleDeps = Seq(frame.js, `clause-ext`.js)
    }
    object jvm extends FullJvmModule {

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
    object jvm extends FullJvmModule {

      def moduleDeps = Seq(frame.jvm)

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
    object jvm extends FullJvmModule
  }
  object `model-eval` extends Module {
    object js extends JsModule {
      def moduleDeps = Seq(`clause-ext`.js)
    }
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(`clause-ext`.js)
    }
  }

  object datasets extends Module {
    object js extends JsModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"com.lihaoyi::fastparse::$fastparseVersion"
      )
    }
    object jvm extends JvmModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        // TODO remove macmemo
        ivy"com.softwaremill.macmemo::macros::$macmemoVersion",
        ivy"com.lihaoyi::fastparse::$fastparseVersion"
      )
      object test extends Tests {
        def moduleDeps = super.moduleDeps
        override def millSourcePath = datasets.this.millSourcePath / "test"
        override def scalaVersion = jvm.this.scalaVersion
        // def platformSegment = jvm.this.platformSegment
        override def ivyDeps = Agg(
          ivy"org.julianmichael::freelog::$freelogVersion",
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
