import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import mill.api.DummyInputStream
import mill.eval.Result
import coursier.maven.MavenRepository

import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

import ammonite.ops._

val thisScalaVersion = "2.12.12"
val thisScalaJSVersion = "1.4.0"

// locally published lib
val evilplotVersion = "0.8.1-SNAPSHOT"

// my libs
val jjmVersion = "0.2.1-SNAPSHOT"
val qasrlVersion = "0.3.1-SNAPSHOT"
// val qasrlBankVersion = "0.3.0"
// val spacroVersion = "0.4.0"
val freelogVersion = "0.1.0"


val catsCollectionsVersion = "0.9.1"
val mouseVersion = "0.26.2"

// compiler plugins
val macroParadiseVersion = "2.1.1"
val kindProjectorVersion = "0.11.3"
val splainVersion = "0.3.4"
val betterMonadicForVersion = "0.3.1"

// cats libs
val kittensVersion = "2.2.1"
val declineVersion = "1.3.0"

// non-cats
val breezeVersion = "0.13.2"
val scalaCsvVersion = "1.3.6"
val fastparseVersion = "2.3.1"

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

val munitVersion = "0.7.21"
val munitCatsEffectVersion = "0.11.0"

import $file.`scripts-build`.SimpleJSDepsBuild, SimpleJSDepsBuild.SimpleJSDeps

trait CommonModule extends ScalaModule with ScalafmtModule {

  def platformSegment: String

  override def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

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
    ivy"org.typelevel::mouse::$mouseVersion",
    ivy"org.typelevel::kittens::$kittensVersion",
    ivy"org.typelevel::cats-collections-core::$catsCollectionsVersion",
    // ivy"org.typelevel::alleycats-core::$catsVersion",
    ivy"org.julianmichael::qasrl::$qasrlVersion",
    ivy"org.julianmichael::qasrl-bank::$qasrlVersion",
    ivy"org.julianmichael::qasrl-bank-service::$qasrlVersion",
    ivy"com.github.japgolly.scalacss::core::$scalacssVersion"
  )
}

trait CommonMainModule extends CommonModule {
  def scalaVersion = thisScalaVersion
  def millSourcePath = super.millSourcePath / RelPath.up

  trait CommonTestModule extends CommonModule with TestModule {
    override def ivyDeps = Agg(
      ivy"org.scalameta::munit::$munitVersion",
      ivy"org.typelevel::munit-cats-effect-2::$munitCatsEffectVersion",
    )
    def testFrameworks = Seq("munit.Framework")
  }
}

trait JvmModule extends CommonMainModule {
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

  trait Tests extends super.Tests with CommonTestModule {
    def platformSegment = "jvm"
  }
}

trait FullJvmModule extends JvmModule {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.monovore::decline::$declineVersion",
    ivy"com.monovore::decline-effect::$declineVersion",
    ivy"ch.qos.logback:logback-classic:$logbackVersion"
  )
}

trait JsModule extends CommonMainModule with ScalaJSModule {
  def scalaJSVersion = T(thisScalaJSVersion)
  def platformSegment = "js"
  trait Tests extends super.Tests with CommonTestModule {
    def scalaJSVersion = T(thisScalaJSVersion)
    def platformSegment = "js"
    def moduleKind = T(mill.scalajslib.api.ModuleKind.CommonJSModule)
  }
}

trait FullJsModule extends JsModule with SimpleJSDeps {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.julianmichael::jjm-ui::$jjmVersion",
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

object qfirst extends Module {

  override def millSourcePath = build.millSourcePath / "qfirst-scala"

  object parsing extends Module {
    object js extends JsModule
    object jvm extends JvmModule {
      object test extends Tests
    }
  }

  object cafe extends Module {
    object js extends JsModule {
      def moduleDeps = Seq(parsing.js)
    }
    object jvm extends JvmModule {
      def moduleDeps = Seq(parsing.jvm)
      object test extends Tests
    }
  }

  object `cafe-browse` extends Module {

    def serve(args: String*) = T.command {
      if (T.ctx().log.inStream == DummyInputStream){
        Result.Failure("server needs to be run with the -i/--interactive flag")
      } else {
        val runMain = jvm.runMainFn()
        runMain(
          "qfirst.cafe.browse.Serve", Seq(
            "--js",        js.fastOpt().path.toString,
            "--jsDeps",    js.aggregatedJSDeps().path.toString
          ) ++ args
        )
      }
    }

    object jvm extends FullJvmModule {
      def moduleDeps = Seq(cafe.jvm)
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"com.lihaoyi::scalatags:$scalatagsVersion"
      )
      object test extends Tests
    }
    object js extends FullJsModule {
      def moduleDeps = Seq(cafe.js)
      def mainClass = T(Some("qfirst.cafe.browse.Main"))
    }
  }

  object `clause-ext` extends Module {
    object js extends JsModule
    object jvm extends JvmModule
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

  // object `clause-ext-demo` extends Module {
  //   object js extends FullJsModule {

  //     def moduleDeps = Seq(`clause-ext`.js, `model-eval`.js)

  //     override def ivyDeps = super.ivyDeps() ++ Agg(
  //       ivy"org.julianmichael::spacro::$spacroVersion",
  //       ivy"org.julianmichael::qasrl-crowd::$qasrlVersion"
  //     )
  //   }
  //   object jvm extends FullJvmModule {

  //     def moduleDeps = Seq(`clause-ext`.jvm, `model-eval`.jvm)

  //     override def resources = T.sources(
  //       millSourcePath / "resources",
  //       `clause-ext-demo`.js.fastOpt().path / RelPath.up,
  //       `clause-ext-demo`.js.aggregatedJSDeps().path / RelPath.up
  //     )

  //     override def ivyDeps = super.ivyDeps() ++ Agg(
  //       ivy"org.julianmichael::spacro::$spacroVersion",
  //       ivy"org.julianmichael::qasrl-crowd::$qasrlVersion",
  //       ivy"org.julianmichael::jjm-corenlp::$jjmVersion",
  //       ivy"com.lihaoyi::ammonite-ops::$ammoniteOpsVersion",
  //       // not sure if any of these are needed
  //       // ivy"com.github.japgolly.scalacss::ext-scalatags:$scalacssVersion",
  //       // ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
  //       // ivy"org.slf4j:slf4j-api:$slf4jApiVersion" // decided to match scala-logging transitive dep
  //     )
  //   }
  // }

  object clustering extends Module {
    object js extends JsModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::freelog::$freelogVersion"
      )
    }
    object jvm extends FullJvmModule {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::freelog::$freelogVersion",
        ivy"org.scalanlp::breeze:$breezeVersion",
        ivy"org.scalanlp::breeze-natives:$breezeVersion"
      )
    }
  }

  object frame extends Module {
    object js extends JsModule {
      def moduleDeps = Seq(`model-eval`.js, clustering.js)

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::jjm-datasets::$jjmVersion",
        ivy"org.julianmichael::freelog::$freelogVersion",
        ivy"com.cibo::evilplot::$evilplotVersion"
      )
    }
    object jvm extends FullJvmModule {
      def moduleDeps = Seq(`model-eval`.jvm, clustering.jvm)

      override def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.julianmichael::jjm-datasets::$jjmVersion",
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
    object jvm extends FullJvmModule { def moduleDeps = Seq(frame.jvm, `clause-ext`.jvm) }
    object js extends FullJsModule { def moduleDeps = Seq(frame.js, `clause-ext`.js) }

    def serve(args: String*) = T.command {
      val jsPath = js.fastOpt().path.toString
      val jsDepsPath = js.aggregatedJSDeps().path.toString
      val runMain = jvm.runMainFn()
      runMain(
        "qfirst.frame.ann.Serve",
        List(
          "--jsDeps", jsDepsPath,
          "--js", jsPath
        ) ++ args)
    }
  }

  object `frame-browse` extends Module {
    object jvm extends FullJvmModule { def moduleDeps = Seq(frame.jvm) }
    object js extends FullJsModule { def moduleDeps = Seq(frame.js) }

    def serve(args: String*) = T.command {
      val jsPath = js.fastOpt().path.toString
      val jsDepsPath = js.aggregatedJSDeps().path.toString
      val runMain = jvm.runMainFn()
      runMain(
        "qfirst.frame.browse.Serve",
        List(
          "--jsDeps", jsDepsPath,
          "--js", jsPath
        ) ++ args)
    }
  }

  object `model-gen` extends Module {
    object jvm extends FullJvmModule
  }
  object `model-eval` extends Module {
    object jvm extends FullJvmModule { def moduleDeps = Seq(`clause-ext`.js) }
    object js extends JsModule { def moduleDeps = Seq(`clause-ext`.js) }
  }
}
