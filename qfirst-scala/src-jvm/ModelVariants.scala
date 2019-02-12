package qfirst

import cats.~>
import cats.Applicative
import cats.Id
import cats.Monad
import cats.Monoid
import cats.data.State
import cats.data.StateT
import cats.implicits._

import io.circe.Encoder
import io.circe.ACursor
import io.circe.Json

import scala.util.Random

object ModelVariants {

  case class Hyperparams[F[_]: Monad](
    tokenHandler: TokenHandler[F],
    feedForwardNumLayers: F[Int],
    feedForwardHiddenDims: F[Int],
    feedForwardActivations: F[String],
    feedForwardDropout: F[Double],
    questionEncoderSlotEmbeddingDim: F[Int],
    questionEncoderNumLayers: F[Int],
    questionEncoderOutputDim: F[Int],
    questionGeneratorSlotHiddenDim: F[Int],
    questionGeneratorRNNHiddenDim: F[Int],
    questionGeneratorSlotEmbeddingDim: F[Int],
    questionGeneratorNumLayers: F[Int],
    spanSelectorHiddenDim: F[Int],
    includeSpanFFNN: F[Boolean],
    predicateFeatureDim: F[Int],
    sentenceEncoderNumLayers: F[Int],
    sentenceEncoderHiddenDimOpt: F[Option[Int]],
    textEmbeddingDropout: F[Double],
    // test/prod varying params
    trainPath: F[String],
    devPath: F[String],
    numEpochs: F[Int],
    cudaDevice: F[Int]
  ) extends Monad[F] {
    def pure[A](a: A): F[A] = Monad[F].pure(a)
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = Monad[F].tailRecM(a)(f)
    def mapK[G[_]: Monad](f: F ~> G) = Hyperparams[G](
      tokenHandler = tokenHandler.mapK(f),
      feedForwardNumLayers = f(feedForwardNumLayers),
      feedForwardHiddenDims = f(feedForwardHiddenDims),
      feedForwardActivations = f(feedForwardActivations),
      feedForwardDropout = f(feedForwardDropout),
      questionEncoderSlotEmbeddingDim = f(questionEncoderSlotEmbeddingDim),
      questionEncoderNumLayers = f(questionEncoderNumLayers),
      questionEncoderOutputDim = f(questionEncoderOutputDim),
      questionGeneratorSlotHiddenDim = f(questionGeneratorSlotHiddenDim),
      questionGeneratorRNNHiddenDim = f(questionGeneratorRNNHiddenDim),
      questionGeneratorSlotEmbeddingDim = f(questionGeneratorSlotEmbeddingDim),
      questionGeneratorNumLayers = f(questionGeneratorNumLayers),
      spanSelectorHiddenDim = f(spanSelectorHiddenDim),
      includeSpanFFNN = f(includeSpanFFNN),
      predicateFeatureDim = f(predicateFeatureDim),
      sentenceEncoderNumLayers = f(sentenceEncoderNumLayers),
      sentenceEncoderHiddenDimOpt = f(sentenceEncoderHiddenDimOpt),
      textEmbeddingDropout = f(textEmbeddingDropout),
      trainPath = f(trainPath),
      devPath = f(devPath),
      numEpochs = f(numEpochs),
      cudaDevice = f(cudaDevice),
    )
  }
  object Hyperparams {
    val test = Hyperparams[Id](
      tokenHandler = TokenHandler.glove[Id],
      feedForwardNumLayers = 2,
      feedForwardHiddenDims = 100,
      feedForwardActivations = "relu",
      feedForwardDropout = 0.0,
      questionEncoderSlotEmbeddingDim = 100,
      questionEncoderNumLayers = 2,
      questionEncoderOutputDim = 100,
      questionGeneratorSlotHiddenDim = 100,
      questionGeneratorSlotEmbeddingDim = 100,
      questionGeneratorRNNHiddenDim = 200,
      questionGeneratorNumLayers = 2,
      spanSelectorHiddenDim = 100,
      includeSpanFFNN = false,
      predicateFeatureDim = 0,
      sentenceEncoderNumLayers = 2,
      sentenceEncoderHiddenDimOpt = None,
      textEmbeddingDropout = 0.0,
      trainPath = "dev-mini.jsonl",
      devPath = "dev-mini.jsonl",
      numEpochs = 1,
      cudaDevice = -1
    )
    val elmoList = Hyperparams[List](
      tokenHandler = TokenHandler.elmo[List],
      feedForwardNumLayers = List(1, 2, 4),
      feedForwardHiddenDims = List(100, 300, 500),
      feedForwardActivations = List("relu"),
      feedForwardDropout = List(0.0, 0.1),
      questionEncoderSlotEmbeddingDim = List(100),
      questionEncoderNumLayers = List(2, 4),
      questionEncoderOutputDim = List(100),
      questionGeneratorSlotHiddenDim = List(100),
      questionGeneratorRNNHiddenDim = List(200),
      questionGeneratorSlotEmbeddingDim = List(200),
      questionGeneratorNumLayers = List(4, 8),
      includeSpanFFNN = List(true),
      spanSelectorHiddenDim = List(100),
      predicateFeatureDim = List(100),
      sentenceEncoderNumLayers = List(4, 8),
      sentenceEncoderHiddenDimOpt = List(Some(300), Some(600)),
      textEmbeddingDropout = List(0.0, 0.1),

      trainPath = List("qasrl-v2_1/expanded/train.jsonl"),
      devPath = List("qasrl-v2_1/expanded.dev.jsonl"),
      numEpochs = List(200),
      cudaDevice = List(0)
    )

    val bertList = elmoList.copy[List](
      tokenHandler = TokenHandler.bert[List],
      sentenceEncoderHiddenDimOpt = List(None)
    )

    private val nextRand = (l: Long) => {
      l * 6364136223846793005L + 1442695040888963407L
    }
    val randomSample = new (List ~> State[Long, ?]) {
      def apply[A](xs: List[A]) = for {
        _ <- State.modify(nextRand)
        l <- State.get[Long]
      } yield xs(((l % xs.length).toInt + xs.length) % xs.length)
    }
  }

  import io.circe.generic.auto._
  import io.circe.syntax._

  type Param[F[_], A] = StateT[F, ACursor, A]

  trait Component[A] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]): StateT[F, ACursor, A]
    def generate[F[_]](implicit H: Hyperparams[F]): F[(Json, A)] =
      genConfigs(H).run(Json.obj().hcursor).map {
        case (c, a) => (c.focus.get, a)
      }
    def generateJson[F[_]](implicit H: Hyperparams[F]) = generate.map(_._1)
  }

  // implicit param not put into json
  def param[F[_]: Applicative, A](value: F[A]) = StateT.liftF[F, ACursor, A](value)
  // explicit param put into json
  def param[F[_]: Monad, A: Encoder](
    name: String, value: F[A]
  ): StateT[F, ACursor, A] = {
    StateT.liftF[F, ACursor, A](value).flatMap { v =>
      StateT.modify[F, ACursor](c =>
        c.withFocus(json =>
          Json.fromJsonObject(json.asObject.get.add(name, v.asJson))
        )
      ).as(v)
    }
  }
  def param_[F[_]: Monad, A: Encoder](name: String, value: F[A]) = param(name, value).as(())
  // explicit param from component
  def param[F[_]: Hyperparams, A](name: String, component: Component[A]): StateT[F, ACursor, A] = {
    StateT.liftF[F, ACursor, (Json, A)](component.generate).flatMap {
      case (newField, retValue) =>
        StateT.modify[F, ACursor](c =>
          c.withFocus(json =>
            Json.fromJsonObject(json.asObject.get.add(name, newField))
          )
        ).as(retValue)
    }
  }
  // def nest[F[_]: Monad, A](name: String, param: Param[F, A]): F[A] = param.runA(Json.obj().hcursor)
  def nest[F[_]: Monad, A](name: String, param: Param[F, A]): StateT[F, ACursor, A] = {
    StateT.liftF[F, ACursor, (ACursor, A)](param.run(Json.obj().hcursor)).flatMap { case (newJsonC, result) =>
      StateT.modify[F, ACursor](c =>
        c.withFocus(json =>
          Json.fromJsonObject(json.asObject.get.add(name, newJsonC.focus.get))
        )
      ).as(result)
    }
  }

  // AllenNLP stuff
  case class TokenHandler[F[_]: Monad](indexers: List[Param[F, Unit]], embedders: List[Param[F, Int]]) {
    def mapK[G[_]: Monad](f: F ~> G) = TokenHandler[G](
      indexers.map(_.mapK(f)),
      embedders.map(_.mapK(f)),
    )
    def getIndexers = indexers.sequence.as(())
    def getEmbedders = embedders.sequence.map(_.combineAll)
  }
  object TokenHandler {
    implicit def tokenHandlerMonoid[F[_]: Monad]: Monoid[TokenHandler[F]] = new Monoid[TokenHandler[F]] {
      def empty = TokenHandler[F](Nil, Nil)
      def combine(x: TokenHandler[F], y: TokenHandler[F]) = TokenHandler[F](
        x.indexers ++ y.indexers,
        x.embedders ++ y.embedders)
    }

    val gloveIndexerJson = Json.obj(
      "type" -> "single_id".asJson,
      "lowercase_tokens" -> true.asJson
    )
    val gloveEmbedderJson = Json.obj(
      "tokens" -> Json.obj(
        "type" -> "embedding".asJson,
        "pretrained_file" -> "https://s3-us-west-2.amazonaws.com/allennlp/datasets/glove/glove.6B.100d.txt.gz".asJson
      )
    )
    def glove[F[_]: Monad] = TokenHandler(
      indexers = List(param_("tokens", Monad[F].pure(gloveIndexerJson))),
      embedders = List(param("tokens", Monad[F].pure(gloveEmbedderJson)).as(100))
    )

    val elmoIndexerJson = Json.obj("type" -> "elmo_characters".asJson)
    val elmoEmbedderJson = Json.obj(
      "type" -> "elmo_token_embedder".asJson,
      "options_file" -> "https://s3-us-west-2.amazonaws.com/allennlp/models/elmo/2x4096_512_2048cnn_2xhighway/elmo_2x4096_512_2048cnn_2xhighway_options.json".asJson,
      "weight_file" -> "https://s3-us-west-2.amazonaws.com/allennlp/models/elmo/2x4096_512_2048cnn_2xhighway/elmo_2x4096_512_2048cnn_2xhighway_weights.hdf5".asJson,
      "do_layer_norm" -> false.asJson,
      "dropout" -> (0.5).asJson
    )
    def elmo[F[_]: Monad] = TokenHandler(
      indexers = List(param_("elmo", Monad[F].pure(elmoIndexerJson))),
      embedders = List(param("elmo", Monad[F].pure(elmoEmbedderJson)).as(1024))
    )

    val bertIndexerJson = Json.obj(
      "type" -> "bert-pretrained".asJson,
      "do_lowercase" -> true.asJson,
      "pretrained_model" -> "bert-base-uncased".asJson,
      "use_starting_offsets" -> true.asJson
    )
    val bertTokenCharactersJson = Json.obj(
      "type" -> "characters".asJson,
      "min_padding_length" -> 3.asJson
    )
    val bertEmbedderToIndexerMapJson = Json.obj(
      "bert" -> Json.arr("bert".asJson, "bert-offsets".asJson),
      "token_characters" -> Json.arr("token_characters".asJson)
    )
    val bertEmbedderJson = Json.obj(
      "bert" -> Json.obj {
        "type" -> "bert-pretrained".asJson
        "pretrained_model" -> "bert-base-uncased".asJson
      }
    )
    def bert[F[_]: Monad] = TokenHandler(
      indexers = List(
        param_("bert", Monad[F].pure(bertIndexerJson)),
        param_("token_characters", Monad[F].pure(bertTokenCharactersJson))
      ),
      embedders = List(
        for {
          _ <- param("allow_unmatched_keys", Monad[F].pure(true))
          _ <- param("embedder_to_indexer_map", Monad[F].pure(bertEmbedderToIndexerMapJson))
          _ <- param("token_embedders", Monad[F].pure(bertEmbedderJson))
        } yield 768
      )
    )
  }

  case class AllenNLPIterator(batchSize: Int) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("bucket"))
      _ <- param("sorting_keys", H.pure(List(List("text", "num_tokens"))))
      _ <- param("batch_size", H.pure(batchSize))
    } yield ()
  }

  case class Trainer(validationMetric: String) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("num_epochs", H.numEpochs)
      _ <- param("grad_norm", H.pure(1.0))
      _ <- param("patience", H.pure(5))
      _ <- param("validation_metric", H.pure(validationMetric))
      _ <- param("cuda_device", H.cudaDevice)
      _ <- param("optimizer", H.pure(Json.obj("type" -> "adadelta".asJson, "rho" -> (0.95).asJson)))
    } yield ()
  }

  case class StackedEncoder(inputDim: Int, hiddenSize: Int) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("alternating_lstm"))
      _ <- param("use_highway", H.pure(true))
      _ <- param("recurrent_dropout_probability", H.pure(0.1))
      _ <- param("input_size", H.pure(inputDim))
      _ <- param("hidden_size", H.pure(hiddenSize))
      _ <- param("num_layers", H.sentenceEncoderNumLayers)
    } yield ()
  }

  case class FeedForward(inputDim: Int) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("num_layers", H.feedForwardNumLayers)
      _ <- param("hidden_dims", H.feedForwardHiddenDims)
      _ <- param("activations", H.feedForwardActivations)
      _ <- param("dropout", H.feedForwardDropout)
    } yield ()
  }

  // My modules

  case class QuestionEncoder(
    slotNames: List[String], inputDim: Int, outputDim: Int
  ) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("slot_names", H.pure(slotNames))
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("slot_embedding_dim", H.questionEncoderSlotEmbeddingDim)
      _ <- param("output_dim", H.pure(outputDim))
      _ <- param("num_layers", H.questionEncoderNumLayers)
    } yield ()
  }

  case class SentenceEncoder() extends Component[Int] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = param(H.sentenceEncoderHiddenDimOpt).flatMap {
      case Some(outputDim) => for {
        tokenDim <- nest("text_field_embedder", H.tokenHandler.getEmbedders)
        _ <- param("embedding_dropout", H.textEmbeddingDropout)
        predicateFeatureDim <- param("predicate_feature_dim", H.predicateFeatureDim)
        _ <- param("stacked_encoder", StackedEncoder(tokenDim + predicateFeatureDim, outputDim))
      } yield outputDim
      case None => nest("text_field_embedder", H.tokenHandler.getEmbedders)
    }
  }

  case class QuestionGenerator(slotNames: List[String], inputDim: Int) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("slot_names", H.pure(slotNames))
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("slot_hidden_dim", H.questionGeneratorSlotHiddenDim)
      _ <- param("rnn_hidden_dim", H.questionGeneratorRNNHiddenDim)
      _ <- param("slot_embedding_dim", H.questionGeneratorSlotEmbeddingDim)
      _ <- param("num_layers", H.questionGeneratorNumLayers)
    } yield ()
  }

  case class SpanSelector(
    inputDim: Int, extraInputDim: Option[Int],
  ) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("extra_input_dim", H.pure(extraInputDim.getOrElse(0)))
      spanHiddenDim <- param("span_hidden_dim", H.spanSelectorHiddenDim)
      includeSpanFFNN <- param(H.includeSpanFFNN)
      _ <- (if(includeSpanFFNN) param("span_ffnn", FeedForward(spanHiddenDim)) else param(H.unit))
      _ <- param("pruning_ratio", H.pure(2.0))
      _ <- param("objective", H.pure("binary"))
      _ <- param("gold_span_selection_policy", H.pure("union"))
    } yield ()
  }

  case class QasrlFilter(
    minAnswers: Int, minValidAnswers: Int
  ) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("min_answers", H.pure(minAnswers))
      _ <- param("min_valid_answers", H.pure(minValidAnswers))
    } yield ()
  }
  object QasrlFilter {
    def validQuestions = QasrlFilter(3, 3)
    def questionsWithAnswers = QasrlFilter(1, 1)
    def allQuestions = QasrlFilter(1, 0)
  }

  case class QasrlInstanceReader(instanceType: String) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure(instanceType))
    } yield ()
  }

  case class DatasetReader(
    filter: QasrlFilter,
    instanceReader: QasrlInstanceReader
  ) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("qfirst_qasrl"))
      _ <- nest("token_indexers", H.tokenHandler.getIndexers)
      _ <- param("qasrl_filter", filter)
      _ <- param("instance_reader", instanceReader)
    } yield ()
  }

  case class Model(
    datasetReader: DatasetReader,
    model: Component[Unit],
    validationMetric: String
  ) extends Component[Unit] {
    val batchSize: Int = 256
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("dataset_reader", datasetReader)
      _ <- param("train_data_path", H.trainPath)
      _ <- param("validation_data_path", H.devPath)
      _ <- param("model", model)
      _ <- param("iterator", AllenNLPIterator(batchSize))
      _ <- param("trainer", Trainer(validationMetric))
    } yield ()
  }
  object Model {
    def question(slotNames: List[String]) = Model(
      datasetReader = DatasetReader(QasrlFilter.validQuestions, QasrlInstanceReader("question")),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qfirst_question_generator"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("question_generator", QuestionGenerator(slotNames, encoderOutputDim))
        } yield ()
      },
      validationMetric = "-perplexity-per-question"
    )

    def questionToSpan(
      slotNames: List[String],
      classifyInvalids: Boolean,
      ) = Model(
      datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("question")),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qfirst_question_answerer"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          questionEncodingDim <- param(H.questionEncoderOutputDim)
          _ <- param("question_injection", H.pure("top"))
          _ <- param("question_encoder", QuestionEncoder(slotNames, encoderOutputDim, questionEncodingDim))
          _ <- param("span_selector", SpanSelector(encoderOutputDim, Some(questionEncodingDim)))
          _ <- param("classify_invalids", H.pure(classifyInvalids))
        } yield ()
      },
      validationMetric = "+f1"
    )

    val span = Model(
      datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("verb_answers")),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("afirst_span_detector"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("span_selector", SpanSelector(encoderOutputDim, None))
        } yield ()
      },
      validationMetric = "+f1"
    )

    def spanToQuestion(slotNames: List[String]) = Model(
      datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("verb_answers")),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("afirst_question_generator"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("question_generator", QuestionGenerator(slotNames, 2 * encoderOutputDim))
        } yield ()
      },
      validationMetric = "+f1"
    )
  }

  val fullSlots = List("wh", "aux", "subj", "verb", "obj", "prep", "obj2")
  val clauseSlots = List("clause-subj", "clause-aux", "clause-verb", "clause-obj", "clause-prep1", "clause-prep1-obj", "clause-prep2", "clause-prep2-obj", "clause-misc", "clause-qarg")

  val elmoModels = MapTree.fork(
    "span" -> MapTree.leaf[String](Model.span),
    "span_to_question" -> MapTree.leaf[String](Model.spanToQuestion(fullSlots)),
    "question" -> MapTree.fromPairs(
      "full" -> Model.question(fullSlots)
    ),
    "question_to_span" -> MapTree.fromPairs(
      "full_-invalid" -> Model.questionToSpan(fullSlots, false),
      "full" -> Model.questionToSpan(fullSlots, true)
    )
  )

  val bertModels = MapTree.fork(
    "span" -> MapTree.leaf[String](Model.span),
    "span_to_question" -> MapTree.leaf[String](Model.spanToQuestion(fullSlots)),
    "question" -> MapTree.fromPairs(
      "full" -> Model.question(fullSlots),
      "clausal" -> Model.question(clauseSlots),
      // TODO: no-tan, no-animacy, no-tan & no-animacy, all clausal
    ),
    "question_to_span" -> MapTree.fromPairs(
      "full" -> Model.questionToSpan(fullSlots, true),
      "clausal" -> Model.questionToSpan(clauseSlots, true),
      // TODO: no-tan, no-animacy, no-tan & no-animacy, all clausal
    )
  )

  val testModels = elmoModels.merge(bertModels, (x, y) => y)

  import cats.effect.IO
  import java.nio.file.Paths
  import java.nio.file.Path
  val printer = io.circe.Printer.spaces2

  def modelsWithPaths(models: MapTree[String, Model], root: Path) = models.branches.map {
    case (segments, model) =>
      val path = segments.foldLeft(root)(_ resolve _)
      path -> model
  }

  def writeTest(path: Path, model: Model) = {
    FileUtil.writeJson(path.resolve("test/config.json"), printer)(model.generateJson(Hyperparams.test))
  }
  def writeAll(path: Path, model: Model, hyperparams: Hyperparams[List]) = {
    val jsons = model.generateJson(hyperparams)
    jsons.zipWithIndex.traverse { case (json, index) =>
      FileUtil.writeJson(path.resolve(s"grid/$index.json"), printer)(json)
    }

    (new util.Random()).shuffle(jsons).zipWithIndex.traverse {
      case (json, index) =>
        FileUtil.writeJson(path.resolve(s"shuffle/$index.json"), printer)(json)
    }
  }

  def generateAll(rootStr: String): IO[Unit] = {
    val root = Paths.get(rootStr)
    for {
      _ <- modelsWithPaths(testModels, root.resolve("test")).traverse {
        case (path, model) => writeTest(path, model)
      }
      _ <- modelsWithPaths(elmoModels, root.resolve("elmo")).traverse {
        case (path, model) => writeAll(path, model, Hyperparams.elmoList)
      }
      _ <- modelsWithPaths(bertModels, root.resolve("bert")).traverse {
        case (path, model) => writeAll(path, model, Hyperparams.bertList)
      }
    } yield ()
  }
}
