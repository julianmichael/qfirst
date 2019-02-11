package qfirst

import cats.Applicative
import cats.Id
import cats.Monad
import cats.Monoid
import cats.data.StateT
import cats.implicits._

import io.circe.Encoder
import io.circe.ACursor
import io.circe.Json

object ModelVariantsGen {

  case class Hyperparams[F[_]: Monad](
    tokenHandler: TokenHandler[F],
    feedForwardNumLayers: F[Int],
    feedForwardHiddenDims: F[Int],
    feedForwardActivations: F[String],
    feedForwardDropout: F[Double],
    questionEncoderSlotEmbeddingDim: F[Int],
    questionEncoderNumLayers: F[Int],
    questionEncoderOutputDim: F[Int],
    sentenceEncoderNumLayers: F[Int],
    questionGeneratorSlotHiddenDim: F[Int],
    questionGeneratorRNNHiddenDim: F[Int],
    questionGeneratorSlotEmbeddingDim: F[Int],
    questionGeneratorNumLayers: F[Int],
    spanSelectorHiddenDim: F[Int],
    predicateFeatureDim: F[Int],
    sentenceEncoderHiddenDimOpt: F[Option[Int]],
    embeddingDropout: F[Double],
    // test/prod varying params
    trainPath: F[String],
    devPath: F[String],
    numEpochs: F[Int],
    cudaDevice: F[Int]
  ) extends Monad[F] {
    def pure[A](a: A): F[A] = Monad[F].pure(a)
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
    def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = Monad[F].tailRecM(a)(f)
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
      sentenceEncoderNumLayers = 2,
      questionGeneratorSlotHiddenDim = 100,
      questionGeneratorRNNHiddenDim = 200,
      questionGeneratorSlotEmbeddingDim = 200,
      questionGeneratorNumLayers = 2,
      spanSelectorHiddenDim = 100,
      predicateFeatureDim = 100,
      sentenceEncoderHiddenDimOpt = None,
      embeddingDropout = 0.0,
      trainPath = "dev-mini.jsonl",
      devPath = "dev-mini.jsonl",
      numEpochs = 1,
      cudaDevice = -1
    )
    val elmoList = Hyperparams[List](
      tokenHandler = TokenHandler.elmo[List],
      feedForwardNumLayers = List(2, 4, 8),
      feedForwardHiddenDims = List(100, 300, 500),
      feedForwardActivations = List("relu"),
      feedForwardDropout = List(0.0),
      questionEncoderSlotEmbeddingDim = List(100, 200),
      questionEncoderNumLayers = List(2, 4, 8),
      questionEncoderOutputDim = List(100),
      sentenceEncoderNumLayers = List(2, 4, 8),
      questionGeneratorSlotHiddenDim = List(100),
      questionGeneratorRNNHiddenDim = List(200),
      questionGeneratorSlotEmbeddingDim = List(200),
      questionGeneratorNumLayers = List(2, 4, 8),
      spanSelectorHiddenDim = List(100),
      predicateFeatureDim = List(100),
      sentenceEncoderHiddenDimOpt = List(Some(300)),
      embeddingDropout = List(0.0, 0.1),

      trainPath = List("qasrl-v2_1/expanded/train.jsonl"),
      devPath = List("qasrl-v2_1/expanded.dev.jsonl"),
      numEpochs = List(200),
      cudaDevice = List(0)
    )
  }

  import io.circe.generic.auto._
  import io.circe.syntax._

  type Param[F[_], A] = StateT[F, ACursor, A]

  trait Component[F[_], A] {
    def genConfigs(implicit H: Hyperparams[F]): StateT[F, ACursor, A]
    def generate(implicit H: Hyperparams[F]): F[(Json, A)] =
      genConfigs(H).run(Json.obj().hcursor).map {
        case (c, a) => (c.focus.get, a)
      }
    def generateJson(implicit H: Hyperparams[F]) = generate.map(_._1)
  }
  object Component {
    def fromParam[F[_], A](param: StateT[F, ACursor, A]) = new Component[F, A] {
      def genConfigs(implicit H: Hyperparams[F]) = param
    }
    def fromParams[F[_], A: Monoid](params: StateT[F, ACursor, A]*) = new Component[F, A] {
      def genConfigs(implicit H: Hyperparams[F]) = params.toList.sequence.map(_.combineAll)
    }
    def fromParams_[F[_], A](params: StateT[F, ACursor, A]*) = new Component[F, Unit] {
      def genConfigs(implicit H: Hyperparams[F]) = params.toList.sequence.as(())
    }
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
  def param[F[_]: Hyperparams, A](name: String, component: Component[F, A]): StateT[F, ACursor, A] = {
    StateT.liftF[F, ACursor, (Json, A)](component.generate).flatMap {
      case (newField, retValue) =>
        StateT.modify[F, ACursor](c =>
          c.withFocus(json =>
            Json.fromJsonObject(json.asObject.get.add(name, newField))
          )
        ).as(retValue)
    }
  }

  // // AllenNLP stuff

  case class TokenHandler[F[_]](indexers: List[Param[F, Unit]], embedders: List[Param[F, Int]]) {
    def getIndexers: Component[F, Unit] = Component.fromParams_(indexers: _*)
    def getEmbedders: Component[F, Int] = Component.fromParams(embedders: _*)
  }
  object TokenHandler {
    implicit def tokenHandlerMonoid[F[_]]: Monoid[TokenHandler[F]] = new Monoid[TokenHandler[F]] {
      def empty = TokenHandler(Nil, Nil)
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
          // "trainable": false,
          // "embedding_dim": 100,
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

  case class AllenNLPIterator[F[_]](batchSize: Int) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("bucket"))
      _ <- param("sorting_keys", H.pure(List(List("text", "num_tokens"))))
      _ <- param("batch_size", H.pure(batchSize))
    } yield ()
  }

  case class Trainer[F[_]](validationMetric: String) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("num_epochs", H.numEpochs)
      _ <- param("grad_norm", H.pure(1.0))
      _ <- param("patience", H.pure(5))
      _ <- param("validation_metric", H.pure(validationMetric))
      _ <- param("cuda_device", H.cudaDevice)
      _ <- param("optimizer", H.pure(Json.obj("type" -> "adadelta".asJson, "rho" -> (0.95).asJson)))
    } yield ()
  }

  case class StackedEncoder[F[_]](inputDim: Int, hiddenSize: Int) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("alternating_lstm"))
      _ <- param("use_highway", H.pure(true))
      _ <- param("recurrent_dropout_probability", H.pure(0.1))
      _ <- param("input_size", H.pure(inputDim))
      _ <- param("hidden_size", H.pure(hiddenSize))
      _ <- param("num_layers", H.sentenceEncoderNumLayers)
    } yield ()
  }

  case class FeedForward[F[_]](inputDim: Int) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("num_layers", H.feedForwardNumLayers)
      _ <- param("hidden_dims", H.feedForwardHiddenDims)
      _ <- param("activations", H.feedForwardActivations)
      _ <- param("dropout", H.feedForwardDropout)
    } yield ()
  }

  // My modules

  case class QuestionEncoder[F[_]](
    slotNames: List[String], inputDim: Int, outputDim: Int
  ) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("slot_names", H.pure(slotNames))
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("slot_embedding_dim", H.questionEncoderSlotEmbeddingDim)
      _ <- param("output_dim", H.pure(outputDim))
      _ <- param("num_layers", H.questionEncoderNumLayers)
    } yield ()
  }

  case class SentenceEncoder[F[_]]() extends Component[F, Int] {
    def genConfigs(implicit H: Hyperparams[F]) = param(H.sentenceEncoderHiddenDimOpt).flatMap {
      case Some(outputDim) => for {
        tokenDim <- param("text_field_embedder", H.tokenHandler.getEmbedders)
        _ <- param("embedding_dropout", H.embeddingDropout)
        predicateFeatureDim <- param("predicate_feature_dim", H.predicateFeatureDim)
        _ <- param("stacked_encoder", StackedEncoder[F](tokenDim + predicateFeatureDim, outputDim))
      } yield outputDim
      case None => param("text_field_embedder", H.tokenHandler.getEmbedders)
    }
  }

  case class QuestionGenerator[F[_]](slotNames: List[String], inputDim: Int) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("slot_names", H.pure(slotNames))
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("slot_hidden_dim", H.questionGeneratorSlotHiddenDim)
      _ <- param("rnn_hidden_dim", H.questionGeneratorRNNHiddenDim)
      _ <- param("slot_embedding_dim", H.questionGeneratorSlotEmbeddingDim)
      _ <- param("num_layers", H.questionGeneratorNumLayers)
    } yield ()
  }

  case class SpanSelector[F[_]](
    inputDim: Int, extraInputDim: Option[Int],
  ) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("extra_input_dim", H.pure(extraInputDim.getOrElse(0)))
      spanHiddenDim <- param("span_hidden_dim", H.spanSelectorHiddenDim)
      _ <- param("span_ffnn", FeedForward[F](spanHiddenDim))
      _ <- param("pruning_ratio", H.pure(2.0))
      _ <- param("objective", H.pure("binary"))
      _ <- param("gold_span_selection_policy", H.pure("union"))
    } yield ()
  }

  case class QasrlFilter[F[_]](
    minAnswers: Int, minValidAnswers: Int
  ) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("min_answers", H.pure(minAnswers))
      _ <- param("min_valid_answers", H.pure(minValidAnswers))
    } yield ()
  }
  object QasrlFilter {
    def validQuestions[F[_]] = QasrlFilter[F](3, 3)
    def questionsWithAnswers[F[_]] = QasrlFilter[F](1, 1)
    def allQuestions[F[_]] = QasrlFilter[F](1, 0)
  }

  case class QasrlInstanceReader[F[_]](instanceType: String) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure(instanceType))
    } yield ()
  }

  case class DatasetReader[F[_]](
    filter: QasrlFilter[F],
    instanceReader: QasrlInstanceReader[F]
  ) extends Component[F, Unit] {
    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("qfirst_qasrl"))
      _ <- param("token_indexers", H.tokenHandler.getIndexers)
      _ <- param("qasrl_filter", filter)
      _ <- param("instance_reader", instanceReader)
    } yield ()
  }

  trait ModelVariant[F[_]] extends Component[F, Unit] {
    def datasetReader: DatasetReader[F]
    def model: Component[F, Unit]
    def validationMetric: String
    def batchSize: Int = 256

    def genConfigs(implicit H: Hyperparams[F]) = for {
      _ <- param("dataset_reader", datasetReader)
      _ <- param("train_data_path", H.trainPath)
      _ <- param("validation_data_path", H.devPath)
      _ <- param("model", model)
      _ <- param("iterator", AllenNLPIterator[F](batchSize))
      _ <- param("trainer", Trainer[F](validationMetric))
    } yield ()
  }

  case class PredicateQuestionGenerator[F[_]](slotNames: List[String]) extends ModelVariant[F] {
    val datasetReader = DatasetReader(QasrlFilter.validQuestions, QasrlInstanceReader("question"))
    val model = new Component[F, Unit] {
      def genConfigs(implicit H: Hyperparams[F]) = for {
        _ <- param("type", H.pure("qfirst_question_generator"))
        encoderOutputDim <- param("sentence_encoder", SentenceEncoder[F]())
        _ <- param("question_generator", QuestionGenerator[F](slotNames, encoderOutputDim))
      } yield ()
    }
    val validationMetric = "-perplexity-per-question"
  }

  case class QuestionAnswerer[F[_]](
    slotNames: List[String],
    classifyInvalids: Boolean,
  ) extends ModelVariant[F] {
    val datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("question"))
    val model = new Component[F, Unit] {
      def genConfigs(implicit H: Hyperparams[F]) = for {
        _ <- param("type", H.pure("qfirst_question_answerer"))
        encoderOutputDim <- param("sentence_encoder", SentenceEncoder[F]())
        questionEncodingDim <- param(H.questionEncoderOutputDim)
        _ <- param("question_injection", H.pure("top"))
        _ <- param("question_encoder", QuestionEncoder[F](slotNames, encoderOutputDim, questionEncodingDim))
        _ <- param("span_selector", SpanSelector[F](encoderOutputDim, Some(questionEncodingDim)))
        _ <- param("classify_invalids", H.pure(classifyInvalids))
      } yield ()
    }
    val validationMetric = "+f1"
  }

  case class SpanDetector[F[_]]() extends ModelVariant[F] {
    val datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("verb_answers"))
    val model = new Component[F, Unit] {
      def genConfigs(implicit H: Hyperparams[F]) = for {
        _ <- param("type", H.pure("afirst_span_detector"))
        encoderOutputDim <- param("sentence_encoder", SentenceEncoder[F]())
        _ <- param("span_selector", SpanSelector[F](encoderOutputDim, None))
      } yield ()
    }
    val validationMetric = "+f1"
  }

  case class SpanQuestionGenerator[F[_]](slotNames: List[String]) extends ModelVariant[F] {
    val datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("verb_answers"))
    val model = new Component[F, Unit] {
      def genConfigs(implicit H: Hyperparams[F]) = for {
        _ <- param("type", H.pure("afirst_question_generator"))
        encoderOutputDim <- param("sentence_encoder", SentenceEncoder[F])
        _ <- param("question_generator", QuestionGenerator[F](slotNames, 2 * encoderOutputDim))
      } yield ()
    }
    val validationMetric = "+f1"
  }

  // val fullSlots = List("wh", "aux", "subj", "verb", "obj", "prep", "obj2")

  // val variants = MapTree.fork(
  //   "span" -> MapTree.leaf(Span)
  //     MapTree.fromFork(
  //     "test" ->
  //   ),
  //   "afirst" -> MapTree.fromPairs(
  //     "qg" -> SpanQuestionGenerator(),
  //     "span-detection" -> SpanDetector()
  //   ),
  //   "qfirst" -> MapTree.fromPairs(
  //     "qg-full" -> PredicateQuestionGenerator(fullSlots),
  //     "qa-baseline" -> QuestionAnswerer(fullSlots, hasInvalidToken = false, useAllQuestions = false),
  //     "qa-allqs" -> QuestionAnswerer(fullSlots, hasInvalidToken = false, useAllQuestions = true),
  //     "qa-allqs-invalid" -> QuestionAnswerer(fullSlots, hasInvalidToken = true, useAllQuestions = true)
  //   )
  // )

  // import cats.effect.IO
  // import java.nio.file.Paths
  // import java.nio.file.Path
  // val printer = io.circe.Printer.spaces2

  // def generateAllConfigs(root: Path): IO[Unit] = {
  //   variants.branches.traverse { case (revDirectories, variant) =>
  //     val path = revDirectories.foldLeft(root)(_ resolve _)
  //     variant.generateJson.zipWithIndex.traverse { case (json, index) =>
  //       FileUtil.writeJson(path.resolve(s"$index.json"), printer)(json)
  //     }
  //   }
  // }.as(())
}
