package qfirst

import cats.~>
import cats.Applicative
import cats.Id
import cats.Monad
import cats.Monoid
import cats.data.State
import cats.data.StateT
import cats.implicits._

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.ExitCode

import com.monovore.decline.Command
import com.monovore.decline.Opts

import io.circe.Encoder
import io.circe.ACursor
import io.circe.Json

import scala.util.Random

object ModelVariants extends IOApp {

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
    spanUncertaintyFactor: F[Double],
    predicateFeatureDim: F[Int],
    sentenceEncoderNumLayers: F[Int],
    sentenceEncoderHiddenDimOpt: F[Option[Int]],
    textEmbeddingDropout: F[Double],
    injectPredicate: F[Boolean],
    // optimization params
    useBertAdam: F[Boolean],
    baseAdamLR: F[Double],
    useBertWarmupSchedule: F[Boolean],
    bertWeightsAdamLR: F[Double],
    bertWeightsWarmupNumSteps: F[Int],
    // test/prod varying params
    trainPath: F[String],
    devPath: F[String],
    numEpochs: F[Int],
    cudaDevice: F[Int],
    // batch size varies with expected num parameters, but we'll be scaling down until it fits in memory
    maxBatchSize: F[Int],
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
      spanUncertaintyFactor = f(spanUncertaintyFactor),
      predicateFeatureDim = f(predicateFeatureDim),
      sentenceEncoderNumLayers = f(sentenceEncoderNumLayers),
      sentenceEncoderHiddenDimOpt = f(sentenceEncoderHiddenDimOpt),
      textEmbeddingDropout = f(textEmbeddingDropout),
      injectPredicate = f(injectPredicate),
      useBertAdam = f(useBertAdam),
      baseAdamLR = f(baseAdamLR),
      useBertWarmupSchedule = f(useBertWarmupSchedule),
      bertWeightsAdamLR = f(bertWeightsAdamLR),
      bertWeightsWarmupNumSteps = f(bertWeightsWarmupNumSteps),
      trainPath = f(trainPath),
      devPath = f(devPath),
      numEpochs = f(numEpochs),
      cudaDevice = f(cudaDevice),
      maxBatchSize = f(maxBatchSize),
    )
  }
  object Hyperparams {
    val test = Hyperparams[Id](
      tokenHandler = TokenHandler.glove[Id](usePretrained = false),
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
      spanUncertaintyFactor = 2.0,
      predicateFeatureDim = 0,
      sentenceEncoderNumLayers = 2,
      sentenceEncoderHiddenDimOpt = None,
      textEmbeddingDropout = 0.0,
      injectPredicate = false,
      // optimization
      useBertAdam = false,
      baseAdamLR = 0.00005,
      useBertWarmupSchedule = false,
      bertWeightsAdamLR = 0.00002,
      bertWeightsWarmupNumSteps = 10000,
      // test/prod
      trainPath = "dev-mini.jsonl",
      devPath = "dev-mini.jsonl",
      numEpochs = 1,
      cudaDevice = -1,
      maxBatchSize = 256,
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
      questionGeneratorNumLayers = List(2, 4),
      includeSpanFFNN = List(true),
      spanUncertaintyFactor = List(1.1, 1.5, 2.0, 4.0),
      spanSelectorHiddenDim = List(50, 100, 200),
      predicateFeatureDim = List(100),
      sentenceEncoderNumLayers = List(4, 8),
      sentenceEncoderHiddenDimOpt = List(Some(300), Some(600)),
      textEmbeddingDropout = List(0.0),
      injectPredicate = List(false),
      // optimization
      useBertAdam = List(false),
      baseAdamLR = List(0.00005),
      useBertWarmupSchedule = List(false),
      bertWeightsAdamLR = List(0.00002),
      bertWeightsWarmupNumSteps = List(10000),
      // test/prod
      trainPath = List("qasrl-v2_1/expanded/train.jsonl.gz"),
      devPath = List("qasrl-v2_1/expanded/dev.jsonl.gz"),
      numEpochs = List(30),
      cudaDevice = List(0),
      maxBatchSize = List(256),
    )

    val bertListFeature = elmoList.copy[List](
      tokenHandler = TokenHandler.bert[List](finetune = false),
      maxBatchSize = List(256),
      injectPredicate = List(true, false),
    )
    val bertListFinetune = bertListFeature.copy[List](
      tokenHandler = TokenHandler.bert[List](finetune = true),
      sentenceEncoderHiddenDimOpt = List(None),
      feedForwardNumLayers = List(1, 2),
      includeSpanFFNN = List(true, false),
      injectPredicate = List(true),
      useBertAdam = List(true),
      baseAdamLR = List(5e-5, 2e-4),
      bertWeightsAdamLR = List(1e-5, 2e-5, 3e-5),
      useBertWarmupSchedule = List(true),
      bertWeightsWarmupNumSteps = List(100, 1000, 10000),
      numEpochs = List(10),
      maxBatchSize = List(32),
    )
    val bertListShallowFinetune = bertListFinetune.copy[List](
      tokenHandler = TokenHandler.bert[List](finetune = true),
      useBertAdam = List(true),
      bertWeightsAdamLR = List(2e-5, 3e-5, 5e-5),
      useBertWarmupSchedule = List(false),
      numEpochs = List(10),
      maxBatchSize = List(32))

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
    def gloveEmbedderJson(usePretrained: Boolean) = {
      if(usePretrained) {
        Json.obj(
          "type" -> "embedding".asJson,
          "pretrained_file" -> "https://s3-us-west-2.amazonaws.com/allennlp/datasets/glove/glove.6B.100d.txt.gz".asJson,
          "embedding_dim" -> 100.asJson
        )
      } else {
        Json.obj(
          "type" -> "embedding".asJson,
          "embedding_dim" -> 100.asJson
        )
      }
    }
    def glove[F[_]: Monad](usePretrained: Boolean) = TokenHandler(
      indexers = List(param_("tokens", Monad[F].pure(gloveIndexerJson))),
      embedders = List(param("tokens", Monad[F].pure(gloveEmbedderJson(usePretrained))).as(100))
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
    def bertEmbedderJson(finetune: Boolean) = Json.obj(
      "bert" -> Json.obj(
        "type" -> "bert-pretrained".asJson,
        "pretrained_model" -> "bert-base-uncased".asJson,
        "requires_grad" -> finetune.asJson,
        "top_layer_only" -> finetune.asJson
      )
    )
    def bertIndexers[F[_]: Monad] = List(
      param_("bert", Monad[F].pure(bertIndexerJson)),
      param_("token_characters", Monad[F].pure(bertTokenCharactersJson))
    )
    def bert[F[_]: Monad](finetune: Boolean) = TokenHandler(
      indexers = bertIndexers[F],
      embedders = List(
        for {
          _ <- param("allow_unmatched_keys", Monad[F].pure(true))
          _ <- param("embedder_to_indexer_map", Monad[F].pure(bertEmbedderToIndexerMapJson))
          _ <- param("token_embedders", Monad[F].pure(bertEmbedderJson(finetune)))
        } yield 768
      )
    )
  }

  case class AllenNLPIterator() extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("bucket"))
      _ <- param("sorting_keys", H.pure(List(List("text", "num_tokens"))))
      _ <- param("batch_size", H.maxBatchSize)
    } yield ()
  }

  case class AdadeltaOptimizer() extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("adadelta"))
      _ <- param("rho", H.pure(0.95))
    } yield ()
  }

  case class BertAdamOptimizer() extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure("bert_adam"))
      useWarmup <- param(H.useBertWarmupSchedule)
      _ <- {
        if(useWarmup) {
          for {
            _ <- param("lr", H.baseAdamLR)
            warmupTotal <- param(H.bertWeightsWarmupNumSteps)
            bertLR <- param(H.bertWeightsAdamLR)
            _ <- param(
              "parameter_groups", H.pure(
                List(List("bert_model") -> Json.obj(
                       "lr" -> bertLR.asJson,
                       "t_total" -> warmupTotal.asJson,
                       "schedule" -> "warmup_constant".asJson
                     ))))
          } yield ()
        } else param("lr", H.bertWeightsAdamLR)
      }
    } yield ()
  }

  case class Trainer(validationMetric: String) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("num_epochs", H.numEpochs)
      _ <- param("grad_norm", H.pure(1.0))
      _ <- param("patience", H.pure(2))
      _ <- param("validation_metric", H.pure(validationMetric))
      _ <- param("cuda_device", H.cudaDevice)
      useBertAdam <- param(H.useBertAdam)
      _ <- param("optimizer", if(useBertAdam) BertAdamOptimizer() else AdadeltaOptimizer())
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
    inputDim: Int, extraInputDim: Option[Int], doDensityEstimation: Boolean = false
  ) extends Component[Unit] {
    val objective = if(doDensityEstimation) "density_mle" else "binary"
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("input_dim", H.pure(inputDim))
      _ <- param("extra_input_dim", H.pure(extraInputDim.getOrElse(0)))
      spanHiddenDim <- param("span_hidden_dim", H.spanSelectorHiddenDim)
      includeSpanFFNN <- param(H.includeSpanFFNN)
      _ <- (if(includeSpanFFNN) param("span_ffnn", FeedForward(spanHiddenDim)) else param(H.unit))
      _ <- param("objective", H.pure(objective))
      _ <- (if(doDensityEstimation) param("uncertainty_factor", H.spanUncertaintyFactor) else param(H.unit))
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

  case class QasrlInstanceReader(
    instanceType: String,
    slotNames: Option[List[String]] = None,
    clauseInfoFile: Option[String] = None
  ) extends Component[Unit] {
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("type", H.pure(instanceType))
      _ <- slotNames.fold(param(H.unit))(s => param("slot_names", H.pure(s)).as(()))
      _ <- clauseInfoFile.fold(param(H.unit))(s => param("clause_info_files", H.pure(List(s))).as(()))
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
    def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
      _ <- param("dataset_reader", datasetReader)
      _ <- param("train_data_path", H.trainPath)
      _ <- param("validation_data_path", H.devPath)
      _ <- param("model", model)
      _ <- param("iterator", AllenNLPIterator())
      _ <- param("trainer", Trainer(validationMetric))
    } yield ()
  }
  object Model {
    def question(slotNames: List[String], includeClauseData: Boolean = false) = Model(
      datasetReader = DatasetReader(
        QasrlFilter.validQuestions,
        QasrlInstanceReader("question", slotNames = Some(slotNames), clauseInfoFile = Some("clause-data-train-dev.jsonl").filter(_ => includeClauseData))
      ),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_question"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("question_generator", QuestionGenerator(slotNames, encoderOutputDim))
        } yield ()
      },
      validationMetric = "-perplexity-per-question"
    )

    def questionToSpan(
      slotNames: List[String],
      classifyInvalids: Boolean,
      includeClauseData: Boolean = false
    ) = Model(
      datasetReader = DatasetReader(
        QasrlFilter.allQuestions,
        QasrlInstanceReader("question", slotNames = Some(slotNames), clauseInfoFile = Some("clause-data-train-dev.jsonl").filter(_ => includeClauseData))
      ),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_question_to_span"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          questionEncodingDim <- param(H.questionEncoderOutputDim)
          _ <- param("question_encoder", QuestionEncoder(slotNames, encoderOutputDim, questionEncodingDim))
          _ <- param("span_selector", SpanSelector(encoderOutputDim, Some(questionEncodingDim + encoderOutputDim)))
          _ <- param("classify_invalids", H.pure(classifyInvalids))
        } yield ()
      },
      validationMetric = if(classifyInvalids) "+span-f1" else "+f1"
    )

    def span(doDensityEstimation: Boolean) = Model(
      datasetReader = DatasetReader(QasrlFilter.validQuestions, QasrlInstanceReader("verb_answers")),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_span"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          injectPredicate <- param("inject_predicate", H.injectPredicate)
          extraInputDimOpt = if(injectPredicate) Some(encoderOutputDim) else None
          _ <- param("span_selector", SpanSelector(encoderOutputDim, extraInputDimOpt, doDensityEstimation))
        } yield ()
      },
      validationMetric = "+f1"
    )

    def spanToQuestion(slotNames: List[String]) = Model(
      datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("verb_qas")),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_span_to_question"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          injectPredicate <- param("inject_predicate", H.injectPredicate)
          questionGeneratorInputDim = (if(injectPredicate) 3 else 2) * encoderOutputDim
          _ <- param("question_generator", QuestionGenerator(slotNames, questionGeneratorInputDim))
        } yield ()
      },
      validationMetric = "-perplexity-per-question"
    )

    def questionToSpanBert(validQuestionsOnly: Boolean) = Model(
      datasetReader = DatasetReader(
        if(validQuestionsOnly) QasrlFilter.validQuestions else QasrlFilter.allQuestions,
        QasrlInstanceReader("question_with_sentence_single_span")),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_question_to_span_bert"))
          _ <- nest("text_field_embedder", H.tokenHandler.getEmbedders)
        } yield ()
      },
      validationMetric = "+f1"
    )

    val animacy = Model(
      datasetReader = DatasetReader(
        QasrlFilter.questionsWithAnswers,
        QasrlInstanceReader("span_animacy")
      ),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_animacy"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("animacy_ffnn", FeedForward(encoderOutputDim))
          _ <- param("inject_predicate", H.injectPredicate)
        } yield ()
      },
      validationMetric = "+f1"
    )

    val tan = Model(
      datasetReader = DatasetReader(
        QasrlFilter.validQuestions,
        QasrlInstanceReader("question_factored")
      ),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_multiclass"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("label_name", H.pure("tan_set"))
          _ <- param("label_namespace", H.pure("tan-string-labels"))
        } yield ()
      },
      validationMetric = "+f1"
    )

    val spanToTan = Model(
      datasetReader = DatasetReader(
        QasrlFilter.questionsWithAnswers,
        QasrlInstanceReader("span_tan")
      ),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_span_to_tan"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("tan_ffnn", FeedForward(encoderOutputDim))
          _ <- param("inject_predicate", H.injectPredicate)
        } yield ()
      },
      validationMetric = "+f1"
    )

    val clauseString = Model(
      datasetReader = DatasetReader(
        QasrlFilter.validQuestions,
        QasrlInstanceReader("question_factored", clauseInfoFile = Some("clause-data-train-dev.jsonl"))
      ),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_multiclass"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("label_name", H.pure("clause_set"))
          _ <- param("label_namespace", H.pure("abst-clause-labels"))
        } yield ()
      },
      validationMetric = "+f1"
    )

    val answerSlot = Model(
      datasetReader = DatasetReader(
        QasrlFilter.questionsWithAnswers,
        QasrlInstanceReader("question_factored", clauseInfoFile = Some("clause-data-train-dev.jsonl"))
      ),
      model = new Component[Unit] {
        def genConfigs[F[_]](implicit H: Hyperparams[F]) = for {
          _ <- param("type", H.pure("qasrl_clause_and_span_to_answer_slot"))
          encoderOutputDim <- param("sentence_encoder", SentenceEncoder())
          _ <- param("qarg_ffnn", FeedForward(encoderOutputDim))
        } yield ()
      },
      validationMetric = "+f1"
    )
  }

  val fullSlots = List("wh", "aux", "subj", "verb", "obj", "prep", "obj2")
  val noTanSlots = List("wh", "subj", "abst-verb", "obj", "prep", "obj2")
  val clauseSlots = List("clause-subj", "clause-aux", "clause-verb", "clause-obj", "clause-prep1", "clause-prep1-obj", "clause-prep2", "clause-prep2-obj", "clause-misc", "clause-qarg")
  val clauseNoAnimSlots = List("clause-abst-subj", "clause-aux", "clause-verb", "clause-abst-obj", "clause-prep1", "clause-abst-prep1-obj", "clause-prep2", "clause-abst-prep2-obj", "clause-abst-misc", "clause-qarg")
  val clauseNoTanSlots = List("clause-subj", "clause-abst-verb", "clause-obj", "clause-prep1", "clause-prep1-obj", "clause-prep2", "clause-prep2-obj", "clause-misc", "clause-qarg")
  val clauseNoTanOrAnimSlots = List("clause-abst-subj", "clause-abst-verb", "clause-abst-obj", "clause-prep1", "clause-abst-prep1-obj", "clause-prep2", "clause-abst-prep2-obj", "clause-abst-misc", "clause-qarg")

  val models = MapTree.fork(
    "span" -> MapTree.fromPairs(
      "binary" -> Model.span(doDensityEstimation = false),
      "density" -> Model.span(doDensityEstimation = true)
    ),
    "span_to_question" -> MapTree.leaf[String](Model.spanToQuestion(fullSlots)),
    "question" -> MapTree.fromPairs(
      "full" -> Model.question(fullSlots),
      "no_tan" -> Model.question(noTanSlots),
      "clausal" -> Model.question(clauseSlots, includeClauseData = true),
      "clausal_no_tan" -> Model.question(clauseNoTanSlots, includeClauseData = true),
      "clausal_no_anim" -> Model.question(clauseNoAnimSlots, includeClauseData = true),
      "clausal_no_tan_or_anim" -> Model.question(clauseNoTanOrAnimSlots, includeClauseData = true),
    ),
    "question_to_span" -> MapTree.fromPairs(
      "full" -> Model.questionToSpan(fullSlots, true),
      "no_tan" -> Model.questionToSpan(noTanSlots, true),
      "clausal" -> Model.questionToSpan(clauseSlots, true, includeClauseData = true),
      "clausal_no_tan" -> Model.questionToSpan(clauseNoTanSlots, true, includeClauseData = true),
      "clausal_no_anim" -> Model.questionToSpan(clauseNoAnimSlots, true, includeClauseData = true),
      "clausal_no_tan_or_anim" -> Model.questionToSpan(clauseNoTanOrAnimSlots, true, includeClauseData = true),
    ),
    "animacy" -> MapTree.leaf[String](Model.animacy),
    "tan" -> MapTree.leaf[String](Model.tan),
    "span_to_tan" -> MapTree.leaf[String](Model.spanToTan),
    "clause_string" -> MapTree.leaf[String](Model.clauseString),
    "answer_slot" -> MapTree.leaf[String](Model.answerSlot)
  )

  val bertSpecializedModels = MapTree.fork(
    "question_to_span" -> MapTree.fromPairs(
      "full-bert-specialized-valid" -> Model.questionToSpanBert(validQuestionsOnly = true),
      "full-bert-specialized" -> Model.questionToSpanBert(validQuestionsOnly = false),
    )
  )

  val testModels = models
    .merge(bertSpecializedModels, (x, y) => y)

  import java.nio.file.Paths
  import java.nio.file.Path
  val printer = io.circe.Printer.spaces2

  def modelsWithPaths(models: MapTree[String, Model], root: Path) = models.branches.map {
    case (segments, model) =>
      val path = segments.foldLeft(root)(_ resolve _)
      path -> model
  }

  def writeTest(path: Path, model: Model) = {
    FileUtil.writeJson(path.resolve("config.json"), printer)(model.generateJson(Hyperparams.test))
  }
  def writeAll(path: Path, model: Model, hyperparams: Hyperparams[List]) = {
    val jsons = model.generateJson(hyperparams)
    for {
      _ <- (new util.Random()).shuffle(jsons).take(12).zipWithIndex.traverse {
        case (json, index) =>
          FileUtil.writeJson(path.resolve(s"$index.json"), printer)(json)
      }
    } yield ()
  }

  def generateAll(root: Path): IO[ExitCode] = for {
    _ <- modelsWithPaths(testModels, root.resolve("test")).traverse {
      case (path, model) => writeTest(path, model)
    }
    _ <- modelsWithPaths(models, root.resolve("elmo")).traverse {
      case (path, model) => writeAll(path, model, Hyperparams.elmoList)
    }
    _ <- modelsWithPaths(models, root.resolve("bert-feature")).traverse {
      case (path, model) => writeAll(path, model, Hyperparams.bertListFeature)
    }
    _ <- modelsWithPaths(models, root.resolve("bert-finetune")).traverse {
      case (path, model) => writeAll(path, model, Hyperparams.bertListFinetune)
    }
    _ <- modelsWithPaths(bertSpecializedModels, root.resolve("bert-specialized")).traverse {
      case (path, model) => writeAll(path, model, Hyperparams.bertListShallowFinetune)
    }
  } yield ExitCode.Success

  val command = Command(
    name = "mill qfirst.jvm.runHyperparams",
    header = "Generate hyperparam configurations."
  ) {
    val path = Opts.option[Path](
      "path", metavar = "path", help = "Path to create the directory to place all of the generated config files."
    )
    (path).map(generateAll)
  }

  def run(args: List[String]): IO[ExitCode] = {
    command.parse(args) match {
      case Left(help) => IO { System.err.println(help); ExitCode.Error }
      case Right(main) => main
    }
  }

}
