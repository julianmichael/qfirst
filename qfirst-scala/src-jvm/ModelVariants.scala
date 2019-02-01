package qfirst

import cats.Monoid
import cats.data.StateT
import cats.implicits._

import io.circe.Encoder
import io.circe.ACursor
import io.circe.Json

object ModelVariants {

  import io.circe.generic.auto._
  import io.circe.syntax._

  trait Component[A] {
    def genConfigs: StateT[List, ACursor, A]
    def generate: List[(Json, A)] =
      genConfigs.run(Json.obj().hcursor).map {
        case (c, a) => (c.focus.get, a)
      }
    def generateJson = generate.map(_._1)
  }
  object Component {
    def fromParam[A](param: StateT[List, ACursor, A]) = new Component[A] {
      def genConfigs = param
    }
    def fromParams[A: Monoid](params: StateT[List, ACursor, A]*) = new Component[A] {
      def genConfigs = params.toList.sequence.map(_.combineAll)
    }
    def fromParams_[A](params: StateT[List, ACursor, A]*) = new Component[Unit] {
      def genConfigs = params.toList.sequence.as(())
    }
  }

  // implicit param not put into json
  def param[A](values: List[A]) = StateT.liftF[List, ACursor, A](values)
  // explicit param put into json
  def param[A: Encoder](
    name: String, values: List[A]
  ): StateT[List, ACursor, A] = {
    StateT.liftF[List, ACursor, A](values).flatMap { value =>
      StateT.modify[List, ACursor](c =>
        c.withFocus(json =>
          Json.fromJsonObject(json.asObject.get.add(name, value.asJson))
        )
      ).as(value)
    }
  }
  // explicit param from component
  def param[A](name: String, component: Component[A]): StateT[List, ACursor, A] = {
    StateT.liftF[List, ACursor, (Json, A)](component.generate).flatMap {
      case (newField, retValue) =>
        StateT.modify[List, ACursor](c =>
          c.withFocus(json =>
            Json.fromJsonObject(json.asObject.get.add(name, newField))
          )
        ).as(retValue)
    }
  }

  object Hyperparams {
    val questionEncoderSlotEmbeddingDim = List(100, 200)
    val questionEncoderNumLayers = List(2, 4, 8)
    val questionEncoderOutputDim = List(100)
    val sentenceEncoderNumLayers = List(2, 4, 8)
    val questionGeneratorSlotHiddenDim = List(100)
    val questionGeneratorRNNHiddenDim = List(200)
    val questionGeneratorNumLayers = List(2, 4, 8)
    val spanSelectorHiddenDim = List(100)
    val predicateFeatureDim = List(100)
    val sentenceEncoderHiddenDim = List(300)
  }
  val H = Hyperparams

  val elmoEmbedderJson = Json.obj(
    "type" -> "elmo_token_embedder".asJson,
    "options_file" -> "https://s3-us-west-2.amazonaws.com/allennlp/models/elmo/2x4096_512_2048cnn_2xhighway/elmo_2x4096_512_2048cnn_2xhighway_options.json".asJson,
    "weight_file" -> "https://s3-us-west-2.amazonaws.com/allennlp/models/elmo/2x4096_512_2048cnn_2xhighway/elmo_2x4096_512_2048cnn_2xhighway_weights.hdf5".asJson,
    "do_layer_norm" -> false.asJson,
    "dropout" -> (0.5).asJson
  )
  val gloveEmbedderJson = Json.obj(
      "tokens" -> Json.obj(
        "type" -> "embedding".asJson,
        "pretrained_file" -> "https://s3-us-west-2.amazonaws.com/allennlp/datasets/glove/glove.6B.100d.txt.gz".asJson
        // "trainable": false,
        // "embedding_dim": 100,
      )
  )
  val elmoEmbedder = param("elmo", List(elmoEmbedderJson)).as(1024)
  val gloveEmbedder = param("tokens", List(gloveEmbedderJson)).as(100)

  val elmoIndexerJson = Json.obj("type" -> "elmo_characters".asJson)
  val elmoIndexer = param("elmo", List(elmoIndexerJson))

  // TODO: will have BERT option for TextFieldEmbedder
  // returns token dimensionality

  case class AllenNLPIterator(batchSizes: List[Int]) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("type", List("bucket"))
      _ <- param("sorting_keys", List(List(List("text", "num_tokens"))))
      _ <- param("batch_size", batchSizes)
    } yield ()
  }

  case class Trainer(validationMetric: String) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("num_epochs", List(200))
      _ <- param("grad_norm", List(1.0))
      _ <- param("patience", List(10))
      _ <- param("validation_metric", List(validationMetric))
      _ <- param("cuda_device", List(0))
      _ <- param("optimizer", List(Json.obj("type" -> "adadelta".asJson, "rho" -> (0.95).asJson)))
    } yield ()
  }

  case class QuestionEncoder(
    slotNames: List[String], inputDim: Int, outputDim: Int
  ) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("slot_names", List(slotNames))
      _ <- param("input_dim", List(inputDim))
      _ <- param("slot_embedding_dim", H.questionEncoderSlotEmbeddingDim)
      _ <- param("output_dim", List(outputDim))
      _ <- param("layers", H.questionEncoderNumLayers)
    } yield ()
  }

  case class SentenceEncoder(inputDim: Int, hiddenSize: Int) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("type", List("alternating_lstm"))
      _ <- param("use_highway", List(true))
      _ <- param("recurrent_dropout_probability", List(0.1))
      _ <- param("input_size", List(inputDim))
      _ <- param("hidden_size", List(hiddenSize))
      _ <- param("num_layers", H.sentenceEncoderNumLayers)
    } yield ()
  }

  case class QuestionGenerator(slotNames: List[String], inputDim: Int) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("slot_names", List(slotNames))
      _ <- param("input_dim", List(inputDim))
      _ <- param("slot_hidden_dim", H.questionGeneratorSlotHiddenDim)
      _ <- param("rnn_hidden_dim", H.questionGeneratorRNNHiddenDim)
      _ <- param("layers", H.questionGeneratorNumLayers)
    } yield ()
  }

  case class SpanSelector(
    inputDim: Int, topInjectionDim: Option[Int], hasInvalidToken: Boolean = false
  ) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("input_dim", List(inputDim))
      _ <- param("span_hidden_dim", H.spanSelectorHiddenDim)
      _ <- param("top_injection_dim", List(topInjectionDim.getOrElse(0)))
      _ <- param("objective", List("binary")) // TODO could be variant param
      _ <- param("gold_span_selection_policy", List("union"))  // TODO could be variant param
      _ <- param("pruning_ratio", List(2.0))
      _ <- param("metric", List(Json.obj("has_invalid_token" -> hasInvalidToken.asJson)))
    } yield ()
  }

  case class QasrlFilter(
    minAnswers: Int, minValidAnswers: Int
  ) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("min_answers", List(minAnswers))
      _ <- param("min_valid_answers", List(minValidAnswers))
    } yield ()
  }
  object QasrlFilter {
    val validQuestions = QasrlFilter(3, 3)
    val questionsWithAnswers = QasrlFilter(1, 1)
    val allQuestions = QasrlFilter(1, 0)
  }

  case class QasrlInstanceReader(instanceType: String) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("type", List(instanceType))
    } yield ()
  }

  case class DatasetReader(
    filter: QasrlFilter,
    instanceReader: QasrlInstanceReader,
    prependInvalidToken: Boolean = false
  ) extends Component[Unit] {
    def genConfigs = for {
      _ <- param("type", List("qfirst_qasrl"))
      _ <- param("token_indexers", Component.fromParams_(elmoIndexer))
      _ <- param("qasrl_filter", filter)
      _ <- param("instance_reader", instanceReader)
      _ <- param("prepend_invalid_token", List(prependInvalidToken))
    } yield ()
  }

  trait ModelVariant extends Component[Unit] {
    def datasetReader: DatasetReader
    def model: Component[Unit]
    def validationMetric: String
    def batchSizes: List[Int]

    def genConfigs = for {
      _ <- param("dataset_reader", List(datasetReader))
      _ <- param("train_data_path", List("qasrl-v2_1/expanded/train.jsonl.gz"))
      _ <- param("validation_data_path", List("qasrl-v2_1/expanded/dev.jsonl.gz"))
      _ <- param("model", model)
      _ <- param("iterator", AllenNLPIterator(batchSizes))
      _ <- param("trainer", Trainer(validationMetric))
    } yield ()
  }

  case class PredicateQuestionGenerator(slotNames: List[String]) extends ModelVariant {
    val datasetReader = DatasetReader(QasrlFilter.validQuestions, QasrlInstanceReader("question"))
    val model = new Component[Unit] {
      def genConfigs = for {
        _ <- param("type", List("qfirst_question_generator"))
        tokenDim <- param("text_field_embedder", Component.fromParams(elmoEmbedder))
        predDim <- param("predicate_feature_dim", H.predicateFeatureDim)
        encoderOutputDim <- param(H.sentenceEncoderHiddenDim)
        _ <- param("sentence_encoder", SentenceEncoder(predDim + tokenDim, encoderOutputDim))
        _ <- param("question_generator", QuestionGenerator(slotNames, encoderOutputDim))
      } yield ()
    }
    val validationMetric = "-perplexity-per-question"
    val batchSizes = List(128)
  }

  case class QuestionAnswerer(
    slotNames: List[String],
    hasInvalidToken: Boolean,
    useAllQuestions: Boolean
  ) extends ModelVariant {
    val filter = if(useAllQuestions) QasrlFilter.allQuestions else QasrlFilter.questionsWithAnswers
    val datasetReader = DatasetReader(filter, QasrlInstanceReader("question"), hasInvalidToken)
    val model = new Component[Unit] {
      def genConfigs = for {
        _ <- param("type", List("qfirst_question_answerer"))
        tokenDim <- param("text_field_embedder", Component.fromParams(elmoEmbedder))
        predDim <- param("predicate_feature_dim", List(100))
        encoderOutputDim <- param(H.sentenceEncoderHiddenDim)
        questionEncodingDim <- param(H.questionEncoderOutputDim)
        _ <- param("question_injection", List("top"))
        _ <- param("sentence_encoder", SentenceEncoder(predDim + tokenDim, encoderOutputDim))
        _ <- param("question_encoder", QuestionEncoder(slotNames, encoderOutputDim, questionEncodingDim))
        _ <- param("span_selector", SpanSelector(encoderOutputDim, Some(questionEncodingDim), hasInvalidToken))
      } yield ()
    }
    val validationMetric = "+f1"
    val batchSizes = List(128)
  }

  case class SpanDetector() extends ModelVariant {
    val datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("verb_answers"))
    val model = new Component[Unit] {
      def genConfigs = for {
        _ <- param("type", List("afirst_span_detector"))
        tokenDim <- param("text_field_embedder", Component.fromParams(elmoEmbedder))
        predDim <- param("predicate_feature_dim", List(100))
        encoderOutputDim <- param(H.sentenceEncoderHiddenDim)
        _ <- param("sentence_encoder", SentenceEncoder(predDim + tokenDim, encoderOutputDim))
        _ <- param("span_selector", SpanSelector(encoderOutputDim, None))
      } yield ()
    }
    val validationMetric = "+f1"
    val batchSizes = List(128)
  }

  case class SpanQuestionGenerator() extends ModelVariant {
    val datasetReader = DatasetReader(QasrlFilter.allQuestions, QasrlInstanceReader("verb_answers"))
    val model = new Component[Unit] {
      def genConfigs = for {
        _ <- param("type", List("afirst_question_generator"))
        tokenDim <- param("text_field_embedder", Component.fromParams(elmoEmbedder))
        predDim <- param("predicate_feature_dim", List(100))
        encoderOutputDim <- param(H.sentenceEncoderHiddenDim)
        _ <- param("sentence_encoder", SentenceEncoder(predDim + tokenDim, encoderOutputDim))
        _ <- param("question_generator", QuestionGenerator(fullSlots, 2 * encoderOutputDim))
      } yield ()
    }
    val validationMetric = "+f1"
    val batchSizes = List(128)
  }

  val fullSlots = List("wh", "aux", "subj", "verb", "obj", "prep", "obj2")

  val variants = MapTree.fork(
    "afirst" -> MapTree.fromPairs(
      "qg" -> SpanQuestionGenerator(),
      "span-detection" -> SpanDetector()
    ),
    "qfirst" -> MapTree.fromPairs(
      "qg-full" -> PredicateQuestionGenerator(fullSlots),
      "qa-baseline" -> QuestionAnswerer(fullSlots, hasInvalidToken = false, useAllQuestions = false),
      "qa-allqs" -> QuestionAnswerer(fullSlots, hasInvalidToken = false, useAllQuestions = true),
      "qa-allqs-invalid" -> QuestionAnswerer(fullSlots, hasInvalidToken = true, useAllQuestions = true)
    )
  )

  import cats.effect.IO
  import java.nio.file.Paths
  import java.nio.file.Path
  val printer = io.circe.Printer.spaces2

  def generateAllConfigs(root: Path): IO[Unit] = {
    variants.branches.traverse { case (revDirectories, variant) =>
      val path = revDirectories.foldLeft(root)(_ resolve _)
      variant.generateJson.zipWithIndex.traverse { case (json, index) =>
        FileUtil.writeJson(path.resolve(s"$index.json"), printer)(json)
      }
    }
  }.as(())
}
