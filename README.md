# qfirst

Question-first modeling for QA-SRL.

## Evaluation script

Evaluating QA-SRL Models has proven a bit tricky. To make that easier I am
working on a standard evaluation methodology so you can just run a script and
get some numbers and a bit of automated error analysis.

Right now, it assumes you are evaluating against the QA-SRL Bank data, but I'll
make it flexible enough to use with your own data.


### Requirements

You need the [mill](https://github.com/lihaoyi/mill) build tool, version 0.2.5
or greater.

### Input format

Currently it supports one input format, which contains all of the model's
predictions and their probabilities (before filtering based on probabilities or
answer overlap). In the future it will also support an already-filtered format
(similar to the QA-SRL Bank data distribution format) in case your model doesn't
fit into the decoding framework used here.

The format is as a JSON Lines file where each line corresponds to all
predictions for a sentence. You can see where the JSON objects are constructed
in Python [here](qfirst/metrics/prediction_saving_metric.py), or where they are
translated to/from JSON in Scala
[here](qfirst-scala/src-jvm/qfirst/Predictions.scala).

### Running

Currently only the dense eval works out-of-the-box.
To run it, first decide on a working directory for your model predictions,
say `predictions/<model-name>`. Place your model's predictions on `dense/dev`
at `predictions/<model-name>/predictions-dense.jsonl`.
Then you need to decide on how you'll filter the predictions. The evaluation
script will automatically do grid search over a specified space of filtering
parameters to find the setting under which the model produces the best results.

#### Specifying the space of filters

Create the file `predictions/<model-name>/dense/filters.json`. Its contents
should be a JSON object defined as follows:
```json
{
  "threeThreshold": {
    "questionThresholds": List[Float],
    "spanThresholds": List[Float],
    "invalidThresholds": List[Float]
  },
  "twoThreshold": {
    "qaThresholds": List[Float],
    "invalidThresholds": List[Float]
  },
  "oneThreshold": {
    "thresholds": List[Float]
  }
}
```
where all the top-level fields are optional, but at least one needs to be
present. Within each category of filter type (one, two, or three threshold),
the script will try all combinations of thresholds. It will identify the one
that has highest performance (by QA accuracy lower bound) with at least 2
questions per verb on average.
It will then overwrite the `filters.json` file adding a new `best` field
indicating this filter so you can find it and so subsequent runs will not have
to re-do the search. You can also specify your own `best` filter if you have a
specific one to try.

#### Executing the script

Then run 
```
mill qfirst.jvm.runMetrics --gold <path/to/qasrl-bank/qasrl-v2> --pred predictions/<model-name> --mode dense
```
to do the dense eval using the specified space of filters.

### Examining the code

The code for evaluation can be found
[here](qfirst-scala/src-jvm/qfirst/MetricsApp.scala).
This is the entry point and overall logic for organizing how the metrics are
computed. To see the specifics of the computations of accuracy lower and upper
bounds, precision/recall/f1, confusion matrices, etc., they are
[here](qfirst-scala/src-jvm/qfirst/Instances.scala).
All metrics computations are organized around the notion of an _instance_,
which has statistics calculated on it which are aggregated across all instances
before final reporting. There are many ways of splitting out instances from each
other for the purposes of computing accuracy on questions, answers,
question-answer pairs, etc., so this framework makes things a little more
manageable.
