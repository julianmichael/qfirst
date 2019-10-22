# qfirst

Working repository for QA-SRL research.

## Contents

* `qfirst/`: Python directory with ML model code.
* `qfirst-scala/`: Scala directory with other utilities:
  * `clause/`: Automatically convert QA-SRL to the simple clausal format.
  * `clause-ext/`: An extended clausal format including two preposition slots.
  * `clause-ext-ann/`: Annotate clause disambiguation in the extended format.
  * `clause-ext-demo/`: Show the construction of clause-level output from a question-first model trained with the extended clausal format.
  * `frame/`: Induce and evaluate verb frames.
  * `frame-ann/`: Manually annotate verb frames.
  * `frame-browse/`: View induced frames and construct gold data for question-paraphrasing eval.
  * `metrics/`: General utilities for computing metrics.
  * `model-eval/`: Evaluate QA-SRL models with varying output formats.
  * `model-gen/`: Auto-generate AllenNLP configs for all of the QA-SRL model components.
  * `reprocess/`: Fix the slot formatting errors in the QA-SRL Bank 2.0.

## Setup

Run `./scripts/setup.sh` to download extra data files.

### Python setup
I need to document this. I can't remember if AllenNLP was used as a source or
library dependency, and what version, or what version of PyTorch. The rest is
in `requirements.txt` though.

### Scala setup
You need the [mill](https://www.lihaoyi.com/mill/) build tool version 0.5.1 or
later.

## Usage

Still working on fixing up and documenting usage for all of the modules.

### Clause formation / discourse question demo

After doing the setup, you should have mill installed and you should have a
file `data/clausal-predictions.jsonl` of model predictions. Then, in
`scripts/clause_demo.scala`, modify the `domain` and `httpPort` values to the
ones you want to host the demo at (localhost is fine for the domain). Then
execute `./scripts/run_clause_demo.sh` and after it finishes printing output
the demo will be running at `http://<domain>:<port>/task/evaluation/preview`.
