# Test fixtures

The files in `0.8/`, `1.0/`, and `1.1/` are the example metadata files from
`datasets/<version>/<name>/metadata.json` in
[mlcommons/croissant](https://github.com/mlcommons/croissant) at commit
`ec24358`, each renamed to `<name>.json`. `1.1-multilingual.json` is
`python/mlcroissant/mlcroissant/_src/tests/graphs/1.1/multilingual_fields/metadata.json`
from the same commit.

These files are copyright MLCommons and distributed under the Apache License,
Version 2.0, included as [LICENSE-croissant.md](LICENSE-croissant.md).

`invalid/1.0/` and `invalid/1.1/` hold
`python/mlcroissant/mlcroissant/_src/tests/graphs/<version>/<name>/metadata.json`
from the same commit, each renamed to `<name>.json`: documents with known
errors.

`mlcroissant-warnings.json` records, for each example in `1.0/` and `1.1/`,
the recommended properties that `mlcroissant` 1.1.1 reports as absent; none
of the examples has an error in `mlcroissant`.

`datasets/<version>/<name>/` holds `datasets/<version>/<name>/` from the same
commit for the datasets mlcroissant loads from local files in its hermetic
tests (`python/mlcroissant/mlcroissant/_src/datasets_test.py`), and
`titanic`: each `metadata.json` or recipe document, its `data/`, and its
`output/<record set>.jsonl`, the records mlcroissant produces.
`titanic/output/passengers.jsonl` holds the first 100 of its 1309 records.
