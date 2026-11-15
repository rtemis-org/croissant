[![r-ci](https://github.com/rtemis-org/croissant/actions/workflows/r-ci.yml/badge.svg)](https://github.com/rtemis-org/croissant/actions/workflows/r-ci.yml) [![croissant status badge](https://rtemis-org.r-universe.dev/croissant/badges/version)](https://rtemis-org.r-universe.dev/croissant)

# croissant

Read and write [`croissant`](https://docs.mlcommons.org/croissant/docs/croissant-spec.html) metadata files.

![croissant cover](https://docs.rtemis.org/r/croissant/assets/cover.avif)

## Installation

### Latest version from `r-universe`

```r
pak::repo_add(rtemis = "https://rtemis-org.r-universe.dev")
pak::pak("croissant")
```

or using `install.packages`:

```r
install.packages(
  'croissant',
  repos = c('https://rtemis-org.r-universe.dev', 'https://cloud.r-project.org')
)
```

### Development version from GitHub

```r
pak::pak("rtemis-org/croissant")
```

## Hugging Face datasets

A dataset on the Hugging Face Hub can have two Croissant documents: the metadata the Hub generates from its Parquet conversion, and a Croissant file the dataset's authors store in the repository.

```r
library(croissant)
# Metadata the Hub generates
x <- read_croissant_hf("rtemis/demo")
# The repository's own metadata file, at tag v1.0.0
y <- read_croissant("hf://datasets/rtemis/demo@v1.0.0/croissant.json")
dat <- read_record_set(y, "classification")
```

[Hugging Face datasets](https://docs.rtemis.org/r/croissant/huggingface.html) compares the two.

## Kaggle datasets

Kaggle generates Croissant metadata for each public dataset. A dataset can also store a Croissant file of its own, which is how a Parquet dataset on Kaggle is described, since Kaggle generates no record set for Parquet files. A Kaggle API token, read from `KAGGLE_API_TOKEN` or `~/.kaggle/access_token` as the Kaggle tools read it, gives access to private datasets.

```r
# Metadata Kaggle generates
x <- read_croissant_kaggle("yasserh/titanic-dataset")
dat <- read_record_set(x, "Titanic-Dataset.csv")
# The dataset's own metadata file
y <- read_croissant_kaggle("rtemisorg/rtemis-demo", file = "croissant.json")
dat <- read_record_set(y, "classification")
```

[Kaggle datasets](https://docs.rtemis.org/r/croissant/kaggle.html) describes both.

## Development

Development tasks go through the `justfile`; `just --list` shows every recipe.

- `just install` -- format, document, and install
- `just test` -- run the test suite
- `just lint`, `just check-rd`, `just spell`
- `just check-cran` -- run `R CMD check --as-cran`
