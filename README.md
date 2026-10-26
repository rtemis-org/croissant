[![r-ci](https://github.com/rtemis-org/croissant/actions/workflows/r-ci.yml/badge.svg)](https://github.com/rtemis-org/croissant/actions/workflows/r-ci.yml) [![croissant status badge](https://rtemis-org.r-universe.dev/croissant/badges/version)](https://rtemis-org.r-universe.dev/croissant)

# croissant: 'croissant' Package

What the package does, in one paragraph ending with a period.

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

## Development

Development tasks go through the `justfile`; `just --list` shows every recipe.

- `just install` -- format, document, and install
- `just test` -- run the test suite
- `just lint`, `just check-rd`, `just spell`
- `just check-cran` -- run `R CMD check --as-cran`
