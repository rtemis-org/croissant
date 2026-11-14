# croissant 0.0.2

- Every typed property of the node classes is an 'rtemis.core' (>= 0.5.0) property declaration, so an assignment is validated by the same rules and messages as every other rtemis class.
- Each node class with a JSON-LD type holds it in a `type` property, such as `"cr:Field"` for `Field` and `"sc:Dataset"` for `Croissant`.
- A text property, such as `name` or `description`, holds a string, a character vector of strings without language tags, a character vector named by distinct language tags, or a list of tagged strings for text whose tags repeat or are missing from some strings (see `?croissant`); `read_croissant()` reads and `write_croissant()` writes each form.
- `record_set` and `distribution` key each node by its `@id`, else its first name, else its position, made distinct by `make.unique()`.
- `croissant_from_data()` and `read_record_set()` read and write Parquet with 'arrow' when it is installed and with 'nanoparquet' otherwise, so Parquet record sets read and write without 'arrow', including under webR. 'nanoparquet' writes date-times in UTC and reads an ordered factor as an unordered one.

# croissant 0.0.1

- `read_croissant()` reads Croissant 1.0 and 1.1 JSON-LD metadata from a file, URL, or JSON string into a `Croissant` object.
- `write_croissant()` writes a `Croissant` object as JSON-LD with the standard context of its version.
- S7 classes `Croissant`, `FileObject`, `FileSet`, `RecordSet`, `Field`, `ParentField`, `Source`, `Extract`, `Transform`, `Person`, `Organization`, and `CreativeWork` represent Croissant nodes, with the properties that 'mlcroissant' types; every other property is kept in `other_properties`.
- S7 generics `record_sets()`, `enumerations()`, `fields()`, `files()`, and `about()` return the data record sets, the enumeration record sets, the fields of a record set, the files, and the dataset-level metadata of a `Croissant` object as R values; `fields()` and `files()` return a data.table, tibble, data.frame, or arrow Table.
- `print()` on each node class shows its `@id`, description, and the properties that are set; a `Croissant` object lists its record sets with their fields, then its enumerations, files, and metadata, and the calls that list a record set's fields and read its records; its `record_set` and `distribution` lists are named by `@id`.
- `croissant_context()` returns the standard JSON-LD context of a Croissant version.
- `validate_croissant()` reports every error and warning in Croissant metadata as a `CroissantIssues` object; `is_valid()` tests it for errors, and `issues()` returns its issues, or those of a `read_croissant()` error, as a table, optionally of one severity.
- `read_croissant()` reports every read problem of a document in one error.
- `read_croissant()` and `croissant_from_data()` record in `base` the directory that relative `contentUrl`s resolve against, so `read_record_set()` reads the records of an object read from a file, URL, or Hugging Face path without `base`.
- `read_record_set()` reads the records of a record set as a data.table, tibble, data.frame, or arrow Table, from CSV, TSV, JSON, JSON Lines, and Parquet files, local or remote, including files in zip and tar archives; each column is named by its field `@id` without the record set's `@id` and `/`.
- `croissant_from_data()` writes a table to a CSV or Parquet file and returns Croissant metadata that describes it, with field data types from column classes and the file's SHA-256 checksum; every accepted column class (integer, integer64, double, logical, character, factor, ordered factor, Date, POSIXct) reads back through `read_record_set()` identical, with its levels, order, and time zone, and any other class is an error; a factor's levels, in order, form an enumeration record set that its field references, and a Parquet file keeps the factor as a dictionary-encoded column with its levels.
- `read_record_set()` returns a text field that references an enumeration record set as a factor whose levels follow the enumeration's record order, a text column that a Parquet file stores dictionary-encoded as a factor with the file's levels, a `cr:Int64` field as integer64, and a Parquet date-time with its time zone.
- `read_croissant_hf()` reads the Croissant metadata of a Hugging Face dataset, and `read_croissant()` and `read_record_set()` accept `hf://datasets/` paths; `read_record_set()` downloads the dataset's Parquet files from the resolved commit and verifies them.
- `read_croissant_hf()` reports a dataset whose Croissant metadata the Hub has not generated yet, and names the repository's own `croissant.json` when it has one.
- `hf_download()` downloads and verifies one file of a Hugging Face dataset.
- `read_record_set()` gains `n_max`, which stops reading files once that many records are held, and `token`, for gated and private Hugging Face datasets.
- `read_croissant()`, `read_croissant_hf()`, and `read_record_set()` take `verbosity`; at `1L`, the default, each reports what it reads and then a summary: the dataset name with counts of record sets, enumerations, and files, or the rows and columns read, with the elapsed time and whether `n_max` was reached.
