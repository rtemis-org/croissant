# croissant NEWS

## Version 0.0.1

- `read_croissant()` reads Croissant 1.0 and 1.1 JSON-LD metadata from a file, URL, or JSON string into a `Croissant` object.
- `write_croissant()` writes a `Croissant` object as JSON-LD with the standard context of its version.
- S7 classes `Croissant`, `FileObject`, `FileSet`, `RecordSet`, `Field`, `ParentField`, `Source`, `Extract`, `Transform`, `Person`, `Organization`, and `CreativeWork` represent Croissant nodes, with the properties that 'mlcroissant' types; every other property is kept in `other_properties`.
- `croissant_context()` returns the standard JSON-LD context of a Croissant version.
- `validate_croissant()` reports every error and warning in Croissant metadata as a `CroissantIssues` object; `is_valid()` tests it for errors.
- `read_croissant()` reports every read problem of a document in one error.
- `read_record_set()` reads the records of a record set as a data.table, tibble, data.frame, or arrow Table, from CSV, TSV, JSON, JSON Lines, and Parquet files, local or downloaded, including files in zip and tar archives.
- `croissant_from_data()` writes a table to a CSV or Parquet file and returns Croissant metadata that describes it, with field data types from column classes and the file's SHA-256 checksum.
- `read_croissant_hf()` reads the Croissant metadata of a Hugging Face dataset, and `read_croissant()` and `read_record_set()` accept `hf://datasets/` paths; `read_record_set()` downloads the dataset's Parquet files from the resolved commit and verifies them.
- `hf_download()` downloads and verifies one file of a Hugging Face dataset.
- `read_record_set()` gains `n_max`, which stops reading files once that many records are held, and `token`, for gated and private Hugging Face datasets.
