# 02_S7_nodes.R
# ::croissant::
# 2026- EDG rtemis.org

# S7 classes for the Croissant node types.
# spec: croissant/read-write#node-classes
#
# Each class types the properties that mlcroissant 1.1.1 types on the same
# node. Every other property of the node is kept in `other_properties`, keyed
# by its canonical JSON-LD key and holding its parsed JSON value, so reading
# and writing a document loses nothing. `.node_terms` maps each typed property
# to its JSON-LD key and to the conversion `read_value()` and `write_value()`
# apply.

# %% Properties ----
# Each typed property holds any value a Croissant document gives it, so that a
# document reads without loss and `validate_croissant()` reports what it
# finds: a string admits "" and strings of spaces, such as a separator of " ",
# and a list admits zero elements. Every declaration is nullable; NULL is a
# property the document does not give.

# A JSON string.
nullable_string <- prop_string(nullable = TRUE, min_length = 0L)

# One or more JSON strings.
nullable_strings <- prop_array(prop_string(min_length = 0L), nullable = TRUE)

# A JSON number.
nullable_number <- prop_number(nullable = TRUE)

# Parsed JSON records: an array or an object.
nullable_records <- prop_union(
  list(
    prop_array(prop_any(), min_items = 0L),
    prop_map(prop_any(), min_members = 0L)
  ),
  nullable = TRUE
)

# Properties without a typed counterpart, keyed by canonical JSON-LD key, each
# holding its parsed JSON value.
other_members <- prop_map(prop_any(), min_members = 0L, nullable = TRUE)

# Text, in one of four forms: a string; strings without language tags; strings
# named by distinct language tags; or a list of tagged strings, each a `value`
# with an optional `language`, for text whose tags repeat or are missing from
# some strings.
nullable_text <- prop_union(
  list(
    prop_string(min_length = 0L),
    prop_array(prop_string(min_length = 0L)),
    prop_map(prop_string(min_length = 0L)),
    prop_array(prop_struct(list(
      value = prop_string(min_length = 0L),
      language = prop_string(nullable = TRUE)
    )))
  ),
  nullable = TRUE
)

# An array of `Field` objects. `Field` declares two such properties, which
# refer to `Field` itself.
field_list <- new_property(
  class = NULL | class_list,
  validator = function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    ok <- vapply(value, \(v) is_node(v, "Field"), logical(1L))
    if (!all(ok) || !is.null(names(value))) {
      return("must be NULL or an unnamed list of Field objects")
    }
    NULL
  }
)

# A map of nodes, property `name` of the class, keyed by `node_keys()`, so
# that `x@record_set$titanic` selects a node. The setter runs in the
# constructor and on every assignment.
node_map <- function(name, values) {
  property <- prop_map(values, min_members = 0L, nullable = TRUE)
  property[["setter"]] <- function(self, value) {
    if (!is.null(value)) {
      names(value) <- node_keys(value)
    }
    prop(self, name) <- value
    self
  }
  property
}

# The key of each node: its `@id`, else its first name, else its position,
# made distinct by `make.unique()`. A document may repeat an `@id`, which
# `validate_croissant()` reports.
node_keys <- function(nodes) {
  keys <- vapply(nodes, node_key, character(1L))
  blank <- !nzchar(keys)
  keys[blank] <- as.character(which(blank))
  make.unique(keys)
}

# `@id` of a node, else its first name, else "".
node_key <- function(x) {
  if (!S7_inherits(x) || !"id" %in% prop_names(x)) {
    return("")
  }
  x@id %||% first_text(x@name) %||% ""
}


# %% Person ----
#' Person: a person who created or published a dataset
#'
#' @param type Character: JSON-LD `@type`, the constant `"sc:Person"`.
#' @param id Optional Character: `@id`.
#' @param name Optional Character or List: Name, as text (see [croissant-package]).
#' @param description Optional Character or List: Description, as text (see
#' [croissant-package]).
#' @param email Optional Character: Email address.
#' @param url Optional Character: URL.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `Person` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Person(name = "Ada Lovelace", email = "ada@example.org")
Person <- new_class(
  name = "Person",
  package = "croissant",
  properties = list(
    type = prop_string(const = "sc:Person"),
    id = nullable_string,
    name = nullable_text,
    description = nullable_text,
    email = nullable_string,
    url = nullable_string,
    other_properties = other_members
  )
) # /croissant::Person


# %% Organization ----
#' Organization: an organization that created or published a dataset
#'
#' @param type Character: JSON-LD `@type`, the constant `"sc:Organization"`.
#' @param id Optional Character: `@id`.
#' @param name Optional Character or List: Name, as text (see [croissant-package]).
#' @param description Optional Character or List: Description, as text (see
#' [croissant-package]).
#' @param email Optional Character: Email address.
#' @param url Optional Character: URL.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `Organization` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Organization(name = "MLCommons", url = "https://mlcommons.org")
Organization <- new_class(
  name = "Organization",
  package = "croissant",
  properties = list(
    type = prop_string(const = "sc:Organization"),
    id = nullable_string,
    name = nullable_text,
    description = nullable_text,
    email = nullable_string,
    url = nullable_string,
    other_properties = other_members
  )
) # /croissant::Organization


# %% CreativeWork ----
#' CreativeWork: a license or other work named by a dataset
#'
#' @param type Character: JSON-LD `@type`, the constant `"sc:CreativeWork"`.
#' @param id Optional Character: `@id`.
#' @param name Optional Character or List: Name, as text (see [croissant-package]).
#' @param description Optional Character or List: Description, as text (see
#' [croissant-package]).
#' @param url Optional Character: URL.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `CreativeWork` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' CreativeWork(name = "CC BY 4.0", url = "https://creativecommons.org/licenses/by/4.0/")
CreativeWork <- new_class(
  name = "CreativeWork",
  package = "croissant",
  properties = list(
    type = prop_string(const = "sc:CreativeWork"),
    id = nullable_string,
    name = nullable_text,
    description = nullable_text,
    url = nullable_string,
    other_properties = other_members
  )
) # /croissant::CreativeWork


# %% agent_list ----
# Agents: `Person` and `Organization` objects, told apart by `type`.
agent_list <- prop_array(
  prop_union(list(prop_instance(Person), prop_instance(Organization))),
  min_items = 0L,
  nullable = TRUE
)


# %% nullable_license ----
# Licenses: strings, or a list of strings and `CreativeWork` objects.
nullable_license <- prop_union(
  list(
    prop_array(prop_string(min_length = 0L)),
    prop_array(
      prop_union(list(
        prop_string(min_length = 0L),
        prop_instance(CreativeWork)
      ))
    )
  ),
  nullable = TRUE
)


# %% Extract ----
#' Extract: how a field's values are read from a source
#'
#' @param column Optional Character: Column name in a tabular file.
#' @param file_property Optional Character: File property, such as
#' `"content"` or `"filename"`.
#' @param json_path Optional Character: JSONPath expression.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `Extract` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Extract(column = "age")
Extract <- new_class(
  name = "Extract",
  package = "croissant",
  properties = list(
    column = nullable_string,
    file_property = nullable_string,
    json_path = nullable_string,
    other_properties = other_members
  )
) # /croissant::Extract


# %% Transform ----
#' Transform: a transformation applied to extracted values
#'
#' @param format Optional Character: Format by which the value is parsed, such as
#' a date format.
#' @param json_path Optional Character: JSONPath expression.
#' @param read_lines Optional Logical: Whether to split the content into
#' lines.
#' @param regex Optional Character: Regular expression whose first capture
#' group is kept.
#' @param replace Optional Character: Replacement pattern.
#' @param sampling_rate Optional Numeric: Sampling rate of audio content.
#' @param separator Optional Character: Separator that splits the value.
#' @param un_archive Optional Logical: Whether to extract an archive.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `Transform` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Transform(regex = "^(\\d+)_.*$")
Transform <- new_class(
  name = "Transform",
  package = "croissant",
  properties = list(
    format = nullable_string,
    json_path = nullable_string,
    read_lines = prop_boolean(default = NULL, nullable = TRUE),
    regex = nullable_string,
    replace = nullable_string,
    sampling_rate = nullable_number,
    separator = nullable_string,
    un_archive = prop_boolean(default = NULL, nullable = TRUE),
    other_properties = other_members
  )
) # /croissant::Transform


# %% Source ----
#' Source: where a field's values come from
#'
#' Names exactly one of `file_object`, `file_set`, or `field` by `@id`, and
#' optionally how values are extracted and transformed.
#'
#' @param file_object Optional Character: `@id` of a `FileObject`.
#' @param file_set Optional Character: `@id` of a `FileSet`.
#' @param record_set Optional Character: `@id` of a `RecordSet`.
#' @param field Optional Character: `@id` of a `Field`.
#' @param extract Optional `Extract` object.
#' @param transform Optional List: `Transform` objects, applied in order.
#' @param format Optional Character: Format of the source values.
#' @param sampling_rate Optional Numeric: Sampling rate of audio content.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `Source` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Source(file_object = "passengers.csv", extract = Extract(column = "name"))
Source <- new_class(
  name = "Source",
  package = "croissant",
  properties = list(
    file_object = nullable_string,
    file_set = nullable_string,
    record_set = nullable_string,
    field = nullable_string,
    extract = prop_instance(Extract, nullable = TRUE),
    transform = prop_array(
      prop_instance(Transform),
      min_items = 0L,
      nullable = TRUE
    ),
    format = nullable_string,
    sampling_rate = nullable_number,
    other_properties = other_members
  )
) # /croissant::Source


# %% ParentField ----
#' ParentField: the field a nested field joins to
#'
#' @param source Optional `Source` object.
#' @param references Optional `Source` object.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `ParentField` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' ParentField(source = Source(file_object = "data.json", extract = Extract(json_path = "$.id")))
ParentField <- new_class(
  name = "ParentField",
  package = "croissant",
  properties = list(
    source = prop_instance(Source, nullable = TRUE),
    references = prop_instance(Source, nullable = TRUE),
    other_properties = other_members
  )
) # /croissant::ParentField


# %% Field ----
#' Field: one column or attribute of a record set
#'
#' @param type Character: JSON-LD `@type`, the constant `"cr:Field"`.
#' @param id Optional Character: `@id`, unique within the document.
#' @param name Optional Character or List: Name, as text (see [croissant-package]).
#' @param description Optional Character or List: Description, as text (see
#' [croissant-package]).
#' @param data_type Optional Character: Data types, as compact IRIs such as
#' `"sc:Text"` or `"sc:Integer"`.
#' @param source Optional `Source` object: Where the values come from.
#' @param references Optional `Source` object: The field this field joins to.
#' @param sub_field Optional List: `Field` objects nested in this field.
#' @param parent_field Optional `ParentField` object.
#' @param annotation Optional List: `Field` objects that annotate this field.
#' @param repeated Optional Logical: Whether the field holds a list of values.
#' @param is_array Optional Logical: Whether the field holds an array.
#' @param array_shape Optional Character: Array shape, such as `"28,28"`.
#' @param equivalent_property Optional Character: IRIs of properties
#' equivalent to this field, such as `"prov:wasDerivedFrom"`.
#' @param value Optional: Constant value of the field, any JSON value.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `Field` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Field(
#'   id = "passengers/name",
#'   name = "name",
#'   data_type = "sc:Text",
#'   source = Source(file_object = "passengers.csv", extract = Extract(column = "name"))
#' )
Field <- new_class(
  name = "Field",
  package = "croissant",
  properties = list(
    type = prop_string(const = "cr:Field"),
    id = nullable_string,
    name = nullable_text,
    description = nullable_text,
    data_type = nullable_strings,
    source = prop_instance(Source, nullable = TRUE),
    references = prop_instance(Source, nullable = TRUE),
    sub_field = field_list,
    parent_field = prop_instance(ParentField, nullable = TRUE),
    annotation = field_list,
    repeated = prop_boolean(default = NULL, nullable = TRUE),
    is_array = prop_boolean(default = NULL, nullable = TRUE),
    array_shape = nullable_string,
    equivalent_property = nullable_strings,
    value = prop_any(),
    other_properties = other_members
  )
) # /croissant::Field


# %% RecordSet ----
#' RecordSet: a set of records with a common structure
#'
#' @param type Character: JSON-LD `@type`, the constant `"cr:RecordSet"`.
#' @param id Optional Character: `@id`, unique within the document.
#' @param name Optional Character or List: Name, as text (see [croissant-package]).
#' @param description Optional Character or List: Description, as text (see
#' [croissant-package]).
#' @param data_type Optional Character: Data types, such as
#' `"sc:Enumeration"`.
#' @param key Optional Character: `@id` of each field that forms the record
#' key.
#' @param field Optional List: `Field` objects.
#' @param annotation Optional List: `Field` objects that annotate the record
#' set.
#' @param data Optional List: Records given inline, as parsed JSON.
#' @param examples Optional List: Example records, as parsed JSON.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `RecordSet` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' RecordSet(id = "passengers", name = "passengers", field = list(Field(id = "passengers/name")))
RecordSet <- new_class(
  name = "RecordSet",
  package = "croissant",
  properties = list(
    type = prop_string(const = "cr:RecordSet"),
    id = nullable_string,
    name = nullable_text,
    description = nullable_text,
    data_type = nullable_strings,
    key = nullable_strings,
    field = prop_array(prop_instance(Field), min_items = 0L, nullable = TRUE),
    annotation = prop_array(
      prop_instance(Field),
      min_items = 0L,
      nullable = TRUE
    ),
    data = nullable_records,
    examples = nullable_records,
    other_properties = other_members
  )
) # /croissant::RecordSet


# %% FileObject ----
#' FileObject: a single file of the dataset
#'
#' @param type Character: JSON-LD `@type`, the constant `"cr:FileObject"`.
#' @param id Optional Character: `@id`, unique within the document.
#' @param name Optional Character or List: Name, as text (see [croissant-package]).
#' @param description Optional Character or List: Description, as text (see
#' [croissant-package]).
#' @param content_url Optional Character: URL or relative path of the file.
#' @param content_size Optional Character: Size, such as `"117743 B"`.
#' @param encoding_format Optional Character: MIME types, such as
#' `"text/csv"`.
#' @param sha256 Optional Character: SHA-256 checksum.
#' @param md5 Optional Character: MD5 checksum.
#' @param contained_in Optional Character: `@id` of each `FileObject` or
#' `FileSet` the file is extracted from. A `containedIn` given as a source
#' with a transform is kept in `other_properties`.
#' @param same_as Optional Character: URLs of the same file elsewhere.
#' @param source Optional `Source` object: The file this file is derived from.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `FileObject` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' FileObject(
#'   id = "passengers.csv",
#'   content_url = "data/titanic.csv",
#'   encoding_format = "text/csv"
#' )
FileObject <- new_class(
  name = "FileObject",
  package = "croissant",
  properties = list(
    type = prop_string(const = "cr:FileObject"),
    id = nullable_string,
    name = nullable_text,
    description = nullable_text,
    content_url = nullable_string,
    content_size = nullable_string,
    encoding_format = nullable_strings,
    sha256 = nullable_string,
    md5 = nullable_string,
    contained_in = nullable_strings,
    same_as = nullable_strings,
    source = prop_instance(Source, nullable = TRUE),
    other_properties = other_members
  )
) # /croissant::FileObject


# %% FileSet ----
#' FileSet: a set of files matched by glob patterns
#'
#' @param type Character: JSON-LD `@type`, the constant `"cr:FileSet"`.
#' @param id Optional Character: `@id`, unique within the document.
#' @param name Optional Character or List: Name, as text (see [croissant-package]).
#' @param description Optional Character or List: Description, as text (see
#' [croissant-package]).
#' @param encoding_format Optional Character: MIME types of the files.
#' @param includes Optional Character: Glob patterns of the files included.
#' @param excludes Optional Character: Glob patterns of the files excluded.
#' @param contained_in Optional Character: `@id` of each `FileObject` or
#' `FileSet` the files are extracted from. A `containedIn` given as a source
#' with a transform is kept in `other_properties`.
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values.
#'
#' @return `FileSet` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' FileSet(
#'   id = "images",
#'   contained_in = "archive.zip",
#'   encoding_format = "image/jpeg",
#'   includes = "*.jpg"
#' )
FileSet <- new_class(
  name = "FileSet",
  package = "croissant",
  properties = list(
    type = prop_string(const = "cr:FileSet"),
    id = nullable_string,
    name = nullable_text,
    description = nullable_text,
    encoding_format = nullable_strings,
    includes = nullable_strings,
    excludes = nullable_strings,
    contained_in = nullable_strings,
    other_properties = other_members
  )
) # /croissant::FileSet


# %% Croissant ----
#' Croissant: metadata of one dataset
#'
#' The Responsible AI properties (`rai:` namespace) are character vectors.
#' Dates are kept as the strings the document gives. Properties are read and
#' set with `$` and `[[`, as are the properties of the node classes.
#'
#' @param type Character: JSON-LD `@type`, the constant `"sc:Dataset"`.
#' @param version Character \{"1.0", "1.1"\}: Croissant version the metadata
#' conforms to.
#' @param id Optional Character: `@id` of the dataset.
#' @param name Optional Character or List: Name, as text (see [croissant-package]).
#' @param description Optional Character or List: Description, as text (see
#' [croissant-package]).
#' @param conforms_to Optional Character: IRIs of the specifications the
#' metadata conforms to. [write_croissant()] writes the Croissant IRI of
#' `version` when this omits it.
#' @param dataset_version Optional Character: Version of the dataset.
#' @param sd_version Optional Character: Version of the metadata document.
#' @param license Optional Character or List: Licenses, as names or URLs, or
#' a list of strings and `CreativeWork` objects.
#' @param sd_license Optional Character or List: Licenses of the metadata
#' document, in the form of `license`.
#' @param url Optional Character: URL of the dataset.
#' @param same_as Optional Character: URLs of the same dataset elsewhere.
#' @param cite_as Optional Character: Citation.
#' @param creator Optional List: `Person` and `Organization` objects.
#' @param publisher Optional List: `Person` and `Organization` objects.
#' @param date_created Optional Character: Creation date.
#' @param date_modified Optional Character: Modification date.
#' @param date_published Optional Character: Publication date.
#' @param keywords Optional Character: Keywords.
#' @param in_language Optional Character: Languages of the content, such as
#' `"en"`.
#' @param is_live_dataset Optional Logical: Whether the dataset changes over
#' time, in which case its files need no checksum.
#' @param distribution Optional List: `FileObject` and `FileSet` objects, each
#' named by its `@id`, else its first name, else its position, made distinct
#' by [make.unique()].
#' @param record_set Optional List: `RecordSet` objects, named as
#' `distribution` is, so that `x@record_set$<id>` selects one.
#' @param data_collection,data_collection_type,data_collection_missing_data,data_collection_raw_data Optional Character: How the data was collected (`rai:dataCollection`, `rai:dataCollectionType`, `rai:dataCollectionMissingData`, `rai:dataCollectionRawData`).
#' @param data_imputation_protocol,data_preprocessing_protocol,data_manipulation_protocol Optional Character: How the data was processed (`rai:dataImputationProtocol`, `rai:dataPreprocessingProtocol`, `rai:dataManipulationProtocol`).
#' @param data_annotation_protocol,data_annotation_platform,data_annotation_analysis,annotations_per_item,annotator_demographics,machine_annotation_tools Optional Character: How the data was annotated (`rai:dataAnnotationProtocol`, `rai:dataAnnotationPlatform`, `rai:dataAnnotationAnalysis`, `rai:annotationsPerItem`, `rai:annotatorDemographics`, `rai:machineAnnotationTools`).
#' @param data_biases,data_use_cases,data_limitations,data_social_impact,personal_sensitive_information,data_release_maintenance_plan Optional Character: Uses, limits, and maintenance of the data (`rai:dataBiases`, `rai:dataUseCases`, `rai:dataLimitations`, `rai:dataSocialImpact`, `rai:personalSensitiveInformation`, `rai:dataReleaseMaintenancePlan`).
#' @param other_properties Optional List: Properties without a typed
#' counterpart, keyed by canonical JSON-LD key, with their parsed JSON values,
#' such as the `prov:` properties.
#' @param base Optional Character: Directory or URL that relative
#' `contentUrl`s resolve against: the location of the document
#' [read_croissant()] read, or the directory [croissant_from_data()] wrote the
#' data file to. [read_record_set()] uses it when its `base` is `NULL`. It is
#' not a Croissant property, and [write_croissant()] does not write it.
#'
#' @return `Croissant` object.
#'
#' @author EDG
#' @export
#'
#' @examples
#' Croissant(
#'   name = "Titanic",
#'   creator = list(Organization(name = "OpenML")),
#'   distribution = list(FileObject(id = "passengers.csv", content_url = "titanic.csv"))
#' )
Croissant <- new_class(
  name = "Croissant",
  package = "croissant",
  properties = list(
    type = prop_string(const = "sc:Dataset"),
    version = prop_string(default = "1.1", enum = .croissant_versions),
    id = nullable_string,
    name = nullable_text,
    description = nullable_text,
    conforms_to = nullable_strings,
    dataset_version = nullable_string,
    sd_version = nullable_string,
    license = nullable_license,
    sd_license = nullable_license,
    url = nullable_string,
    same_as = nullable_strings,
    cite_as = nullable_string,
    creator = agent_list,
    publisher = agent_list,
    date_created = nullable_string,
    date_modified = nullable_string,
    date_published = nullable_string,
    keywords = nullable_strings,
    in_language = nullable_strings,
    is_live_dataset = prop_boolean(default = NULL, nullable = TRUE),
    distribution = node_map(
      "distribution",
      prop_union(list(prop_instance(FileObject), prop_instance(FileSet)))
    ),
    record_set = node_map("record_set", prop_instance(RecordSet)),
    data_collection = nullable_strings,
    data_collection_type = nullable_strings,
    data_collection_missing_data = nullable_strings,
    data_collection_raw_data = nullable_strings,
    data_imputation_protocol = nullable_strings,
    data_preprocessing_protocol = nullable_strings,
    data_manipulation_protocol = nullable_strings,
    data_annotation_protocol = nullable_strings,
    data_annotation_platform = nullable_strings,
    data_annotation_analysis = nullable_strings,
    annotations_per_item = nullable_strings,
    annotator_demographics = nullable_strings,
    machine_annotation_tools = nullable_strings,
    data_biases = nullable_strings,
    data_use_cases = nullable_strings,
    data_limitations = nullable_strings,
    data_social_impact = nullable_strings,
    personal_sensitive_information = nullable_strings,
    data_release_maintenance_plan = nullable_strings,
    other_properties = other_members,
    base = nullable_string
  )
) # /croissant::Croissant


# %% class_type ----
#' JSON-LD `@type` of a node class
#'
#' @param cls S7 class: Node class.
#'
#' @return Character: The constant of the class's `type` property, or NULL for
#' a class without one.
#'
#' @author EDG
#' @keywords internal
#' @noRd
class_type <- function(cls) {
  type <- cls@properties[["type"]]
  if (is.null(type)) NULL else prop_spec(type)[["const"]]
} # /croissant::class_type


# %% .node_terms ----
# Typed properties of each class: JSON-LD key, conversion kind, the node class
# of nested values, `alt`, a key that is read in place of `key` when `key` is
# absent, and `fallback`, whether a value that does not fit the kind is kept
# in `other_properties` without an issue (schema.org allows a creator to be a
# plain name, which is not a Person). `containedIn` is
# https://schema.org/containedIn in Croissant 1.0 and cr:containedIn in 1.1,
# and Field `value` is https://schema.org/value in mlcroissant and cr:value in
# the 1.1 context; the schema.org forms canonicalize to `sc:containedIn` and
# `sc:value`. `type` is the `@type` the class reads and writes, the constant of its `type` property.
.term <- function(key, kind, node = NULL, alt = NULL, fallback = FALSE) {
  list(key = key, kind = kind, node = node, alt = alt, fallback = fallback)
}

.agent_terms <- list(
  id = .term("@id", "string"),
  name = .term("name", "text"),
  description = .term("description", "text"),
  email = .term("email", "string"),
  url = .term("url", "string")
)

.rai_terms <- list(
  data_collection = "dataCollection",
  data_collection_type = "dataCollectionType",
  data_collection_missing_data = "dataCollectionMissingData",
  data_collection_raw_data = "dataCollectionRawData",
  data_imputation_protocol = "dataImputationProtocol",
  data_preprocessing_protocol = "dataPreprocessingProtocol",
  data_manipulation_protocol = "dataManipulationProtocol",
  data_annotation_protocol = "dataAnnotationProtocol",
  data_annotation_platform = "dataAnnotationPlatform",
  data_annotation_analysis = "dataAnnotationAnalysis",
  annotations_per_item = "annotationsPerItem",
  annotator_demographics = "annotatorDemographics",
  machine_annotation_tools = "machineAnnotationTools",
  data_biases = "dataBiases",
  data_use_cases = "dataUseCases",
  data_limitations = "dataLimitations",
  data_social_impact = "dataSocialImpact",
  personal_sensitive_information = "personalSensitiveInformation",
  data_release_maintenance_plan = "dataReleaseMaintenancePlan"
)

.node_terms <- list(
  Person = list(type = class_type(Person), terms = .agent_terms),
  Organization = list(type = class_type(Organization), terms = .agent_terms),
  CreativeWork = list(
    type = class_type(CreativeWork),
    terms = .agent_terms[c("id", "name", "description", "url")]
  ),
  Extract = list(
    type = NULL,
    terms = list(
      column = .term("column", "string"),
      file_property = .term("fileProperty", "string"),
      json_path = .term("jsonPath", "string")
    )
  ),
  Transform = list(
    type = NULL,
    terms = list(
      format = .term("format", "string"),
      json_path = .term("jsonPath", "string"),
      read_lines = .term("readLines", "logical"),
      regex = .term("regex", "string"),
      replace = .term("replace", "string"),
      sampling_rate = .term("samplingRate", "number"),
      separator = .term("separator", "string"),
      un_archive = .term("unArchive", "logical")
    )
  ),
  Source = list(
    type = NULL,
    terms = list(
      file_object = .term("fileObject", "ref"),
      file_set = .term("fileSet", "ref"),
      record_set = .term("recordSet", "ref"),
      field = .term("field", "ref"),
      extract = .term("extract", "node", "Extract"),
      transform = .term("transform", "nodes_or_one", "Transform"),
      format = .term("format", "string"),
      sampling_rate = .term("samplingRate", "number")
    )
  ),
  ParentField = list(
    type = NULL,
    terms = list(
      source = .term("source", "node", "Source"),
      references = .term("references", "node", "Source")
    )
  ),
  Field = list(
    type = class_type(Field),
    terms = list(
      id = .term("@id", "string"),
      name = .term("name", "text"),
      description = .term("description", "text"),
      data_type = .term("dataType", "strings"),
      source = .term("source", "node", "Source"),
      references = .term("references", "node", "Source"),
      sub_field = .term("subField", "nodes", "Field"),
      parent_field = .term("parentField", "node", "ParentField"),
      annotation = .term("annotation", "nodes", "Field"),
      repeated = .term("repeated", "logical"),
      is_array = .term("isArray", "logical"),
      array_shape = .term("arrayShape", "string"),
      equivalent_property = .term("equivalentProperty", "strings"),
      value = .term("value", "json", alt = "sc:value")
    )
  ),
  RecordSet = list(
    type = class_type(RecordSet),
    terms = list(
      id = .term("@id", "string"),
      name = .term("name", "text"),
      description = .term("description", "text"),
      data_type = .term("dataType", "strings"),
      key = .term("key", "refs"),
      field = .term("field", "nodes", "Field"),
      annotation = .term("annotation", "nodes", "Field"),
      data = .term("data", "json"),
      examples = .term("examples", "json")
    )
  ),
  FileObject = list(
    type = class_type(FileObject),
    terms = list(
      id = .term("@id", "string"),
      name = .term("name", "text"),
      description = .term("description", "text"),
      content_url = .term("contentUrl", "string"),
      content_size = .term("contentSize", "string"),
      encoding_format = .term("encodingFormat", "strings"),
      sha256 = .term("sha256", "string"),
      md5 = .term("md5", "string"),
      contained_in = .term(
        "containedIn",
        "refs",
        alt = "sc:containedIn",
        fallback = TRUE
      ),
      same_as = .term("sameAs", "strings"),
      source = .term("source", "node", "Source")
    )
  ),
  FileSet = list(
    type = class_type(FileSet),
    terms = list(
      id = .term("@id", "string"),
      name = .term("name", "text"),
      description = .term("description", "text"),
      encoding_format = .term("encodingFormat", "strings"),
      includes = .term("includes", "strings"),
      excludes = .term("cr:excludes", "strings"),
      contained_in = .term(
        "containedIn",
        "refs",
        alt = "sc:containedIn",
        fallback = TRUE
      )
    )
  ),
  Croissant = list(
    type = class_type(Croissant),
    terms = c(
      list(
        id = .term("@id", "string"),
        name = .term("name", "text"),
        description = .term("description", "text"),
        conforms_to = .term("conformsTo", "strings"),
        dataset_version = .term("version", "string"),
        sd_version = .term("sdVersion", "string"),
        license = .term("license", "license"),
        sd_license = .term("sdLicense", "license"),
        url = .term("url", "string"),
        same_as = .term("sameAs", "strings"),
        cite_as = .term("citeAs", "string"),
        creator = .term("creator", "agents", fallback = TRUE),
        publisher = .term("publisher", "agents", fallback = TRUE),
        date_created = .term("dateCreated", "string"),
        date_modified = .term("dateModified", "string"),
        date_published = .term("datePublished", "string"),
        keywords = .term("keywords", "strings"),
        in_language = .term("inLanguage", "strings"),
        is_live_dataset = .term("isLiveDataset", "logical"),
        distribution = .term("distribution", "distribution"),
        record_set = .term("recordSet", "nodes", "RecordSet")
      ),
      lapply(.rai_terms, \(k) .term(paste0("rai:", k), "strings"))
    )
  )
)


# Alternative keys of the typed properties: every `alt` key in `.node_terms`.
.alt_keys <- unique(unlist(lapply(.node_terms, function(spec) {
  lapply(spec[["terms"]], \(t) t[["alt"]])
})))


# %% is_node ----
#' Test whether a value is a node of one of the named classes
#'
#' @param x Value.
#' @param classes Character: Node class names.
#'
#' @return Logical scalar.
#'
#' @author EDG
#' @keywords internal
#' @noRd
is_node <- function(x, classes) {
  S7_inherits(x) && S7_class(x)@name %in% classes
} # /croissant::is_node


# %% node_class ----
#' Node class constructor by name
#'
#' @param name Character: Class name.
#'
#' @return S7 class.
#'
#' @author EDG
#' @keywords internal
#' @noRd
node_class <- function(name) {
  switch(
    name,
    Person = Person,
    Organization = Organization,
    CreativeWork = CreativeWork,
    Extract = Extract,
    Transform = Transform,
    Source = Source,
    ParentField = ParentField,
    Field = Field,
    RecordSet = RecordSet,
    FileObject = FileObject,
    FileSet = FileSet,
    Croissant = Croissant
  )
} # /croissant::node_class
