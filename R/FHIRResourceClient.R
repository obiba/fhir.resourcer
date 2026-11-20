#' FHIR resource client
#'
#' Build a FHIR handler from a resource object describing access to a
#' HL7 FHIR server.
#'
#' The FHIR bundles are fetched once, using the resource URL, and cached. All
#' the tables are extracted from these bundles: several resource types can be
#' obtained in one search with `_include`/`_revinclude`. No other query is
#' issued, so the resource URL defines the scope of the accessible data.
#'
#' The client can be used in two ways:
#'
#' * `asDataFrame()` returns the table of the resource type configured by the
#'   resource `format` (default `Patient`). The format can also list repeated
#'   elements to expand into rows, separated by `|`, e.g. `Observation|component`.
#'
#' Repeated elements (several codings, components...) are otherwise pasted in a
#' single cell, separated by `:::`. They can be expanded into rows (`melt`), or a
#' single repetition can be selected with an XPath filter in the columns (`cols`).
#' * `getResourceTypes()`, `getTable()`, `getTables()` and `joinTables()` give
#'   access to all the resource types present in the bundles, e.g. for
#'   server-side analysis packages.
#'
#' A `subject.id` (resp. `patient.id`) column is added to the tables having a
#' `subject` (resp. `patient`) reference: it holds the identifier of the
#' referenced resource, to be matched with its `id` column.
#'
#' @docType class
#' @format A R6 object of class FHIRResourceClient
#' @import resourcer R6 fhircrackr
#' @importFrom xml2 xml_find_all xml_name
#' @importFrom stats setNames
#' @export
FHIRResourceClient <- R6::R6Class(
  "FHIRResourceClient",
  inherit = ResourceClient,
  public = list(

    #' @description Creates a new FHIRResourceClient instance.
    #' @param resource A valid resource object.
    #' @return A FHIRResourceClient object.
    initialize = function(resource) {
      super$initialize(resource)
    },

    #' @description Coerce the resource value extracted from the FHIR server to a data.frame.
    #' @param ... Additional parameters to as.data.frame (not used yet).
    #' @return A data.frame of the resources of the type defined by the resource format.
    asDataFrame = function(...) {
      format <- fhir_parse_format(super$getResource()$format)
      self$getTable(format$type, melt = format$melt)
    },

    #' @description Get the FHIR bundles returned by the resource URL (fetched once).
    #' @return A fhircrackr bundle list.
    getBundles = function() {
      if (is.null(private$.bundles)) {
        resource <- super$getResource()
        url <- substring(resource$url, 6) # remove leading 'fhir+'
        private$.bundles <- fhir_fetch(url, resource$identity, resource$secret)
      }
      private$.bundles
    },

    #' @description Get the types of the resources present in the bundles.
    #' @return A character vector of resource types, e.g. "Patient", "Observation".
    getResourceTypes = function() {
      xpath <- "/*[local-name()='Bundle']/*[local-name()='entry']/*[local-name()='resource']/*"
      unique(unlist(lapply(self$getBundles(), function(bundle) {
        xml2::xml_name(xml2::xml_find_all(bundle, xpath))
      })))
    },

    #' @description Extract the table of a resource type from the bundles.
    #' @param type The resource type, e.g. "Observation".
    #' @param cols Columns to extract, passed to fhircrackr::fhir_table_description().
    #'   Default is all the elements. XPath filters select a single repetition, e.g.
    #'   `c(loinc = "code/coding[system/@value='http://loinc.org']/code")`.
    #'   Tables with default columns are cached.
    #' @param melt Repeated elements to expand into rows, e.g. "component" or
    #'   "code.coding" (column name prefixes). The other columns are repeated in each
    #'   row; melting several elements gives all their combinations. Default is none:
    #'   repetitions are pasted in a single cell, separated by `:::`.
    #' @return A data.frame, empty if the resource type is not present.
    getTable = function(type, cols = NULL, melt = NULL) {
      if (!is.null(cols)) {
        return(fhir_crack_table(self$getBundles(), type, cols, melt))
      }
      key <- paste(c(type, melt), collapse = "|")
      if (is.null(private$.tables[[key]])) {
        private$.tables[[key]] <- fhir_crack_table(self$getBundles(), type, melt = melt)
      }
      private$.tables[[key]]
    },

    #' @description Extract the tables of several resource types from the bundles.
    #' @param types The resource types. Default is all the types present in the bundles.
    #' @return A named list of data.frames.
    getTables = function(types = self$getResourceTypes()) {
      stats::setNames(lapply(types, self$getTable), types)
    },

    #' @description Join the resources of a type with the resources they reference.
    #' @param from The type of the referencing resources, e.g. "Observation".
    #' @param to The type of the referenced resources, e.g. "Patient".
    #' @param reference The referencing element, e.g. "subject".
    #' @return A data.frame with one row per `from` resource and the columns of
    #'   the referenced `to` resource, prefixed by its type (e.g. "Patient.gender"),
    #'   empty when the reference does not resolve. No column is added when no
    #'   `to` resource is present.
    joinTables = function(from, to, reference = "subject") {
      fromTable <- self$getTable(from)
      toTable <- self$getTable(to)
      refCol <- paste0(reference, ".reference")
      if (!(refCol %in% names(fromTable))) {
        stop("No '", reference, "' reference in the ", from, " resources", call. = FALSE)
      }
      refs <- fromTable[[refCol]]
      if (any(grepl(":::", refs, fixed = TRUE))) {
        stop("Multi-valued '", reference, "' references in the ", from, " resources cannot be joined", call. = FALSE)
      }
      idx <- match(fhir_reference_key(refs), paste0(to, "/", toTable[["id"]]))
      joined <- toTable[idx, , drop = FALSE]
      if (ncol(joined) > 0) names(joined) <- paste0(to, ".", names(joined))
      rownames(joined) <- NULL
      cbind(fromTable, joined)
    }
  ),
  private = list(
    .bundles = NULL,
    .tables = list()
  )
)

# Parse the resource format "Type|element|element": the resource type (default
# "Patient", also accepts the legacy XPath form "//Patient") and the repeated
# elements to melt.
fhir_parse_format <- function(format) {
  parts <- trimws(strsplit(if (is.null(format)) "" else format, "|", fixed = TRUE)[[1]])
  type <- sub("^/+", "", if (length(parts) > 0) parts[1] else "")
  melt <- parts[-1]
  list(type = if (nchar(type) == 0) "Patient" else type,
       melt = if (length(melt[nchar(melt) > 0]) > 0) melt[nchar(melt) > 0] else NULL)
}

# Search the FHIR server. A user name with a password is basic authentication,
# a secret without a user name is a bearer token.
fhir_fetch <- function(url, identity = NULL, secret = NULL) {
  has <- function(x) !is.null(x) && nchar(x) > 0
  if (has(identity)) {
    fhir_search(url, username = identity, password = secret, verbose = 0)
  } else if (has(secret)) {
    fhir_search(url, token = secret, verbose = 0)
  } else {
    fhir_search(url, verbose = 0)
  }
}

# Crack the resources of one type, melting the repeated elements and adding the
# identifiers of the subject/patient references.
fhir_crack_table <- function(bundles, type, cols = NULL, melt = NULL) {
  brackets <- if (length(melt) > 0) c("[", "]") else character()
  design <- if (is.null(cols)) {
    fhir_table_description(type, brackets = brackets)
  } else {
    fhir_table_description(type, cols = cols, brackets = brackets)
  }
  df <- suppressWarnings(fhir_crack(bundles = bundles, design = design, verbose = 0))
  if (length(melt) > 0 && nrow(df) > 0) {
    df <- fhir_rm_indices(fhir_melt_elements(df, melt, brackets), brackets = brackets)
  }
  for (ref in c("subject", "patient")) {
    refCol <- paste0(ref, ".reference")
    if (refCol %in% names(df)) {
      df[[paste0(ref, ".id")]] <- sub("^[^/]*/", "", fhir_reference_key(df[[refCol]]))
    }
  }
  df
}

# Normalise references to "Type/id": relative ("Patient/123") or absolute and
# versioned ("https://host/fhir/Patient/123/_history/2") references.
fhir_reference_key <- function(reference) {
  key <- sub("/_history/[^/]*$", "", reference)
  sub("^.*/([^/]+/[^/]+)$", "\\1", key)
}

# Melt the repeated elements, one level at a time: fhir_melt() splits the outer
# level of the indices, so "code.coding" is melted on "code" then on "code.coding".
# Elements absent from the table are ignored.
fhir_melt_elements <- function(df, melt, brackets) {
  melted <- character()
  for (element in melt) {
    parts <- strsplit(element, ".", fixed = TRUE)[[1]]
    for (i in seq_along(parts)) {
      prefix <- paste(parts[1:i], collapse = ".")
      cols <- names(df)[names(df) == prefix | startsWith(names(df), paste0(prefix, "."))]
      if (!(prefix %in% melted) && length(cols) > 0) {
        df <- fhir_melt(df, columns = cols, brackets = brackets, sep = ":::", all_columns = TRUE)
        df$resource_identifier <- NULL
        melted <- c(melted, prefix)
      }
    }
  }
  df
}
