#' FHIR resource client
#'
#' Build a FHIR handler from a resource object describing access to a 
#' HL7 FHIR server.
#'
#' @docType class
#' @format A R6 object of class FHIRResourceClient
#' @import resourcer R6 fhircrackr
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
    #' @return A data.frame.
    asDataFrame = function(...) {
      private$getOrRead()
    }
  ),
  private = list(
    .obj = NULL,
    getOrRead = function() {
      if (is.null(private$.obj)) {
        resource <- super$getResource()
        url <- substring(resource$url, 6) # remove leading 'fhir+'
        bundles <- fhir_fetch(url, resource$identity, resource$secret)
        design <- fhir_table_description(resource = fhir_resource_type(resource$format))
        private$.obj <- fhir_crack(bundles = bundles, design = design, verbose = 0)
      }
      private$.obj
    }
  )
)

# Resource type from the resource format: "Patient", also accepts the legacy XPath form "//Patient".
fhir_resource_type <- function(format) {
  type <- sub("^/+", "", if (is.null(format)) "" else format)
  if (nchar(type) == 0) "Patient" else type
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
