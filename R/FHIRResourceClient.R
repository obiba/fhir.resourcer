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
        format <- resource$format
        if (is.null(format)) {
          format <- "//Patient"
        }
        url <- substring(resource$url, 6) # remove leading 'fhir+'
        bundles <- fhir_search(url)
        design <- list(
          entities = list(
            resource = format,
            cols = "./*"
          )
        )
        dfs <- fhir_crack(bundles = bundles, design = design)
        private$.obj <- dfs$entities
      }
      private$.obj
    }
  )
)