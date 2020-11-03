#' FHIR resource resolver
#'
#' Build a FHIR resource client from a resource object describing access to a 
#' HL7 FHIR server.
#'
#' @docType class
#' @format A R6 object of class FHIRResourceResolver
#' @import resourcer R6
#' @export
FHIRResourceResolver <- R6::R6Class(
  "FHIRResourceResolver",
  inherit = ResourceResolver,
  public = list(
    #' @description Check if resource applies to this resolver.
    #' @param x A resource object.
    isFor = function(x) {
      if (super$isFor(x)) {
        startsWith(x$url, "fhir+")
      } else {
        FALSE
      }
    },
    #' @description Create a new instance.
    #' @param x A resource object.
    newClient = function(x) {
      if (self$isFor(x)) {
        FHIRResourceClient$new(x)
      } else {
        NULL
      }
    }
  )
)