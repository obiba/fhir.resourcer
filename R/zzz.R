# register S3 file getter
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Registering FHIRResourceResolver..."))
  resourcer::registerResourceResolver(FHIRResourceResolver$new())
}

# unregister S3 file getter
.onDetach <- function(libpath) {
  resourcer::unregisterResourceResolver("FHIRResourceResolver")
}
