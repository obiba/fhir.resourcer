test_that("FHIR resource builder works", {
  res <- resourcer::newResource(
    name = "test",
    url = "fhir+https://hapi.fhir.org/baseR4/Patient",
    format = "Patient"
  )
  expect_equal(res$url, "fhir+https://hapi.fhir.org/baseR4/Patient")
  expect_null(res$identity)
  expect_null(res$secret)
  expect_equal(res$format, "Patient")
})

test_that("FHIR resource resolver applies to fhir+ URLs only", {
  resolver <- FHIRResourceResolver$new()
  expect_true(resolver$isFor(resourcer::newResource(url = "fhir+https://example.org/fhir/Patient")))
  expect_false(resolver$isFor(resourcer::newResource(url = "https://example.org/fhir/Patient")))
})

test_that("Resource type accepts plain and legacy XPath forms", {
  expect_equal(fhir_resource_type(NULL), "Patient")
  expect_equal(fhir_resource_type(""), "Patient")
  expect_equal(fhir_resource_type("Observation"), "Observation")
  expect_equal(fhir_resource_type("//Observation"), "Observation")
})

test_that("FHIR resource client extracts resources and passes credentials", {
  calls <- list()
  local_mocked_bindings(fhir_fetch = function(url, identity = NULL, secret = NULL) {
    calls[[length(calls) + 1]] <<- list(url = url, identity = identity, secret = secret)
    fhircrackr::fhir_unserialize(fhircrackr::example_bundles1)
  })

  res <- resourcer::newResource(url = "fhir+https://example.org/fhir/Patient", format = "//Patient",
                                identity = "user", secret = "pwd")
  df <- FHIRResourceClient$new(res)$asDataFrame()
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_true("id" %in% names(df))
  expect_equal(calls[[1]], list(url = "https://example.org/fhir/Patient", identity = "user", secret = "pwd"))

  res <- resourcer::newResource(url = "fhir+https://example.org/fhir/Patient", secret = "token")
  FHIRResourceClient$new(res)$asDataFrame()
  expect_null(calls[[2]]$identity)
  expect_equal(calls[[2]]$secret, "token")
})

test_that("Credentials map to basic authentication or bearer token", {
  args <- NULL
  local_mocked_bindings(fhir_search = function(...) args <<- list(...))
  fhir_fetch("u")
  expect_equal(args, list("u", verbose = 0))
  fhir_fetch("u", "user", "pwd")
  expect_equal(args, list("u", username = "user", password = "pwd", verbose = 0))
  fhir_fetch("u", NULL, "tok")
  expect_equal(args, list("u", token = "tok", verbose = 0))
})
