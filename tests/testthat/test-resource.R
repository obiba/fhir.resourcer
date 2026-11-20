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

test_that("Format gives the resource type, in plain or legacy XPath form, and the elements to melt", {
  expect_equal(fhir_parse_format(NULL), list(type = "Patient", melt = NULL))
  expect_equal(fhir_parse_format(""), list(type = "Patient", melt = NULL))
  expect_equal(fhir_parse_format("Observation"), list(type = "Observation", melt = NULL))
  expect_equal(fhir_parse_format("//Observation"), list(type = "Observation", melt = NULL))
  expect_equal(fhir_parse_format("Observation| component |code.coding|"),
               list(type = "Observation", melt = c("component", "code.coding")))
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

# client on the fixture bundle: 2 Patients, 3 Observations, 1 Condition
fixture_client <- function(format = NULL) {
  local_mocked_bindings(fhir_fetch = function(...) {
    fhircrackr::fhir_load(test_path("fixtures", "include"))
  }, .env = parent.frame())
  FHIRResourceClient$new(resourcer::newResource(url = "fhir+https://example.org/fhir/Patient", format = format))
}

test_that("References are normalised to Type/id", {
  expect_equal(fhir_reference_key(c("Patient/p1", "https://example.org/fhir/Patient/p2/_history/3",
                                    "https://example.org/fhir/Patient/p3", NA)),
               c("Patient/p1", "Patient/p2", "Patient/p3", NA))
})

test_that("Bundles are fetched once and all resource types are listed", {
  n <- 0
  local_mocked_bindings(fhir_fetch = function(...) {
    n <<- n + 1
    fhircrackr::fhir_load(test_path("fixtures", "include"))
  })
  client <- FHIRResourceClient$new(resourcer::newResource(url = "fhir+https://example.org/fhir/Patient"))
  expect_setequal(client$getResourceTypes(), c("Patient", "Observation", "Condition"))
  client$asDataFrame()
  client$getTable("Observation")
  expect_equal(n, 1)
})

test_that("asDataFrame returns the table of the configured type, with subject ids", {
  df <- fixture_client()$asDataFrame()
  expect_equal(df$id, c("p1", "p2"))
  obs <- fixture_client("Observation")$asDataFrame()
  expect_equal(obs$id, c("o1", "o2", "o3"))
  expect_equal(obs$subject.id, c("p1", "p2", "unknown"))
})

test_that("Tables can be extracted by type, with a column selection", {
  client <- fixture_client()
  tables <- client$getTables()
  expect_setequal(names(tables), c("Patient", "Observation", "Condition"))
  expect_equal(nrow(tables$Condition), 1)
  obs <- client$getTable("Observation", cols = c(id = "id", value = "valueQuantity/value"))
  expect_equal(names(obs), c("id", "value"))
  expect_equal(nrow(client$getTable("Encounter")), 0)
})

test_that("Tables are joined on references", {
  client <- fixture_client()
  df <- client$joinTables("Observation", "Patient")
  expect_equal(nrow(df), 3)
  expect_equal(df$Patient.id, c("p1", "p2", NA))
  expect_equal(df$Patient.gender, c("female", "male", NA))
  expect_equal(df$valueQuantity.value, c("13.2", "14.8", "12.1"))
  expect_equal(client$joinTables("Condition", "Patient")$Patient.birthDate, "1970-01-01")
  expect_equal(client$joinTables("Observation", "Encounter"), client$getTable("Observation"))
  expect_error(client$joinTables("Patient", "Observation"), "No 'subject' reference")
})

test_that("Multi-valued references are rejected", {
  client <- fixture_client()
  obs <- client$getTable("Observation")
  obs$subject.reference[1] <- "Patient/p1:::Patient/p2"
  client$.__enclos_env__$private$.tables$Observation <- obs
  expect_error(client$joinTables("Observation", "Patient"), "Multi-valued 'subject'")
})

test_that("Resources repeated across pages appear once", {
  local_mocked_bindings(fhir_fetch = function(...) {
    page <- function() xml2::read_xml(test_path("fixtures", "include", "1.xml"))
    fhircrackr::fhir_bundle_list(list(page(), page()))
  })
  client <- FHIRResourceClient$new(resourcer::newResource(url = "fhir+https://example.org/fhir/Patient"))
  expect_equal(client$getTable("Patient")$id, c("p1", "p2"))
})

# client on the repeated elements fixture: o1 has 2 codings (LOINC and local), o2 has 2 components
repeated_client <- function(format = NULL) {
  local_mocked_bindings(fhir_fetch = function(...) {
    fhircrackr::fhir_load(test_path("fixtures", "repeated"))
  }, .env = parent.frame())
  FHIRResourceClient$new(resourcer::newResource(url = "fhir+https://example.org/fhir/Observation", format = format))
}

test_that("Repeated elements are pasted in a single cell by default", {
  df <- repeated_client()$getTable("Observation")
  expect_equal(df$code.coding.code, c("718-7:::HB", "85354-9"))
  expect_equal(df$component.valueQuantity.value, c(NA, "120:::80"))
})

test_that("Repeated elements are melted into rows", {
  client <- repeated_client()
  df <- client$getTable("Observation", melt = "component")
  expect_equal(df$id, c("o1", "o2", "o2"))
  expect_equal(df$component.code.coding.code, c(NA, "8480-6", "8462-4"))
  expect_equal(df$component.valueQuantity.value, c(NA, "120", "80"))
  expect_equal(df$code.coding.code, c("718-7:::HB", "85354-9", "85354-9"))
  expect_equal(df$subject.id, c("p1", "p1", "p1"))
  expect_false("resource_identifier" %in% names(df))

  df <- client$getTable("Observation", melt = "code.coding")
  expect_equal(df$id, c("o1", "o1", "o2"))
  expect_equal(df$code.coding.system, c("http://loinc.org", "urn:local", "http://loinc.org"))

  df <- client$getTable("Observation", melt = c("component", "code.coding", "nothing"))
  expect_equal(df$id, c("o1", "o1", "o2", "o2"))
  expect_equal(df$code.coding.code, c("718-7", "HB", "85354-9", "85354-9"))
  expect_equal(df$component.valueQuantity.value, c(NA, NA, "120", "80"))

  expect_equal(nrow(client$getTable("Encounter", melt = "component")), 0)
})

test_that("Format melts repeated elements in asDataFrame", {
  df <- repeated_client("Observation|component")$asDataFrame()
  expect_equal(df$component.code.coding.code, c(NA, "8480-6", "8462-4"))
})

test_that("A single coding is selected with an XPath filter", {
  df <- repeated_client()$getTable("Observation",
    cols = c(id = "id", loinc = "code/coding[system/@value='http://loinc.org']/code"))
  expect_equal(df$loinc, c("718-7", "85354-9"))
})
