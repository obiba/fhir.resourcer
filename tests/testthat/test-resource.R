test_that("FHIR resource builder works", {
  res <- resourcer::newResource(
    name = "test",
    url = "fihr+https://hapi.fhir.org/baseR4/Patient",
    format = "//Patient"
  )
  expect_equal(res$name, "test")
  expect_equal(res$url, "fihr+https://hapi.fhir.org/baseR4/Patient")
  expect_null(res$identity)
  expect_null(res$secret)
  expect_equal(res$format, "//Patient")
  expect_equal(class(res), "resource")
  #client <- resourcer::newResourceClient(res)
  #expect_true("FileResourceClient" %in%  class(client))
  #expect_true("TidyFileResourceClient" %in%  class(client))
})

