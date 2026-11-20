# FHIR Resource R

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fhir.resourcer)](https://cran.r-project.org/package=fhir.resourcer)
[![R-CMD-check](https://github.com/obiba/fhir.resourcer/actions/workflows/ci.yml/badge.svg)](https://github.com/obiba/fhir.resourcer/actions/workflows/ci.yml)

The `fhir.resourcer` package is for accessing the patients' data from a [HL7 FHIR](http://hl7.org/fhir/) server. It makes use of the [fhircrackr](https://github.com/POLAR-fhiR/fhircrackr) R package.

Usage example:

```
library(fhir.resourcer)
res <- resourcer::newResource(url="fhir+https://hapi.fhir.org/baseR4/Patient")
client <- resourcer::newResourceClient(res)
client$asDataFrame()
```

The resource `format` is the type of the FHIR resources to extract from the search results (default `Patient`), e.g. to get observations:

```
res <- resourcer::newResource(url="fhir+https://hapi.fhir.org/baseR4/Observation?code=http://loinc.org|736-9", format="Observation")
```

Authentication: set `identity` and `secret` for a user name and password, or only `secret` for a bearer token.

## Several resource types

The FHIR bundles are fetched once from the resource URL. Several resource types can be obtained in one search with `_include` or `_revinclude`, and then extracted and joined with the client methods, e.g. from a server-side analysis package:

```
res <- resourcer::newResource(url="fhir+https://hapi.fhir.org/baseR4/Observation?code=http://loinc.org|736-9&_include=Observation:subject", format="Observation")
client <- resourcer::newResourceClient(res)
client$getResourceTypes()                    # "Observation" "Patient"
client$getTable("Patient")                   # one table per resource type
client$getTables()                           # named list of tables
client$joinTables("Observation", "Patient")  # observations with Patient.* columns
```

No other query is issued: the resource URL defines the scope of the accessible data. Tables with a `subject` (or `patient`) reference get a `subject.id` (or `patient.id`) column, to be matched with the `id` of the referenced resource.
