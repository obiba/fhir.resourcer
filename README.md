# FHIR Resource R

[![Build Status](https://travis-ci.com/obiba/fhir.resourcer.svg?branch=master)](https://travis-ci.com/obiba/fhir.resourcer)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fhir.resourcer)](https://cran.r-project.org/package=fhir.resourcer)

The `fhir.resourcer` package is for accessing the patients' data from a [HL7 FHIR](http://hl7.org/fhir/) server. It makes use of the [fhircrackr](https://github.com/POLAR-fhiR/fhircrackr) R package.

Usage example:

```
library(fhir.resourcer)
res <- resourcer::newResource(url="fhir+https://hapi.fhir.org/baseR4/Patient")
client <- resourcer::newResourceClient(res)
client$asDataFrame()
```
