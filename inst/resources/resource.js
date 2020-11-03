var fhir_resourcer = {
  settings: {
    "title": "FHIR Resources",
    "description": "Resources that can be retrieved from HL7 FHIR server.",
    "web": "https://github.com/obiba/fhir.resourcer",
    "categories": [],
    "types": [
      {
        "name": "fhir",
        "title": "FHIR",
        "description": "FHIR Patient data.",
        "tags": ["database"],
        "parameters": {
          "$schema": "http://json-schema.org/schema#",
          "type": "array",
          "items": [
            {
              "key": "url",
              "type": "string",
              "title": "URL",
              "description": "The FHIR server URL, including search query."
            },
            {
              "key": "path",
              "type": "string",
              "title": "Path",
              "description": "XPath to the entities to be extracted from the FHIR bundles (see explanations about XPath in [fhircrackr](https://github.com/POLAR-fhiR/fhircrackr)). Optional, default value is '//Patient'."
            }
          ],
          "required": [
            "url"
          ]
        },
        "credentials": {
          "$schema": "http://json-schema.org/schema#",
          "type": "array",
          "description": "Credentials are optional. If not provided, no authentication will be applied.",
          "items": [
            {
              "key": "username",
              "type": "string",
              "title": "User name",
              "description": "The FHIR server user name, if it applies."
            },
            {
              "key": "password",
              "type": "string",
              "title": "Password",
              "format": "password",
              "description": "The FHIR server user password, if it applies."
            }
          ]
        }
      }
    ]
  },
  asResource: function(type, name, params, credentials) {

    //
    // Resource factory functions to be reused
    //
    var toFHIRResource = function(name, params, credentials) {
        return {
            name: name,
            url: "fhir+" + params.url,
            format: params.path ? params.path : "//Patient",
            identity: credentials.username,
            secret: credentials.password
        };
    };

    //
    // Resource factory functions by resource form type
    //
    var toResourceFactories = {
      "fhir": toFHIRResource
    };

    // Check if there is a resource factory function for the requested resource form type
    if (toResourceFactories[type]) {
        return toResourceFactories[type](name, params, credentials);
    }
    return undefined;
  }
}
