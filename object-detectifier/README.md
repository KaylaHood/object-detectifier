# object-detectifier

Developed using GHC v8.8.4 and Cabal v3.2.0.0

##Requirements:
* Haskell Stack Build Tool version 2.5.1 or greater
* Glasgow Haskell Compiler (GHC) version 8.8.4 (or greater, though I ran into build issues with newer versions due to my OS)
* Cabal version 3.2.0.0 (or greater, though I had the same issues with Cabal as I did with GHC re: my OS)
* SQLite 3 installed and accessible from your system PATH
* An active Google Cloud Platform (GCP) account (can be in the "free trial" stage)
* A Service Account created on your GCP Console for access to the Cloud Vision API.  
  Place the json private key in the location `~/.config/gcloud` and name the file `application_default_credentials.json`

##Build instructions:
* In stack.yaml, change `extra-include-dirs` and `extra-lib-dirs` to point to your machine's sqlite3 installation and source folders.  
  The location(s) provided must contain the source code (C libs) and sqlite3.exe (if they are in separate folders, provide multiple paths in the yaml).
* Build the project by running `stack build --test --haddock --no-haddock-hyperlink-source` from the project directory.
* Run with `stack run` - the server has started when "Listening on port 3000" is printed to the console.

##API endpoints:
*NOTE: Responses are NOT in JSON format yet. They are all plain text. Also, many errors are not properly handled - though the server should not stop running if an error occurs.*

* GET /images  
  Prints all images and their metadata (retrieved from the database) to the console.

* GET /images?objects=\[tags...\]  
  Prints all images with the given tags to the console. Does not include image metadata.  
  If you provide `objects=dog,cat` then all images tagged with EITHER "dog" or "cat" (or both) are returned.

* GET /images/{id}  
  Prints the image with the given ID to the console. IDs are integers.

* POST /images
  * Body: `{"location": "{URL or File Path}", "label": "{Optional}", "enable_detection": {Optional, default true -- NOT IMPLEMENTED}}`  
  Sends a request to Google's Cloud Vision API to annotate the image provided. Only tested with JPG and PNG file types.  
  Example CURL request and response:  
```bash
$ curl --header "Content-Type: application/json" --request POST --data '{"location":"D:\\Users\\kayla\\Pictures\\800px-American_quarter_horse.jpg","label":"","enable_detection":true}' "http://localhost:3000/images"

"GoogleCloudVisionV1p2beta1BatchAnnotateImagesResponse' { 
    _gcvvbairResponses = Just [GoogleCloudVisionV1p2beta1AnnotateImageResponse' { 
        _gcvvairLogoAnnotations = Nothing, 
        _gcvvairProductSearchResults = Nothing, 
        _gcvvairContext = Nothing, 
        _gcvvairLabelAnnotations = Nothing, 
        _gcvvairFaceAnnotations = Nothing, 
        _gcvvairError = Nothing, 
        _gcvvairWebDetection = Nothing, 
        _gcvvairSafeSearchAnnotation = Nothing, 
        _gcvvairLandmarkAnnotations = Nothing, 
        _gcvvairLocalizedObjectAnnotations = Just [ GoogleCloudVisionV1p2beta1LocalizedObjectAnnotation' { 
            _gcvvloacLanguageCode = Nothing, 
            _gcvvloacScore = Just (Textual 0.9829599), 
            _gcvvloacBoundingPoly = Just (GoogleCloudVisionV1p2beta1BoundingPoly' { 
                _gNormalizedVertices = Just [GoogleCloudVisionV1p2beta1NormalizedVertex' { 
                    _gX = Just (Textual 5.924554e-2), 
                    _gY = Just (Textual 0.15255374)
                },
                GoogleCloudVisionV1p2beta1NormalizedVertex' { 
                    _gX = Just (Textual 0.91654307), 
                    _gY = Just (Textual 0.15255374)
                },
                GoogleCloudVisionV1p2beta1NormalizedVertex' {
                    _gX = Just (Textual 0.91654307), 
                    _gY = Just (Textual 0.85388875)
                },
                GoogleCloudVisionV1p2beta1NormalizedVertex' {
                    _gX = Just (Textual 5.924554e-2), 
                    _gY = Just (Textual 0.85388875)
                }], 
                _gVertices = Nothing
            }), 
            _gcvvloacName = Just \"Horse\", 
            _gcvvloacMid = Just \"/m/03k3r\"
        }], 
        _gcvvairTextAnnotations = Nothing, 
        _gcvvairCropHintsAnnotation = Nothing, 
        _gcvvairFullTextAnnotation = Nothing, 
        _gcvvairImagePropertiesAnnotation = Nothing
    }
]}"

```