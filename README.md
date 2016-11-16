# EntityRelationsNelson
Framework to clustering relations between named entities based on the context.

## INITIALIZATION

First, it is essential to set working directory, using `setwd`, where files are.
Second, load the following R files:
* `entity-processing`
* `logger`
* `debugger`
* `context-processing`
* `relation-detection`
* `analysis`
* `relation-extraction`
* `pair-treatment`
* `evaluation`

Third, set some parameters:
* `data.type`
  * `"folder"` if input is a folder with texts
  * `"text"` if input is a given text
* `textual.content`
  * a string indicating the folder path when `data.type="folder"`
  * a string with the exact content when `data.type="text"`
* `idiom` - `"pt"` is the only possible language by now
* `what.context`
  * `"all"` if it is the complete context to extract
  * `"between"` if it is the intermediate context to extract
* `only.consecutive`
  * `TRUE` allows pairing involving only consecutive named entities on the same sentence
  * `FALSE` allows pairing involving every named entities on the same sentence
* `exceptions` - vector with named entities to ignore on pairing process 
* `use.stemming` - `TRUE` / `FALSE`

## PREPROCESSING

...

## CLUSTERING

...

## EVALUATION

...
