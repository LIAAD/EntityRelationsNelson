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

### NAMED ENTITY PROCESSING

To know where named entities are on a text, there are two ways:
* `extractEntities` function locates named entities on input text(s), returning a data frame with 5 columns (please install `PAMPO` package and read respective documentation)
 * `source` - the path where text files are or the textual content
 * `to` - the CSV file name where named entity information is written. If empty no file is outputted
* `fileExtraction` function gets named entity information directly from a CSV file following the structure generated by `PAMPO`
 * `file` - the CSV file title
 * `file.header` - boolean value indicating if CSV file has header
 * `file.separator` - character(s) used as separator among attributes of each instance

Some named entity cleaning is done with `cleanEntities` function where `exceptions` are removed too.
* `entities` is the data frame with named entity information
* `special.cases` is a vector with row numbers from specific instances to remove
The returned data frame has the same structure but without the retired records.

If it is required only named entities, forming certain numbers on the sentences, it is required to use `countEntities` function firstly:
* `entity.locations` is the data frame with information about named entities and respective positions by `File`, `Paragraph` and `Sentence`
The returned data frame has `File`, `Paragraph`, `Sentence` and `occurrences` that counts named entities in the position defined by the previous three attributes.

Then `filterSentences` function allows to preserve named entity information, where this ones form certain numbers on respective sentences:
* `entity.locations` - data frame with information about named entities and respective positions by `File`, `Paragraph` and `Sentence`
* `sentence.entities` - data frame with the named entity occurrences per sentence 
* `entity.number` - a vector with the required numbers of named entities on sentences
The returned data frame has the same structure but without named entities escaping from filtering.

### NAMED ENTITY PAIRING

Such way that availed named entities are used to pairing, it is required to use `scanIteratively` or `scanEfficiently` where both has the same arguments:
* `entities` - a data frame with the attributes returned on recognition stage
* `relation.file` - a string, when it is not empty (value by default), to name a CSV file with every pairs and respective contexts encountered
* `order.significance`
 * `TRUE` - elements of named entity pairs are displayed by appearance order on sentence
 * `FALSE` - the elements are displayed by lexicographic order
The data frame that results has `entity1.name` and `entity2.name` attributes as left and right elements of pairs. Also it has `context` attribute that is a string for each pair and it is defined according `what.context` parameter. The number of returned pairs can be influenced by `only.consecutive` parameter.

To avoid repeated pairs and to join every contexts with pair in common, `aggregateContexts` function is essential:
* `relations` - data frame with the pairs of named entities and the extracted contexts
The returned data frame, with the same attributes as the argument, has distinct pairs and a list with every contexts for each pair.

## CLUSTERING

...

## EVALUATION

...
