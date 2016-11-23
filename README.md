# EntityRelationsNelson
Framework to clustering relations between named entities based on the context. Also it can evaluate the clustering results by constraint pairs. It is accepted only non-annotated texts as input.

Phases:
1. Initialization
2. Preprocessing
  2.1. Named Entity Processing
  2.2. Named Entity Pairing
3. Clustering
4. Evaluation

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
* `idiom` - defined language to named entity recognition and stemming (`"pt"` is the only possible by now)
* `what.context`
  * `"all"` if it is the complete context to extract
  * `"between"` if it is the intermediate context to extract
* `only.consecutive`
  * `TRUE` allows pairing involving only consecutive named entities on the same sentence
  * `FALSE` allows pairing involving every named entities on the same sentence
* `exceptions` - vector with named entities to ignore on pairing process and to remove on corpus mapping 
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

Picking named entity pairs and respective contexts, pair clustering can be executed with `defineClustering`:
* `pair.contexts` - data frame with named entity pairs and lists of contexts
* `weight.type` - term weighting (`"TF/IDF"` by default) (try `help(DocumentTermMatrix)` to know another options)
* `distance.measure` - measure (`"euclidean"` by default) to use on `dist` function (try `help(dist)` to know another options)
* `dist.args` - list of arguments (empty by default) to use on `dist` function (try `help(dist)` to know the arguments)
* `algorithm.type` - clustering algorithm to use where the possibilities are `"hierarchical"` and `"k-means"` (being this one by default)
* `hclust.args` - list of arguments (`list(method="ward.D")` by default) to use on `hclust` function (try `help(hclust)` to know the arguments)
* `kmeans.args` - list of arguments (`list(algorithm="Lloyd")` by default) to use on `kmeans` function (try `help(kmeans)` to know the arguments)
* `cluster.percentage` - number of clusters required based on the proportion of pairs (`0.1` by default)
* `remove.stopwords` - stopwords are removed on corpus mapping (where `use.stemming` and `exceptions` parameters also are used) if it is allowed (`TRUE` by default) 
* `words.number` - number of the most frequent words from each cluster that are used as labels (`1` by default)

The obtained data frame has `entity1.name`, `entity2.name`, `cluster.key` and `semantic.label`. This attributes correspond to the left and right elements from clustered pairs, the numbers of clusters where pairs are assigned and the labels that distinguish semantic relations between pairs.



## EVALUATION

After pairs be clustered, it is possible to evaluate the result by F1. Using `evaluateClustering`, it is necessary to give values for the arguments:
* `clusters` - data frame with named entity pairs (`entity1.name` and `entity2.name`), identifications of respective clusters (`cluster.key`) and the word sets that characterize semantic relations (`semantic.label`)
* `must.link` - vectors with strings of constraint pairs, where elements are named entity pairs and it could share the same cluster. The named entity strings of each pair element should be disposed, where left element is followed by right element like on `clusters` data frame. Both elements also should be followed.
* `cannot.link` - vectors with strings of constraint pairs, where elements are named entity pairs and it couldn't share the same cluster. The named entity strings of each pair element should be disposed, where left element is followed by right element like on `clusters` data frame. Both elements also should be followed.
* `logging` - if `TRUE`, constraint pairs, respective alerts of matching and values of precision/recall/F1 are showed. Otherwise, nothing is showed.
A value of double type with F1 score is returned.

