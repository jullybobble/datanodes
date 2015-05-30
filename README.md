Datanodes
=========

An R package caching the result of an expression on disk. It is intended for the incremental redaction of analysis code in R scripts, [R Markdown](http://rmarkdown.rstudio.com/) or [knitr](http://yihui.name/knitr/) documents.

It is in essence similar to the caching functionality of knitr[^knitroptions], available from the console; i.e., without having to *knit* the document to profit from the cache. See the *cache* section of the [knitr options](http://yihui.name/knitr/options) documentation for more information about this functionality in knitr.

Installation
------------

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github('jullybobble/datanodes@master')
```

Features
--------

-   cache files don't require the *datanodes* package to be read;
-   no medadata stored anywhere: relies uniquely on the files' "modified time" provided by the file system;
-   control the full path of `RData` file caching the result;
-   the `datanode` function returns the result of the evaluation of the cached expression, this allows to add a datanode cache anywhere without further modification of any existing code;
-   specify dependency files: the expression will be evaluated only if the dependency files were modified since the cache file was last update;
-   lazy evaluation of a dependency: if a dependency is updated, a datanode depending on this file will be updated only its result is required, this avoids evaluation of non-required results;
-   ask for user input conditioning the execution of the expression if in interactive mode;
-   force the evaluation of the expression, useful for triggering re-evaluation of a dependency graph.

### Planned Features

-   ability to specify the format of the cache file, for exaple CSV;
-   allow for dpendency times with a higher resolution and independent on the file system;
-   trigger re-evaluation on file hash;
-   trigger re-evaluation on expression hash.

Most of the planned features above require the storage of some metadata.

Usage
-----

``` r
library(datanodes)

model_cache <- tempfile()
model <- datanode(model_cache, { 
  # a potentially expensive operation
  # for this example, we choose a not so expensive one...
  lm(formula = mpg ~ wt, data = mtcars)
})
```

During the first execution of the code above, the expression passed as an argument to the `datanode` function will be evaluated and cached in the file `model_cache`. Further executions of the code will read the value from the cache and assign it to the `model` variable without evaluating the expression `lm(formula = mpg ~ wt, data = mtcars)`; which would gain time if evaluating the expression takes longer than reading the cached value from file.

In the following we define a dependency on the result cached above in the file `model_cache` by passing is to the `depends_on` paramameter:

``` r
response_cache <- tempfile()
response <- datanode(response_cache,
                     depends_on = model_cache, {
  # another potentially expensive operation
  # again, for the example this is not so expensive
  predict(model, data.frame(wt = 1:50))
})
```

The first execution of this code snippet will trigger the evaluation of the expression given as argument since the file `response_cache` does not exist.

Further in the development of our code, we decide to add an independent variable to our model. We thus edit the formula describing the model in the first code example above as in the following, setting the argument `force` to `TRUE` to force the evaluation of the expression, without which the cached value would be read from the file `model_cache`.

``` r
model <- datanode(model_cache, force = T, { 
  # this is the
  lm(formula = mpg ~ wt + hp, data = mtcars)
})
```

Afther the update of the model cache, a further execution of the repsonse code in the second block above would trigger a re-evaluation of the expression.
