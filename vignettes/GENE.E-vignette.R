
## @knitr setup, echo=FALSE, warn=FALSE
library(knitr)
options(width=80)


## @knitr wrap-hook, echo=FALSE
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) {
  # this hook is used only when the linewidth option is not NULL
  if (!is.null(n <- options$linewidth)) {
    x = knitr:::split_lines(x)
    # any lines wider than n should be wrapped
    if (any(nchar(x) > n)) x = strwrap(x, width = n)
    x = paste(x, collapse = '\n')
  }
  hook_output(x, options)
})


## @knitr 
library(GENE.E) 
library(golubEsets) 
data(Golub_Merge) 
to.genee(exprs(Golub_Merge), fData(Golub_Merge), show.colnames=F, pData(Golub_Merge)[,c('Gender', 'ALL.AML')], column.hclust=hclust(dist(t(exprs(Golub_Merge)))))


## @knitr 
selection <- from.genee()


