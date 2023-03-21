# Setup

For the whole analysis, you will need the following R packages

```r
install.packages(c("combinat", "igraph", "lsa", "purrr", "quanteda", "rio", "seededlda", 
"spacyr", "stm", "tibble", "keyATM", "brms", "tidyverse", "psych", 
"rmarkdown", "papaja", "parameters", "knitr", "here", "irr"))

## setup spacyr
require(spacyr)
spacy_install()

## These are needed for the pressos
## c("chainsawriot/mzesalike" "xaringanExtra" "xaringan", "shiny")
## Will certainly migrate them to quarto using gesiscss/quarto-revealjs-fakegesis
```

If you just want to render the article:

```r
install.packages(c("papaja", "tidyverse", "brms", "knitr", "rio"))
rmarkdown::render('frame_ica_ea.rmd') ## or use the Makefile: make render
```
