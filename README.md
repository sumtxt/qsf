# qsf

Qualtrics Survey File (QSF) editing and construction in R.

A QSF is a `json` file that can is represented in R as a nested list. For an introduction to Qualtrics' QSF files see Christian Testa's [Quickstart Guide to understanding the Qualtrics Survey File](https://gist.github.com/ctesta01/d4255959dace01431fb90618d1e8c241).


### Install 

```R
remotes::install_github('sumtxt/qsf', build_vignettes=TRUE)
```

Call `vignette("using-qsf")` after installation to see some simple examples on how to use this package. 


### Other packages 

* [QualtricsTools](https://github.com/emma-morgan/QualtricsTools) automatically process Qualtrics survey data into reports. 

* [qualtRics](https://github.com/ropensci/qualtRics/) provides functions to download survey data directly into R using the 'Qualtrics' API.



