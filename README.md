# samples_archive
Data archive with samples data available and doctor information

- Using the code inside `ccrd` directory you can create the `docs` directory to deploy the website:
```R
library(shinylive)
library(httpuv)

setwd("~/path/to/samples_archive")
shinylive::export(appdir = "ccrd", destdir = "docs")
httpuv::runStaticServer("docs/", port=8008)
```


https://uclanelsonlab.github.io/samples_archive/
