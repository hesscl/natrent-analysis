
#first compile the data
source("./compile.R")

#next create some descriptives
source("./describe.R")

#finally, estimate and provide output from models
rmarkdown::render("./model.Rmd")