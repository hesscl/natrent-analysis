
#this is a simple script that executes each of the project scripts
#in sequence such that:
#the data are created, 
#followed by descriptive maps
#followed by models and model output

#first compile the data
source("./compile.R", local = F)

#next create some descriptives
source("./map.R", local = F)

#finally, estimate and provide output from models
source("./model.R", local = F)