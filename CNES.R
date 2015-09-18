library(RCurl)
library(XML)
library(RSelenium)

source("~/Dropbox/CNES/CNES_functions.R")

start.time <- Sys.time()
test <- getLinksCNEs(86099,86200)
end.time <- Sys.time()
time.elapsed <- end.time - start.time


load("~/Dropbox/CNES/temp.Rda")
url.list <- test
#needs RSelenium manual activation in library

#Transform into matrix
url.list <- as.matrix(url.list)


#Setting server up
checkForServer()
startServer()
remDrv <- remoteDriver()
remDrv$open()

#Getting data
cnes.data(url.list=url.list)

#Closing server
#remDr$closeServer()


  
