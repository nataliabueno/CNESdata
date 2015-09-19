##########################################################
#0. Open server
#1. Get links
#2. Scrap president and board infor
#3. Scrap assets
#4. Scrap year budget
#5. Scrap sources
#6. Scrap partnerships and details
##########################################################


source("~/Dropbox/CNES/CNES_functions.R") #requires packages RSelenium, RCurl, XLM

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
boards.test <- cnes.board(url.list=url.list)
assets.test <- cnes.assets(url.list=url.list)




#Closing server
remDrv$closeServer()


  
