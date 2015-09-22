##########################################################
#0. Get links
#1. Open server
#2. Scrap president and board infor
#3. Scrap assets
#4. Scrap year budget
#5. Scrap sources
#6. Scrap partnerships and details 
##########################################################


source("~/Dropbox/CNES/CNES_functions.R") #requires packages RSelenium, RCurl, XLM

start.time <- Sys.time()
links.fivedigits <- getLinksCNEs(00000,99999) #set range of digit
end.time <- Sys.time()
time.elapsed <- end.time - start.time
save(links.fivedigits, "~/Dropbox/CNES/fivedigits.Rda")

#load("~/Dropbox/CNES/temp.Rda")
#url.list <- test
#needs RSelenium manual activation in library

#Transform into matrix
url.list <- as.matrix(links.fivedigits)
#url.list <- url.list[13:16, ] #testing functions

#Setting server up
checkForServer()
startServer()
remDrv <- remoteDriver()
remDrv$open()

#Getting data
boards.test <- cnes.board(url.list=url.list) 
assets.test <- cnes.assets(url.list=url.list) 
budget.test <- cnes.budget(url.list=url.list)
source.test <- cnes.source(url.list=url.list) 
partner.test <- cnes.partner(url.list=url.list)


#Closing server
remDrv$closeServer()


  
