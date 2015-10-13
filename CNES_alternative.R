##########################################################
#0. Get links
#1. Open server
#2. Scrap president and board infor
#3. Scrap assets
#4. Scrap year budget
#5. Scrap sources
#6. Scrap partnerships and details 
##########################################################


source("~/Dropbox/CNES/CNES_functions.R") #requires packages RCurl, XLM, rvest, httr


start.time <- Sys.time()
temp2 <- getLinksCNEs(14000,14050) 
end.time <- Sys.time()
time.elapsed <- end.time - start.time
save(temp2, "~/Dropbox/CNES/temp2.Rda")

load("~/Dropbox/CNES/temp.Rda")
load("~/Dropbox/CNES/temp2.Rda")
url.list <- test


#Transform into matrix
url.list <- as.matrix(url.list)
url.list <- url.list[13:16, ] #testing functions
url.list <- cbind(url.list, c("86130", "86131", "86135", "86136"))

#Getting data
boards.test <- cnes.board(url.list=url.list) 
assets.test <- cnes.assets(url.list=url.list) 
budget.test <- cnes.budget(url.list=url.list)
source.test <- cnes.source(url.list=url.list) 
partner.test <- cnes.partner(url.list=url.list)




  
