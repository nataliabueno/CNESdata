##########################################################
#0. Get links
#1. Open server
#2. Scrap president and board infor
#3. Scrap assets
#4. Scrap year budget
#5. Scrap sources
#6. Scrap partnerships and details 
##########################################################


source("~/Dropbox/CNES/CNES_functions_alternative.R") #requires packages RCurl, XLM, rvest, httr


start.time <- Sys.time()
temp3 <- getLinksCNEs(14000,14500) 
end.time <- Sys.time()
time.elapsed <- end.time - start.time
save(temp3, file="~/Dropbox/CNES/temp3.Rda")

(time.elapsed*(999999/500)) # time estimate

#load("~/Dropbox/CNES/temp.Rda")
#load("~/Dropbox/CNES/temp3.Rda")

#Transform into matrix
url.list <- as.matrix(temp3)

#Getting data
boards.test <- cnes.board(url.list=url.list) 
assets.test <- cnes.assets(url.list=url.list) 
budget.test <- cnes.budget(url.list=url.list)
source.test <- cnes.source(url.list=url.list) 
partner.test <- cnes.partner(url.list=url.list)

