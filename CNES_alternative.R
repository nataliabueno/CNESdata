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

library(XML)
library(rvest)
library(httr)
library(foreach)
library(doParallel)

#set number of clusters
cl <- makeCluster(5)
registerDoParallel(cl)

#A thousand batches of one thousand links

nums = seq(10000, 1000000)
sub.ids = split(nums, cut(nums, 1000))

for (j in 1:1000){
  
  ids = sub.ids[[j]]
  print(j)
  
  results <- foreach(i = ids, .packages=c('rvest','XML','httr'), .combine = rbind) %dopar% {
    
    getLinksCNEs(i)
    
  }
  save(results, file=paste("~/Dropbox/CNES_temp/links/combined_b", j, ".Rda", sep=""))
}


stopCluster(cl)

############################################################

#Binding all data
dir <- ("~/Dropbox/CNES_temp/links/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- results
}

links <- do.call(rbind, all)
save(links, file="~/Dropbox/CNES_temp/links_full.Rda")

###########################################################

#Transform into matrix
url.list <- as.matrix(temp3)

#Getting data
boards.test <- cnes.board(url.list=url.list) 
assets.test <- cnes.assets(url.list=url.list) 
budget.test <- cnes.budget(url.list=url.list)
source.test <- cnes.source(url.list=url.list) 
partner.test <- cnes.partner(url.list=url.list)

