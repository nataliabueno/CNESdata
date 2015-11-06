##########################################################
#1. Scrap links (OK) 
#2. Scrap president (OK) and board information (OK)
#3. Scrap partnerships (OK) and details (OK)
#4. Scrap assets (OK)
#5. Scrap year budget
#6. Scrap sources (OK)
##########################################################

source("~/Dropbox/CNES_temp/CNES_functions_alternativev2.R") #requires packages RCurl, XLM, rvest, httr

library(XML)
library(rvest)
library(httr)
library(foreach)
#library(doParallel)
#library(iterators)
#library(doMC)

#set number of clusters
#cl <- makeCluster(2)
#registerDoMC(2)

############################################################ Links

#A thousand batches of one thousand links

nums = seq(10000, 1000000)
sub.ids = split(nums, cut(nums, 1000))

for (j in 1:1000){
  
  ids = sub.ids[[j]]
  print(j)
  
  results <- foreach(i = ids, .packages=c('rvest','XML','httr'), .combine = rbind) %dopar% {
    
    getLinksCNEs(i)
    
  }
  save(results, file=paste("~/Dropbox/CNES_temp/links/combined", j, ".Rda", sep=""))
}

#Binding all data
dir <- ("~/Dropbox/CNES_temp/links/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- results
}

links <- do.call(rbind, all)
#save(links, file="~/Dropbox/CNES_temp/links_full.Rda")

###########################################################Directors

load("~/Dropbox/CNES_temp/final/links_full.Rda")

nums <- seq(1, nrow(links))
sub.ids = split(nums, cut(nums, 1000))

for (j in 1:1000){
  
  ids = sub.ids[[j]]
  print(j) 
  
  temp <- links[ids,]
  boardf <- cnes.board(url.list=temp)

  save(boardf, file=paste("~/Dropbox/CNES_temp/board/board", 
                            j, ".Rda", sep=""))
}


#Binding all data
dir <- ("~/Dropbox/CNES_temp/board/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- boardf
}

boards.all <- do.call(rbind, all)
save(boards.all, file="~/Dropbox/CNES_temp/final/boards_full.Rda")

########################################################### Directors

load("~/Dropbox/CNES_temp/final/links_full.Rda")

#Transform into matrix
url.list <- as.matrix(links)
#Remove empty spaces
url.list[, 4] <- gsub(" ", "", (url.list[, 4]))

nums <- seq(1, nrow(url.list))
sub.ids = split(nums, cut(nums, 1000))

for (j in 1:1000){
  
  ids = sub.ids[[j]]
  print(j) #does not work
  
  temp <- url.list[ids,]
  directors <- cnes.president(url.list=temp)
  
  save(directors, file=paste("~/Dropbox/CNES_temp/director/director", 
                             j, ".Rda", sep=""))
}

#Binding all data
dir <- ("~/Dropbox/CNES_temp/director/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- directors
}

directors.all <- do.call(rbind, all)
#save(directors.all, file="~/Dropbox/CNES_temp/final/directors_full.Rda")

########################################################### Contracts

load("~/Dropbox/CNES_temp/final/links_full.Rda")

#Transform into matrix
url.list <- as.matrix(links)
#Remove empty spaces
url.list[, 4] <- gsub(" ", "", (url.list[, 4]))

nums <- seq(1, nrow(url.list))
sub.ids = split(nums, cut(nums, 1000))


for(j in 1:1000){
  
  ids = sub.ids[[j]]
  print(j) 
  
  temp <- url.list[ids,]
  contracts <- cnes.partner(url.list=temp)
  save(contracts, file=paste("~/Dropbox/CNES_temp/partner/contracts", 
                             j, ".Rda", sep=""))
  
}

dir <- ("~/Dropbox/CNES_temp/partner/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- contracts
}

contracts.all <- do.call(rbind, all)
#save(contracts.all, file="~/Dropbox/CNES_temp/final/contracts_full.Rda")

########################################################### Contract details

load("~/Dropbox/CNES_temp/final/contracts_full.Rda")
nums <- seq(1, nrow(contracts.all))
sub.ids = split(nums, cut(nums, 1000))


for(z in 1:1000){
  
  ids = sub.ids[[z]]
  print(z) 
  
  dtemp <- contracts.all[ids,]
  contracts.details <- cnes.partner.details(url.list=dtemp)
  save(contracts.details, file=paste("~/Dropbox/CNES_temp/partner_details/contracts_details", 
                             z, ".Rda", sep=""))
  
}

dir <- ("~/Dropbox/CNES_temp/partner_details/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- contracts.details
}

contracts.details.all <- do.call(rbind, all)
save(contracts.details.all, file="~/Dropbox/CNES_temp/final/contracts_details_full.Rda")


########################################################### Source 

load("~/Dropbox/CNES_temp/final/links_full.Rda")
nums <- seq(1, nrow(links))
sub.ids = split(nums, cut(nums, 1000))


for(z in 1:1000){
  
  ids = sub.ids[[z]]
  print(z) 
  
  dtemp <- links[ids,]
  sourcef <- cnes.source(url.list=dtemp)
  save(sourcef, file=paste("~/Dropbox/CNES_temp/sources/sourcef", 
                                     z, ".Rda", sep=""))
  
}

dir <- ("~/Dropbox/CNES_temp/sources/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- sourcef
}

sources.all <- do.call(rbind, all)
save(sources.all, file="~/Dropbox/CNES_temp/final/sources_full.Rda")


########################################################### Budgets

load("~/Dropbox/CNES_temp/final/links_full.Rda")
nums <- seq(1, nrow(links))
sub.ids = split(nums, cut(nums, 1000))


for(z in 1:1000){
  
  ids = sub.ids[[z]]
  print(z) 
  
  dtemp <- links[ids,]
  assets <- cnes.budget(url.list=dtemp)
  save(assets, file=paste("~/Dropbox/CNES_temp/assets/assets", 
                           z, ".Rda", sep=""))
  
}

dir <- ("~/Dropbox/CNES_temp/assets/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- assets
}

assets.all <- do.call(rbind, all)
save(assets.all, file="~/Dropbox/CNES_temp/final/assets_full.Rda")

############################################################ Budget

load("~/Dropbox/CNES_temp/final/links_full.Rda")
nums <- seq(1, nrow(links))
sub.ids = split(nums, cut(nums, 1000))


for(z in 1:1000){
  
  ids = sub.ids[[z]]
  print(z) 
  
  dtemp <- links[ids,]
  budget <- cnes.budget(url.list=dtemp)
  save(budget, file=paste("~/Dropbox/CNES_temp/budget/budget", 
                          z, ".Rda", sep=""))
  
}

dir <- ("~/Dropbox/CNES_temp/budget/")
files <- list.files(dir)
all <- list()
for (k in 1:length(files)){
  load(paste(dir, files[k], sep="")) 
  all[[k]] <- budget
}

budget.all <- do.call(rbind, all)
save(budget.all, file="~/Dropbox/CNES_temp/final/budget_full.Rda")


