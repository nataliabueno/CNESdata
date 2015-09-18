library(RCurl)
library(XML)
library(RSelenium)

source("~/Dropbox/CNES/CNES_functions.R")

start.time <- Sys.time()
test <- getLinksCNEs(86099,86200)
end.time <- Sys.time()
time.elapsed <- end.time - start.time
#41 secs for 31 links (first time)
#59 secs for 31 links (second time)

#save(test, file="~/Dropbox/CNES/temp.Rda")
load("~/Dropbox/CNES/temp.Rda")
url.list <- test

cnes.data <- function(url.list=url.list, org.info=T, conv.info=T, assets.info=T, budget.info=T)

  list.data <- list()

  checkForServer()
  startServer()
  remDrv <- remoteDriver()
  remDrv$open()

  #getting listing of organizations' urls
  #for (i in 1:nrow(url.list)){
    
    i <- 1 #temp
    #identifying organization
    list.data[[1]] <- url.list[i, ]

    #President
    remDrv$navigate(as.character(url.list$url[i]))
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem4']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    list.data[[2]] <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")
  

    #Board
    remDrv$navigate(as.character(url.list$url[i])) #this could be 
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem5']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    list.data[[3]] <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")
    

    #Assets
    remDrv$navigate(as.character(url.list$url[i]))
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem20']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    list.data[[4]] <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")
     
    
    #Yearly Budget
    remDrv$navigate(as.character(url.list$url[i]))
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem21']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    list.data[[5]] <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")
    
    
    #Sources 
    remDrv$navigate(as.character(url.list$url[i]))
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160446_0,0,16,null,'image2')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem13']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    list.data[[6]] <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")


    #Partnerships
    remDrv$navigate(as.character(url.list$url[i]))
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160446_0,0,16,null,'image2')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem14']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    list.data[[7]]  <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")[2]
    
    #Details on partnerships
    list.partner <- list()
    nr.p <- dim(list.data[[7]]$`NULL`)[1]-1 #getting number of partnerships
      for (j in 1:nr.p){
        plink <- sub("RelatorioCircunstanciado",paste("ParceriasSubvencoesPublicas", j, sep=""), url.list$url[i])
        remDrv$navigate(plink)
        list.partner[[j]]  <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")
      }
    

  }

  remDr$closeServer()

 output(list.data, list.partner)
}

