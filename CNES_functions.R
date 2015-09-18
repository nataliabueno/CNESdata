library(RCurl)
library(XML)
library(RSelenium)


getLinksCNEs <- function(num.inicial, num.final){
  require(RCurl)
  require(XML)
  u <- "http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/PLACEHOLDER/RelatorioCircunstanciado.html"
  data.out <- data.frame()
  for (i in num.inicial:num.final){
    print(i)
    url <- gsub("PLACEHOLDER", i, u)
    erro <- try(readHTMLTable(url), silent=TRUE)
    if (!('try-error' %in% class(erro))){
      tabela <- readHTMLTable(url, stringsAsFactors = F)[[2]]
      cnpj <- names(tabela)[2]
      ano <- as.numeric(tabela[2,2])
      data.out <- rbind(data.out, data.frame(cnpj, ano, url))
    }
  }
  return(data.out)
}


cnes.board <- function(url.list=url.list){
  
  #getting listing of organizations' urls
  for (i in 1:nrow(url.list)){
    
    print(i)
    #identifying organization
    data1 <- matrix(NA, nrow(url.list), 20)
    colnames(data1) <- c("cnpj", "ano", "main_url", "sede", "UF", "mun", "Cartorio", "date_reg", "book_page", "nr_register", 
                         "change_previous", "date_current_begin", "date_current_end", "name", "occupation", "position", 
                         "gender", "public_employee", "paid", "which_paid")
     data1[i, 1:3] <- url.list[i,1:3]
    
    #President
    remDrv$navigate(url.list[i,3])
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem4']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    raw <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")$`NULL`
    data1[i, 4] <- as.character(raw$V2[4])#sede
    data1[i, 5] <- as.character(raw$V2[6])#UF
    data1[i, 6] <- as.character(raw$V4[6])#mun
    data1[i, 7] <- as.character(raw$V2[7])#cartorio
    data1[i, 8] <- as.character(raw$V2[6])#data_reg
    data1[i, 9] <- as.character(raw$V4[8])#book_page
    data1[i, 10] <- as.character(raw$V2[9])#nr_reg
    data1[i, 11] <- as.character(raw$V2[11])#change_previous
    data1[i, 12] <- as.character(raw$V2[13])#data_current_begin
    data1[i, 13] <- as.character(raw$V4[13])#data_current_end
    data1[i, 14] <- as.character(raw$V2[14])#name
    data1[i, 15] <- as.character(raw$V2[15])#occupation
    data1[i, 16] <- as.character(raw$V2[16])#position
    data1[i, 17] <- as.character(raw$V2[17])#gender
    data1[i, 18] <- as.character(raw$V2[18])#public_employee
    data1[i, 19] <- as.character(raw$V2[19])#paid
    data1[i, 20] <- as.character(raw$V2[20])#which_paid
  
    
    #Board
    remDrv$navigate(url.list[i,3])
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem5']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    raw <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")$`NULL`
    
    #identifying organization
    temp <- matrix(NA, length(raw$V1)-2, 5)
    colnames(temp) <- c("cnpj", "ano", "name", "position", "public_employee")
    temp[, 1] <- url.list[i, 1]
    temp[, 2] <- url.list[i, 2]
    temp[, 3] <-  as.character(raw$V1[3:length(raw$V1)])
    
  }
  
  
  
  
  
  
  
  
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
    
    save(list.data, list.partner, "cnes.RData")
  }
  
  output(cnes.RData)
}






