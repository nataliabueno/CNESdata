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

    #Organization information (registration, president, board)
    remDrv$navigate('as.character(url.list$url[i])')
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem4']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    president.org <- readHTMLTable(remDrv$getPageSource()[[1]])
    
    list.data[[2]] <- president.org

  
    #remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')
    remDrv$navigate('as.character(url.list$url[i])') #this could be 
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem5']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    board.org <- readHTMLTable(remDrv$getPageSource()[[1]])
    
    list.data[[3]] <- board.org

    #remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')
    remDrv$navigate('as.character(url.list$url[i])')
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem20']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    budget.year.org <- readHTMLTable(remDrv$getPageSource()[[1]])
    
    list.data[[4]] <- budget.year.org 
    
    remDrv$navigate('as.character(url.list$url[i])')
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())#change
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem20']")$clickElement()#change
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    sources.org <- readHTMLTable(remDrv$getPageSource()[[1]])
    
    list.data[[5]] <- sources.org#change
    
    #More work here (parcerias)
    remDrv$navigate('as.character(url.list$url[i])')
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())#change
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem20']")$clickElement()#change
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    parcerias.org <- readHTMLTable(remDrv$getPageSource()[[1]])
    
    list.data[[6]] <- parcerias.org

  }

  remDr$closeServer()

 output(base.cnes)
}

################Don't mind this

# checkForServer()

# startServer()
# remDrv <- remoteDriver()
# remDrv$open()
# remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')
# remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
# remDrv$findElement(using = "xpath", "//div[@id = 'menuItem4']")$clickElement()
# iframe <- remDrv$findElement(using = "xpath", "//iframe")
# remDrv$switchToFrame(iframe)
# readHTMLTable(remDrv$getPageSource()[[1]])

# remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')
# remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
# remDrv$findElement(using = "xpath", "//div[@id = 'menuItem5']")$clickElement()
# iframe <- remDrv$findElement(using = "xpath", "//iframe")
# remDrv$switchToFrame(iframe)
# readHTMLTable(remDrv$getPageSource()[[1]])

# remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')
# remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())
# remDrv$findElement(using = "xpath", "//div[@id = 'menuItem20']")$clickElement()
# iframe <- remDrv$findElement(using = "xpath", "//iframe")
# remDrv$switchToFrame(iframe)
# readHTMLTable(remDrv$getPageSource()[[1]])


# we <- remDrv$findElements(using = "xpath", "//table[@class = 'formulario']")


# class(we)
# ?remoteDriver
# unlist(we)
# we[[1]]
# class(we)
# class(we[[1]])
# length(we[[1]])

# $getElementText()

# ?webElement
# nome <- xpathApply(doc, "//table/tbody/tr/td/table/tbody/tr/td", xmlValue)
# nome <- xpathApply(doc, "//html", xmlValue)
# remDrv$executeScript


# webElem <- remDrv$findElements(using = "xpath", "//img[@id = 'image1']")
# webElem$getElementText()  # Works
# location<-botao$getElementLocation()[c("x", "y")]
# remDrv$mouseMoveToLocation( x = location[1], y = location[2], webElement =botao)
# botao$buttondown()


# <div id="menuItemHilite4" style="position: absolute; left: 4px; top: 2px; color: rgb(51, 102, 153); visibility: hidden;">Estatuto&nbsp;/&nbsp;Diretoria&nbsp;</div>

#   remDrv$getStatus()$build

# remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')

# http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/18629410/RelatorioCircunstanciado.html


# remDrv$navigate("http://www.google.com/ncr")
# webElem <- remDrv$findElement("css selector", "img#hplogo")
# remDrv$executeScript("return document.getElementById('hplogo').hidden;", args = list())


# ?remoteDriver
# class(remDrv)
