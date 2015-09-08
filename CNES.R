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

getLinksCNEs(86099,87000)


#Para os 902 numeros acima, 180 links. Aparentemente funciona super bem. Rode isso num servidores e talvez por partes que vai funcionar.


checkForServer()

startServer()
remDrv <- remoteDriver()
remDrv$open()
remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')
remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
remDrv$findElement(using = "xpath", "//div[@id = 'menuItem4']")$clickElement()
iframe <- remDrv$findElement(using = "xpath", "//iframe")
remDrv$switchToFrame(iframe)
readHTMLTable(remDrv$getPageSource()[[1]])

remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')
remDrv$executeScript("MM_showMenu(window.mm_menu_1027153841_0, 0, 16, null, 'image1')", args = list())
remDrv$findElement(using = "xpath", "//div[@id = 'menuItem5']")$clickElement()
iframe <- remDrv$findElement(using = "xpath", "//iframe")
remDrv$switchToFrame(iframe)
readHTMLTable(remDrv$getPageSource()[[1]])

remDrv$navigate('http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/122018/RelatorioCircunstanciado.html')
remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())
remDrv$findElement(using = "xpath", "//div[@id = 'menuItem20']")$clickElement()
iframe <- remDrv$findElement(using = "xpath", "//iframe")
remDrv$switchToFrame(iframe)
readHTMLTable(remDrv$getPageSource()[[1]])

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
