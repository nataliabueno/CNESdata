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


