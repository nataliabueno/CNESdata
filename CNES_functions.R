##########################################################
#Function to get CNES links OK
#Function to scrap president and board information OK
#Function to scrap assets OK
#Function to scrap year budget OK
#Function to scrap sources OK
#Function to scrap partnerships and partnerships details
##########################################################


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
  
  #identifying organization
  data1 <- matrix(NA, nrow(url.list), 20)
  colnames(data1) <- c("cnpj", "ano", "main_url", "sede", "UF", "mun", "Cartorio", "date_reg", "book_page", "nr_register", 
                       "change_previous", "date_current_begin", "date_current_end", "name", "occupation", "position", 
                       "gender", "public_employee", "paid", "which_paid")
  ltemp <- list()
  
  #getting listing of organizations' urls
  for (i in 1:nrow(url.list)){

    print(i)
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
    temp[, 4] <-  as.character(raw$V2[3:length(raw$V2)])
    temp[, 5] <-  as.character(raw$V3[3:length(raw$V3)])
    ltemp[[i]] <- temp
    
  }
 
  data2 <- do.call(rbind, ltemp)
  return(list(data1, data2))
}  
  
  
cnes.assets <- function(url.list=url.list){

    #identifying organization
    data3 <- matrix(NA, nrow(url.list), 96)
    colnames(data3) <- c("cnpj", "ano", "main_url", "Prestação de Serviços (Exceto Saúde/Educação)_rec",
                         "Recursos - Subvenções Públicas_rec", "Recursos - Contribuições Públicas_rec",
                         "Recursos - Convênios Públicos_rec","Recursos - Auxílios Públicos_rec", 
                         "Recursos - Termo de Parceria_rec", "Doações e Contribuições para Custeio_rec",
                         "Receita de Convênios de Saúde Privados_rec", "Prest. Serviços de Saúde não Conveniados_rec",
                         "SUS - Sistema Único de Saúde_rec", "Inscrições de Cursos e Vestibulares_rec", 
                         "Serviços Educacionais_rec", "Taxa, Mensalidades e Contribuições_rec",
                         "Contribuição de Empresas Mantenedoras_rec", "Doações, Campanhas e Patrocínios_rec",
                         "Recusos Internacionais_rec", "(-) Bolsas de Estudo Concedidas_rec",
                         "(-) Atendimento Gratuito_rec", "(-) Descontos Comerciais Concedidos_rec", "(-) PIS sobre Receitas_rec",
                         "(-) COFINS sobre Receitas_rec", "(-) ICMS sobre Vendas_rec", "(-) ISS sobre Serviços_rec",
                         "(-) Vendas Canceladas_rec", "(-) Outras Deduções_rec","Outras Receitas Operacionais_rec",
                         "Descontos Obtidos_rec", "Renda de Aluguéis e Arrendamentos_rec", "Rendimentos de Títulos e Aplicações no Mercado Financeiro_rec",  
                         "(-) Impostos s/ aplicações Financeiras_rec", "Outras Receitas Financeiras_rec", "Venda de Ativo Permanente_rec",                                
                         "Doações receb. em bens ou mercadorias_rec", "Outras Receitas Não-Operacionais_rec",                         
                         "Outras receitas não classificadas anteriormente_rec", "TOTAL RECEITAS_rec",
                         "Salários de Funcionários (c/ vínculo empregatício)_desp", "Encargos Sociais com Pessoal_desp",                      
                         "Despesas Diversas com Pessoal_desp",  "Remuneração de Dirigentes_desp",                         
                         "Encargos Sociais com Dirigentes_desp",  "Outros Encargos Sociais Compulsórios_desp",              
                         "Outras Despesas com Pessoal_desp",  "Recursos Humanos Externos - Pessoa Física_desp",         
                         "Recursos Humanos Externos - Pessoa Jurídica_desp", "INSS sobre Serviços Prestados por Terceiros_desp",       
                         "Outras Despesas com Serviços Contratados_desp", "Custos de Projetos_desp",                                
                         "Água, Gás e Energia Elétrica_desp",  "Aluguéis Pagos_desp",                                    
                         "Despesas com Veículos_desp", "Diárias e Viagens_desp",                                 
                         "Hospedagem_desp",    "Passagens Aéreas/Rodoviárias_desp",                      
                         "Telefone, Fax e Outras Despesas com Comunicações_desp", "Publicações Técnicas_desp",                              
                         "Serviços Técnicos e Especializados_desp",   "Despesas com Informática_desp",                          
                         "Prêmios de Seguros Contratados_desp",  "Despesas com Atividades Sociais e Culturais_desp",       
                         "Outras Despesas Administrativas_desp",  "Ensino Fundamental_desp",                                
                         "Curso Superior_desp",  "Estagiários_desp",                                       
                         "Mestrados, Doutorados e Pós-Doutorados_desp", "Outras despesas com Bolsas de Estudo_desp",              
                         "Impostos Federais_desp",   "Impostos Estaduais_desp",                                
                         "Impostos Municipais_desp", "CPMF_desp",                                              
                         "COFINS_desp", "IOF_desp",                                               
                         "Outros Tributos, Taxas e Contribuições_desp",  "Doação de Alimentos_desp",                               
                         "Doação de Roupas e Agasalhos_desp", "Doação de Medicamentos_desp",                            
                         "Outras Despesas Beneficentes_desp", "Descontos Concedidos_desp",                              
                         "Despesas Bancárias_desp", "Outras Despesas Financeiras_desp",                       
                         "Despesas com Depreciação_desp", "Despesas com Amortização_desp",                          
                         "Despesas com Leasing_desp", "(-) Recuperação de Despesas_desp",                       
                         "Outras despesas Operacionais_desp", "Custo de Ativo Permanente Vendido_desp",                 
                         "Custo de Ativo Permanente Baixado_desp", "Outras Despesas Não Operacionais_desp",                  
                         "Participações e Contribuições_desp", "Outras Despesas Não Classificadas Anteriormente_desp",   
                         "Provisão para Imposto de Renda e CSLL_desp", "Outras Provisões Constituídas_desp",                     
                         "TOTAL DE DESPESAS_desp")
    
    for (i in 1:nrow(url.list)){   
      print(i)
      data3[i, 1:3] <- url.list[i,1:3]
      
      #Assets
      remDrv$navigate(url.list[i,3])
      remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())
      remDrv$findElement(using = "xpath", "//div[@id = 'menuItem20']")$clickElement()
      iframe <- remDrv$findElement(using = "xpath", "//iframe")
      remDrv$switchToFrame(iframe)
      raw <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")$`NULL`
      data3[i, 4:ncol(data3)] <- as.character(raw$V2[-c(1, 2, 3, 20, 30, 32, 38, 42, 45, 46, 
                                           54, 59, 61, 75, 81, 89, 94, 98, 102,
                                            105, 109, 111, 113)])
    }
    return(data3)
}    
    

cnes.budget <- function(url.list=url.list){
  
  data4 <- matrix(NA, nrow(url.list), 22)
  colnames(data4) <- c("cnpj", "ano", "main_url", "RECEITA", "(-) Imposto Sobre a Receita",                    
                       "(-) Abatimentos e Cancelamentos", "RECEITA LÍQUIDA",                                
                       "(-) Custo de Serviços e Produtos", "SUPERÁVIT OU DÉFICIT BRUTO",                     
                       "(-) Despesas Gerais e Administrativas", "(-) Despesas Financeiras",                       
                       "(-) Despesas Tributárias", "(-) Outras Despesas Operacionais",               
                       "Receitas Financeiras",  "RESULTADO OPERACIONAL",                          
                       "(-) Despesas Não-Operacionais", "Receitas Não-Operacionais",                      
                       "RESULTADOS DO EXERCÍCIO ANTES DE IRenda E CSLL",  "(-) Provisão para IRenda e CSLL",                
                       "RESULTADOS DO EXERCÍCIO DEPOIS DE IRenda E CSLL", "(-) Participações e Contribuições",              
                       "SUPERÁVIT OU DÉFICIT DO EXERCÍCIO")
  
  for (i in 1:nrow(url.list)){   
    print(i)
    data4[i, 1:3] <- url.list[i,1:3]
    
    #Yearly Budget
    remDrv$navigate(url.list[i,3])
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160934_0,0,16,null,'image4')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem21']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    raw <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")$`NULL`
    data4[i, 4:ncol(data4)] <- as.character(raw$V2[-1])
  
  }
  return(data4)
}  


cnes.source <- function(url.list=url.list){
  
  data5 <- matrix(NA, nrow(url.list), 10)
  colnames(data5) <- c("cnpj", "ano", "main_url", "Própria (recursos decorrentes da prestação de serviços da entidade)",
                       "Própria (recursos decorrentes de mensalidades / doações dos membros ou associados)",      
                       "Privada (recursos de doações e parcerias com empresas e entidades privadas)",
                       "Privada (recursos de doações eventuais)",
                       "Pública (recursos de subvenções, convênios e parcerias com órgãos ou entidades públicas)",
                       "Internacional Privada (recursos de entidades e organizações internacionais)",
                       "Internacional Pública (recursos de países estrangeiros, ONU, etc.)") 
  
  for (i in 1:nrow(url.list)){   
    print(i)
    data5[i, 1:3] <- url.list[i,1:3]
    
    #Sources 
    remDrv$navigate(url.list[i,3])
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160446_0,0,16,null,'image2')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem13']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    raw <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")$`NULL`
    data5[i, 4:ncol(data5)] <- as.character(raw$V2[-c(1,2)])
  }
  return(data5) 
}


cnes.partner(url.list=url.list){    
     
  ltemp <- list()
  
  for (i in 1:nrow(url.list)){
    print(i)
    
    #Partnerships
    remDrv$navigate(url.list[i,3])
    remDrv$executeScript("MM_showMenu(window.mm_menu_1027160446_0,0,16,null,'image2')", args = list())
    remDrv$findElement(using = "xpath", "//div[@id = 'menuItem14']")$clickElement()
    iframe <- remDrv$findElement(using = "xpath", "//iframe")
    remDrv$switchToFrame(iframe)
    raw  <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")$`NULL`
    
    temp <- matrix(NA, length(raw$V1)-2, 5)
    colnames(temp) <- c("cnpj", "ano", "Nome Órgão_Entidade", 
                        "Natureza do Instrumento", "Posição na Estrutura Federativa")
    temp[, 1] <- url.list[i, 1]
    temp[, 2] <- url.list[i, 2]
    temp[, 3] <-  as.character(raw$V1[3:length(raw$V1)])
    temp[, 4] <-  as.character(raw$V2[3:length(raw$V2)])
    temp[, 5] <-  as.character(raw$V3[3:length(raw$V3)])
    ltemp[[i]] <- temp
    
  }
  
  data6 <- do.call(rbind, ltemp)
  
  #Details on partnerships
  nr.p <- nrow(ltemp[[i]]) #getting number of partnerships
  
  for (j in 1:nr.p){
    plink <- paste(' ParceriasSubvencoesPublicas', j, '.html \']', sep="")
    remDrv$findElement(using = 'xpath', plink)$clickElement()
    remDrv$findElement(using = 'xpath', "//a[@href = ' ParceriasSubvencoesPublicas2.html ']")$clickElement()
    raw1  <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")$`NULL`
    temp <- matrix(NA, 1, 12)
    colnames(temp) <- c("Nome do Órgão ou Entidade de Parceria", "Classificação do órgão na estrutura administrativa",
                        "Posição do órgão na estrutura federativa", "Origem dos recursos repassados",
                        "Natureza do instrumento de parceria", "Data de publicação na imprensa oficial",
                        "Total de recursos financeiros previstos", "Recursos financeiros já repassados", 
                        "Nº de  Beneficiários", "Previsão de início das atividades", 
                        "Previsão de término das atividades", "Resumo do objetivo da parceria")
    temp[1,1] <-  as.character(raw1$V1[3])
    temp[1,2] <-  as.character(raw1$V1[5])
    temp[1,3] <-  as.character(raw1$V1[7])
    temp[1,4] <-  as.character(raw1$V1[9])
    temp[1,5] <-  as.character(raw1$V1[11])
    temp[1,6] <-  as.character(raw1$V1[13])
    temp[1,7] <-  as.character(raw1$V1[15])
    temp[1,8] <-  as.character(raw1$V1[17])
    temp[1,9] <-  as.character(raw1$V1[19])
    temp[1,10] <-  as.character(raw1$V1[21])
    temp[1,11] <-  as.character(raw1$V1[23])
    temp[1,11] <-  as.character(raw1$V1[25])
    lpartner[[i]] <- temp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
                  
                        
                        
                        
                        
    #plink <- sub("RelatorioCircunstanciado.html", paste(" ParceriasSubvencoesPublicas", j, ".html ", sep=""), url.list[j,3])
    remDrv$navigate(plink)
    raw  <- readHTMLTable(remDrv$getPageSource()[[1]], encoding = "UTF-8")
    temp <- matrix(NA, length(raw$V1)-2, 5)
    colnames(temp) <- c("cnpj", "ano", )
    temp[, 1] <- url.list[i, 1]
    temp[, 2] <- url.list[i, 2]
    temp[, 3] <-  as.character(raw$V1[3:length(raw$V1)])
    temp[, 4] <-  as.character(raw$V2[3:length(raw$V2)])
    temp[, 5] <-  as.character(raw$V3[3:length(raw$V3)])
    lpartner[[i]] <- temp
  }
  
  data7 <- do.call(rbind, lpartner)
  return(list(data6, data7))
}  
    
  



