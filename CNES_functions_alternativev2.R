##########################################################
#Function to get CNES links OK OK
#Function to board information OK OK
#Function to scrap president information OK OK
#Function to scrap assets OK OK
#Function to scrap year budget OK OK 
#Function to scrap sources OK OK
#Function to scrap partnerships OK OK
#Function to scrap partnerships details OK OK 
##########################################################

library(XML)
library(rvest)
library(httr)
library(foreach)
library(doParallel)

getLinksCNEs <- function(num){
  require(XML)
  u <- "http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/PLACEHOLDER/RelatorioCircunstanciado.html"
  data.out <- data.frame()
  url <- gsub("PLACEHOLDER", num, u)
  erro <- try(readHTMLTable(url), silent=TRUE)
  #Sys.sleep(1)
  if (!('try-error' %in% class(erro))){
    tabela <- readHTMLTable(url, stringsAsFactors = F)[[2]]
    cnpj <- names(tabela)[2]
    ano <- as.numeric(tabela[2,2])
    data.out <- rbind(data.out, data.frame(cnpj, ano, url, num))
  }
  return(data.out)
}


###############################################################################


cnes.president <- function(url.list=url.list){
  
  #identifying organization
  data1 <- matrix(NA, nrow(url.list), 18)
  colnames(data1) <- c("cnpj", "year", "main_url", "sede", "UF", "mun",
                       "Cartorio", "date_reg",
                       "change_previous", "date_current_begin", "date_current_end",
                       "name", "occupation", "position", 
                       "gender", "public_employee", "paid", "which_paid")
  ltemp <- list()
  
  #getting listing of organizations' urls
  for (i in 1:nrow(url.list)){
    print(i)
    data1[i, 1:3] <- url.list[i,1:3]
    #President
    
    Sys.sleep(sample(seq(0, 2, by=0.001), 1))
    url.p <- paste("http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/", 
                   url.list[i, 4], "/EstatutoDiretoria.html", sep="")

    page.p <- try(read_html(url.p), silent=TRUE)
        
    if (class(page.p)[1] == "try-error"){
      Sys.sleep(sample(seq(0, 2, by=0.001), 1))
      page.p <- try(read_html(url.p), silent=TRUE)
      if (class(page.p)[1] == "try-error"){
        next
      }
    }
    
    
    data1[i, 4] <- as.character(page.p %>% html_nodes("table .formulario") 
                                %>% .[[1]] %>% html_table() %>% .[2]) #sede
    data1[i, 5] <- as.character(page.p %>% html_nodes("table .formulario") 
                                %>% .[[2]] %>% html_table() %>% .[1,2])#UF
    data1[i, 6] <- as.character(page.p %>% html_nodes("table .formulario") 
                                %>% .[[2]] %>% html_table() %>% .[1,4])#mun
    data1[i, 7] <- as.character(page.p %>% html_nodes("table .formulario") 
                                %>% .[[2]] %>% html_table() %>% .[2,2])#cartorio
    data1[i, 8] <- as.character(page.p %>% html_nodes("table .formulario") 
                                %>% .[[2]] %>% html_table() %>% .[3,2])#data_reg
    data1[i, 9] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[4]] %>% html_table() %>% .[1,2])#change_previous
    data1[i, 10] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[6]] %>% html_table() %>% .[1,2])#data_current_begin
    data1[i, 11] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[6]] %>% html_table() %>% .[1,4])#data_current_end
    data1[i, 12] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[7]] %>% html_table() %>% .[1,2])#name
    data1[i, 13] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[7]] %>% html_table() %>% .[2,2])#occupation
    data1[i, 14] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[7]] %>% html_table() %>% .[3,2])#position
    data1[i, 15] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[7]] %>% html_table() %>% .[4,2])#gender
    data1[i, 16] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[7]] %>% html_table() %>% .[5,2])#public_employee
    data1[i, 17] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[8]] %>% html_table() %>% .[1,2])#paid
    data1[i, 18] <- as.character(page.p %>% html_nodes("table .formulario") 
                                 %>% .[[9]] %>% html_table() %>% .[1,2])#which_paid
  
  }

  return(data1) 
}

###############################################################################

  
cnes.board <- function(url.list=url.list){ 
  
    ltemp <- list()
    
  for (i in 1:nrow(url.list)){
    
    print(i)
    #Board
    Sys.sleep(sample(seq(0, .5, by=0.001), 1))
    url.b <- paste("http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/", 
               url.list[i, 4], "/QualificacaoDiretoria.html", sep="")
   
    page.b <- try(read_html(url.b), silent=TRUE)
  
    if (class(page.b)[1] == "try-error"){
      Sys.sleep(sample(seq(0, 1, by=0.001), 1))
      page.b <- try(read_html(url.b), silent=TRUE)
      if (class(page.b)[1] == "try-error"){
        board <- cbind(as.character(url.list[i, 1]),
                       as.character(url.list[i, 2]), NA, NA, NA)
        next
      }
    }
    
    board <- page.b %>% html_nodes("table .listagem") %>% html_table()
    
    if (length(board)==1) {
    
    board <- cbind(rep(url.list[i, 1], nrow(board[[1]])), rep(url.list[i, 2], nrow(board[[1]])),
                   board[[1]])
    colnames(board) <- c("cnpj", "year", "name", "position", "public_employee")
                       
    ltemp[[i]] <- board
    }  
  }
  data2 <- do.call(rbind, ltemp)
  return(data2)
}  
  
###############################################################################
  
cnes.budget <- function(url.list=url.list){

    #identifying organization
    data3 <- matrix(NA, nrow(url.list), 69)
    colnames(data3) <- c("cnpj", "year", "main_url", "Prestação de Serviços (Exceto Saúde/Educação)", "Recursos - Subvenções Públicas", 
                                                  "Recursos - Contribuições Públicas",  "Recursos - Convênios Públicos", 
                                                  "Recursos - Auxílios Públicos" , "Recursos - Termo de Parceria",
                                                  "Doações e Contribuições para Custeio", "Receita de Convênios de Saúde Privados",
                                                  "Prest. Serviços de Saúde não Conveniados", "SUS - Sistema Único de Saúde", 
                                                  "Inscrições de Cursos e Vestibulares",  "Serviços Educacionais", 
                                                  "Taxa, Mensalidades e Contribuições", "Contribuição de Empresas Mantenedoras", 
                                                  "Doações, Campanhas e Patrocínios",  "Recusos Internacionais", 
                                                  "(-) Bolsas de Estudo Concedidas", "(-) Atendimento Gratuito", 
                                                  "(-) Descontos Comerciais Concedidos",  "(-) PIS sobre Receitas", 
                                                  "(-) COFINS sobre Receitas", "(-) ICMS sobre Vendas", 
                                                  "(-) ISS sobre Serviços", "(-) Vendas Canceladas", 
                                                  "(-) Outras Deduções",  "Outras Receitas Operacionais", 
                                                  "Descontos Obtidos",  "Renda de Aluguéis e Arrendamentos", 
                                                  "Rendimentos de Títulos e Aplicações no Mercado Financeiro",  "(-) Impostos s/ aplicações Financeiras", 
                                                  "Outras Receitas Financeiras", "Venda de Ativo Permanente", 
                                                  "Doações receb. em bens ou mercadorias", "Outras Receitas Não-Operacionais", 
                                                  "Outras receitas não classificadas anteriormente",  "TOTAL RECEITAS", 
                                                  "Salários de Funcionários (c/ vínculo empregatício)", "Encargos Sociais com Pessoal", 
                                                  "Despesas Diversas com Pessoal",  "Remuneração de Dirigentes", 
                                                  "Encargos Sociais com Dirigentes",  "Outros Encargos Sociais Compulsórios", 
                                                  "Outras Despesas com Pessoal", "Recursos Humanos Externos - Pessoa Física", 
                                                  "Recursos Humanos Externos - Pessoa Jurídica", "INSS sobre Serviços Prestados por Terceiros", 
                                                  "Outras Despesas com Serviços Contratados", "Custos de Projetos", 
                                                  "Água, Gás e Energia Elétrica", "Aluguéis Pagos", 
                                                  "Despesas com Veículos", "Diárias e Viagens", 
                                                  "Hospedagem", "Passagens Aéreas/Rodoviárias", 
                                                  "Telefone, Fax e Outras Despesas com Comunicações", "Publicações Técnicas",
                                                  "Serviços Técnicos e Especializados",  "Despesas com Informática", 
                                                  "Prêmios de Seguros Contratados", "Despesas com Atividades Sociais e Culturais", 
                                                  "Outras Despesas Administrativas", "Ensino Fundamental", 
                                                  "Curso Superior", "Estagiários", 
                                                  "Mestrados, Doutorados e Pós-Doutorados",  "Outras despesas com Bolsas de Estudo")

    for (i in 1:nrow(url.list)){   
      print(i)
      #budget
      url.a <- paste("http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/", 
                     url.list[i, 4], "/DemontrativoReceitasDespesasExercicio.html", sep="")
      
      Sys.sleep(sample(seq(0, .5, by=0.001), 1))
      
      page.a <- try(read_html(url.a), silent=TRUE)
      
      if (class(page.a)[1] == "try-error"){
        Sys.sleep(sample(seq(0, .5, by=0.001), 1))
        page.a <- try(read_html(url.a), silent=TRUE)
        if (class(page.a)[1] == "try-error"){
          next
        }
      }
        
      
      receitas <- page.a %>% html_nodes("table .listagem") %>% html_table()  %>% .[[1]]
      despesas <- page.a %>% html_nodes("table .listagem") %>% html_table()  %>% .[[2]]
      
      
      receitas <- receitas[-c(1, 18, 28, 30, 36, 40), ]
      despesas <- despesas[-c(1,2, 10, 15, 17, 31), ]

   
      temp <- c(receitas[,2], despesas[,2])
      data3[i, 1:3] <- as.matrix(url.list[i,1:3])
      data3[i, 4:ncol(data3)] <- temp


    }
    return(data3)
}    
    
###############################################################################

cnes.assets <- function(url.list=url.list){
  
  data4 <- matrix(NA, nrow(url.list), 56)
  colnames(data4) <- c("cnpj", "year", "main_url", "Disponível", "Contas Vinculadas",  
                       "Convênios, Acordos e Ajustes",  "Valores a Receber de Terceiros",
                       "Adiantamento a Empregados",  
                       "Outras Contas e Títulos a Receber", "(-) Provisão para Devedores Duvidosos", 
                       "Estoques",  "Despesas Antecipadas", "Outras Contas do Ativo Circulante", 
                       "TOTAL ATIVO CIRCULANTE", "Valores a Receber a Longo Prazo", 
                       "TOTAL ATIVO REALIZAVEL A LONGO PRAZO", 
                       "Investimentos", "Imobilizado", "(-) Depreciação/Amortização Acumulada", 
                       "Diferido",  "Outros Ativos Permanentes", "TOTAL ATIVO PERMANENTE", 
                       "TOTAL ATIVO", "Fornecedores",  "Obrigações Trabalhistas", 
                       "Obrigações Sociais", "Prestadores de Serviços", "Aluguéis a Pagar", 
                       "Adiantamento de Clientes", "Empréstimos e Financiamentos a Pagar CP", 
                       "Obrigações Fiscais Exceto IRenda e CSLL", 
                       "Convênios Públicos (Saldo)",  "Adiantamento de Projetos",  "Subvenções Públicas (Saldo)",
                       "Recursos de Leis de incentivo Fiscal", "(-) Recursos de Leis de Incentivo Fiscal Utilizados",
                       "Provisão para IRenda e CSLL", 
                       "Sentenças Judiciais Trabalhistas a Pagar", "Sentenças Judiciais a Pagar - Exceto Trabalhista", 
                       "Outros Passivos Circulantes", 
                       "TOTAL PASSIVO CIRCULANTE", "Empréstimos e Financiamentos a Pagar a Longo Prazo",  
                       "Contas a Pagar", 
                       "Aluguéis Antecipados",  "Outros Passivos Exigíveis a Longo Prazo", 
                       "TOTAL PASSIVO EXIGÍVEL A LONGO PRAZO", 
                       "Resultado de Exercícios Futuros", "TOTAL RESULTADO DE EXERCÍCIOS FUTUROS", 
                       "Patrimônio Social (Fundo Patrimonial)", 
                       "Doações Patrimoniais", "Reservas Constituídas",  "Superávit(s) dos(s) exercício(s)", 
                       "Déficit(s) do(s) exercício(s)", "Outras Contas do Patrimônio Social", 
                       "TOTAL PATRIMÔNIO", "TOTAL PASSIVO")

  
  for (i in 1:nrow(url.list)){   
    print(i)
    #Assets
    Sys.sleep(sample(seq(0, .5, by=0.001), 1))
    url.c <- paste("http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/", 
                   url.list[i, 4], "/BalancoPatrimonial.html", sep="")
    
    page.c <- try(read_html(url.c), silent=TRUE)
    
    if (class(page.c)[1] == "try-error"){
      Sys.sleep(sample(seq(0, .5, by=0.001), 1))
      page.c <- try(read_html(url.c), silent=TRUE)
      if (class(page.c)[1] == "try-error"){
        next
      }
    }
    
    
    patrimonio.ativo <- page.c %>% html_nodes("table .listagem") %>% html_table()  %>% .[[1]]
    patrimonio.passivo <- page.c %>% html_nodes("table .listagem") %>% html_table()  %>% .[[2]]
    

    patrimonio.ativo <- patrimonio.ativo[-c(12, 15), ]
    patrimonio.passivo <- patrimonio.passivo[-c(1, 20, 26, 29),   ]
    
    temp <- c(patrimonio.ativo[,2], patrimonio.passivo[,2])
    data4[i, 1:3] <- as.matrix(url.list[i,1:3])
    data4[i, 4:ncol(data4)] <- temp
  }
  return(data4)
}  

###############################################################################


cnes.source <- function(url.list=url.list){
  
  data5 <- matrix(NA, nrow(url.list), 10)
  colnames(data5) <- c("cnpj", "year", "main_url", 
                       "Própria (recursos decorrentes da prestação de serviços da entidade)",
                       "Própria (recursos decorrentes de mensalidades / doações dos membros ou associados)",      
                       "Privada (recursos de doações e parcerias com empresas e entidades privadas)",
                       "Privada (recursos de doações eventuais)",
                       "Pública (recursos de subvenções, convênios e parcerias com órgãos ou entidades públicas)",
                       "Internacional Privada (recursos de entidades e organizações internacionais)",
                       "Internacional Pública (recursos de países estrangeiros, ONU, etc.)") 
  
  for (i in 1:nrow(url.list)){ 
    Sys.sleep(sample(seq(0, .5, by=0.001), 1))
    print(i)
    data5[i, 1:3] <- as.matrix(url.list[i,1:3])

    #Sources 
    url.s <- paste("http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/", 
                   url.list[i, 4], "/FontesRecursos.html", sep="")
    
    
    page.s <- try(read_html(url.s), silent=TRUE)
    
    while (class(page.s)[1] == "try-error"){
      Sys.sleep(sample(seq(0, 1, by=0.001), 1))
      page.s <- try(read_html(url.s))
    }
    
  
    sources <- page.s %>% html_nodes("table .listagem") %>% html_table()  %>% .[[1]]

    data5[i, 4] <- sources$Percentual[1]
    data5[i, 5] <- sources$Percentual[2]
    data5[i, 6] <- sources$Percentual[3]
    data5[i, 7] <- sources$Percentual[4]
    data5[i, 8] <- sources$Percentual[5]
    data5[i, 9] <- sources$Percentual[6]
    data5[i, 10] <- sources$Percentual[7]
    
  }
  return(data5) 
}

###############################################################################

cnes.partner <- function(url.list=url.list){    
     
  ltemp <- list()
  
  for (i in 1:nrow(url.list)){
    print(i)
    Sys.sleep(sample(seq(0, .5, by=0.001), 1))
    #Partnerships
    url.ps <- paste("http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/", 
                   url.list[i, 4], "/ListaParceriasSubvencoesPublicas.html", sep="")
    
    page.ps <- try(read_html(url.ps), silent=TRUE)
    
    if (class(page.ps)[1] == "try-error"){
      Sys.sleep(sample(seq(0, 1, by=0.001), 1))
      page.ps <- try(read_html(url.ps), silent=TRUE)
      if (class(page.ps)[1] == "try-error"){
        next
      }
    }
    
    sources <- page.ps %>% html_nodes("table .listagem") %>% html_table()
      
    if (length(sources)==1) {
      
    temp <- cbind(rep(url.list[i, 1], nrow(sources[[1]])), rep(url.list[i, 2], nrow(sources[[1]])),
                  rep(url.list[i, 4], nrow(sources[[1]])),
                  sources[[1]])
    colnames(temp) <- c("cnpj", "ano", "num", "Nome Órgão_Entidade", 
                        "Natureza do Instrumento", "Posição na Estrutura Federativa")
    ltemp[[i]] <- temp
  }
  }
  data6 <- do.call(rbind, ltemp)
  return(data6)  
}                        
                        
###############################################################################

#Only those with parnternships
cnes.partner.details <- function(url.list=partner){    
  
  ltempf <- list()
  url.list$cnpjs_year <- paste(url.list$cnpj, url.list$ano, sep="_")
  cnpjs <- unique(url.list$cnpjs_year)
  
  for (i in 1:length(cnpjs)){
      print(i)
      Sys.sleep(sample(seq(0, .5, by=0.001), 1))
      temp <- url.list[url.list$cnpjs_year==cnpjs[i], ]
      nr.p <- nrow(temp) #getting number of partnerships

      #Details on partnerships
      temp1 <- matrix(NA, nr.p, 15)
      
      colnames(temp1) <- c("cnpj", "year", "num", "Nome do Órgão ou Entidade de Parceria", "Classificação do órgão na estrutura administrativa",
                           "Posição do órgão na estrutura federativa", "Origem dos recursos repassados",
                           "Natureza do instrumento de parceria", "Data de publicação na imprensa oficial",
                           "Total de recursos financeiros previstos", "Recursos financeiros já repassados", 
                           "Nº de  Beneficiários", "Previsão de início das atividades", 
                           "Previsão de término das atividades", "Resumo do objetivo da parceria")
      
      for (j in 1:nr.p){
        print(j)
        Sys.sleep(sample(seq(0, .5, by=0.001), 1))
        url.j <- paste("http://portal.mj.gov.br/CNEsPublico/relatorioCNEs/", 
                       temp[j, 3], "/ParceriasSubvencoesPublicas", j,  ".html", sep="")
        
        page.j <- try(read_html(url.j), silent=TRUE)
        
        if (class(page.j)[1] == "try-error"){
          Sys.sleep(sample(seq(0, 1, by=0.001), 1))
          page.p <- try(read_html(url.j), silent=TRUE)
          if (class(page.j)[1] == "try-error"){
            next
          }
        }
        
        details <- page.j %>% html_nodes("table .formulario") %>% html_table()
        
        temp1[j,1] <-  as.character(temp[j, 1])
        temp1[j, 2] <- as.character(temp[j,2])
        temp1[j, 3] <- as.character(temp[j,3])
        temp1[j,4] <-  as.character(details[[1]])
        temp1[j,5] <-  as.character(details[[2]])
        temp1[j,6] <-  as.character(details[[3]])
        temp1[j,7] <-  as.character(details[[4]])
        temp1[j,8] <-  as.character(details[[5]])
        temp1[j,9] <- as.character(details[[6]])
        temp1[j,10] <-  as.character(details[[7]])
        temp1[j,11] <-  as.character(details[[8]])
        temp1[j,12] <-  as.character(details[[9]])
        temp1[j,13] <-  as.character(details[[10]])
        temp1[j,14] <-  as.character(details[[11]])
        temp1[j,15] <-   as.character(details[[12]])
      }  
      ltempf[[i]] <- temp1
    } 
  data7 <- do.call(rbind, ltempf)
  return(data7)  
}                        

  



