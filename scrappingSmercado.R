#WebScrapping Supermecado

#Pacotes
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)

# Endereço do Site
webpage <- read_html("https://www.superpaguemenos.com.br/")

# Menu Categorias
results <-  
  webpage%>% 
  html_nodes(".center")%>% 
  html_nodes("nav")%>% 
  html_nodes("ul")%>% 
  html_nodes("li")%>% 
  html_nodes("a")

#Criando lista com menu
categorias <- vector()
idref=''
nr=1
for (i in seq_along(results)) { 
  url_menu <- 
    results[i]%>%
    html_attr("href",trimws(TRUE))
  id=str_extract(url_menu,"[0-9]+")
  if(is.na(id)==TRUE){next}
  if (is.na(url_menu)==FALSE & id != idref) {
    categorias[[nr]] <- url_menu
    nr=nr+1
  }
  idref=id
}

tabelaFinal <- 
  data.frame(CATEGORIA = '',
             PRODUTO = '', 
             PRECO = '')

for (a in seq_along(categorias)) {
  webpage <- 
    read_html(str_replace(paste("https://www.superpaguemenos.com.br",categorias[a]),' ',''))
  # Os produtos dentro das categorias podem estar em várias páginas, 
  # Por isso primeiro pego a quantidade de paginas e depois faço um loop
  # coletando os preços dos produtos pg por pg
  # número de páginas
  results <- 
    webpage%>% 
    html_nodes(".pagination")%>% html_nodes("li")%>% html_nodes("a")
  qtd_pgs=0
  for (b in seq_along(results)) {
    if (results[b]%>%
        html_text(trim = TRUE) != ""){
      qtd_pgs=qtd_pgs+1
    }
  }
  # Formando Data Frame com os produtos e valores
  nrpg=1
  for (c in c(1:qtd_pgs)) {
    webpage_b <- 
      read_html(str_replace_all(paste("https://www.superpaguemenos.com.br",categorias[a],"?p=",nrpg),' ',''))
    results <- 
      webpage_b %>% 
      html_nodes(".item-product")%>% 
      html_nodes(".desc")
    records <- 
      vector("list", length = length(results))
    # listar produtos
    for (d in seq_along(results)) {
      CATEGORIA <- 
        str_to_upper(str_extract(categorias[a],"[a-z]+"))
      PRODUTO <- 
        results[d]%>% 
        html_nodes(".title")%>%
        html_text(trim = TRUE)
      PRECO <- 
        results[d]%>% 
        html_nodes(".prices")%>% 
        html_nodes(".price")%>%
        html_text(trim = TRUE)
      records[[d]] <- 
        data_frame(CATEGORIA = CATEGORIA, 
                   PRODUTO = PRODUTO, 
                   PRECO = PRECO)
    }
    tabela <- 
      bind_rows(records)  
    tabelaFinal <- 
      rbind(tabelaFinal,tabela)
    nrpg=nrpg+1
  }
}

#Visualizar lista de produtos coletados
View(tabelaFinal)

#FIM