# Inicialmente, ir para o command line e iniciar o contêiner:
#
#   docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox:2.53.0

library(magrittr)
library(RSelenium)
library(purrr)
library(stringr)
library(dplyr)
library(rvest)
library(xml2)
#### iniciar ####
#system("docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0")

remDr <- remoteDriver(port = 4445L)


remDr$open()

url1<-"https://www3.tjrj.jus.br/projudi/"

remDr$navigate(url1)
Sys.sleep(5)


webElem <- remDr$findElements(value="//frame")
remDr$switchToFrame(webElem[[2]])

remDr$screenshot(display = TRUE)



login<-"SP215217"
password<-"hZbZBYFj7dax2c"

webElem<- remDr$findElement("id", "login")

webElem$sendKeysToElement(list(login))

webElem<-remDr$findElement("id", "senha")

webElem$sendKeysToElement(list(password))

webElem<-remDr$findElement("id", "btEntrar")

webElem$clickElement()

remDr$screenshot(display = TRUE)


webElem<-remDr$findElement("id","Stm0p1i0eHR")
url_primeiro_grau<-webElem$getElementAttribute("href") %>% unlist()



remDr$screenshot(display = TRUE)


#### fim da autenticação ####




diretorio<-"/home/jose/ufrj/data-raw/processos"
p <- list.files(diretorio,include.dirs = T)
processo<-setdiff(processo,p)

for(j in 1:length(processo)){

  i<-processo[j]

  try({
    remDr$navigate(url_primeiro_grau)
    Sys.sleep(3)

    webElem<- remDr$findElement("id", "numeroProcesso")

    #i <- stringr::str_remove_all(i,"\\D")
    webElem$sendKeysToElement(list(i))

    remDr$screenshot(display = TRUE)

    webElem<-remDr$findElement("id", "pesquisar")
    webElem$clickElement()

    webElem <-remDr$findElement("id","situacaoPrisional")
    webElem$clickElement()


    webElem <-remDr$findElement("id","requisitoTemporal")
    webElem$clickElement()


    remDr$screenshot(display = TRUE)

    dir.create(file.path(diretorio,i))

    remDr$getPageSource() %>%
      .[[1]] %>%
      xml2::read_html() %>%
      xml2::write_html(file.path(diretorio,i,"dados_execucao.html"))


    ## Execução


    ## Perfil do apenado

    webElem<-remDr$findElement("xpath",'//td[contains(.,"Sentenciado:")]/following-sibling::td[1]/a')
    webElem$clickElement()

    webElem<-remDr$findElement("xpath",'//*[@id="tabItemprefix2"]/div[2]/a')
    webElem$clickElement()

    remDr$getPageSource() %>%
      remDr$getPageSource() %>%
      .[[1]] %>%
      xml2::read_html() %>%
      xml2::write_html(file.path(diretorio,i,"perfil_apenado.html"))


    remDr$screenshot(display = TRUE)


    ### Andamento

    # Há inconsistências nos processos. Nem sempre existem processos criminais, mesmo
    # havendo indicação de que existem. Isso leva a reiniciar a busca do processo.
    ## ALém disso, pode ocorrer de que processos muito grandes imponham captchas. Daí é necessário
    ## Reiniciar a conexão.
    webElem<-remDr$findElement("name","backButton")
    webElem$clickElement()
    Sys.sleep(3)
    webElem<-remDr$findElement("id",'tabItemprefix2')

    webElem$clickElement()

    remDr$getPageSource() %>%
      .[[1]] %>%
      xml2::read_html() %>%
      xml2::write_html(file.path(diretorio,i,"andamento.html"))




    #Navegar para processos criminais

    remDr$navigate(url_primeiro_grau)
    Sys.sleep(3)

    webElem<- remDr$findElement("id", "numeroProcesso")

    webElem$sendKeysToElement(list(i))

    remDr$screenshot(display = TRUE)

    webElem<-remDr$findElement("id", "pesquisar")
    webElem$clickElement()

    Sys.sleep(5)
    remDr$screenshot(display = TRUE)


    webElem<-remDr$findElement("id",'tabItemprefix3')



    webElem$clickElement()

    webElem<-remDr$findElements("xpath",'//a[contains(@href,"acao_penal_vinculada")]')

    remDr$screenshot(display = TRUE)


    links<-sapply(webElem, function(x){x$getElementAttribute("href")}) %>%
      unlist()


    walk2(links,1:length(links),~{
      .x %>%
        remDr$navigate()

      remDr$getPageSource() %>%
        .[[1]] %>%
        xml2::read_html() %>%
        xml2::write_html(file.path(diretorio,i,paste0("processo_criminal_",.y,".html")))

    })

  })
}



