#' Baixa números dos processos
#'
#' @param login Login do usuário previamente cadastrado
#' @param passwd Senha do usuário previamente cadastrado
#' @param start Data inicial no formato dd/mm/aaaa
#' @param end  Data final no formato dd/mm/aaaa
#' @param diretorio Diretório onde armazenar os arquivos
#'
#' @details Esta função é interna. Ela pressupõe que esteja com o
#'     conteiner do Selenium rodando e o RSelenium carregado.
#' @return htmls
#'
baixar_numeros <- function(login=NULL,passwd=NULL,start=NULL,end=NULL, diretorio="."){

  if(is.null(login)){
    stop("You have to provide your login")
  }

  if(is.null(passwd)){
    stop("You have to provide your password")
  }

  if(is.null(start)){
    stop("You have to provide your start")
  }

  if(is.null(end)){
    stop("You have to provide your end")
  }

  remDr <- RSelenium::remoteDriver(port = 4445L)

  remDr$open()

  url<-"https://www3.tjrj.jus.br/projudi/"

  remDr$navigate(url)
  Sys.sleep(2)

  webElem <- remDr$findElements(value="//frame")

  remDr$switchToFrame(webElem[[2]])



  webElem<- remDr$findElement("id", "login")

  webElem$sendKeysToElement(list(login))

  webElem<-remDr$findElement("id", "senha")

  webElem$sendKeysToElement(list(passwd))

  webElem<-remDr$findElement("id", "btEntrar")

  webElem$clickElement()

  webElem<-remDr$findElement("id","Stm0p1i1eHR")

  url_busca_avancada<-webElem$getElementAttribute("href") %>%
    unlist()

  remDr$navigate(url_busca_avancada)


  webElem<-remDr$findElement("id","dataInicio")

  webElem$sendKeysToElement(list(start))

  webElem<-remDr$findElement("id","dataFim")

  webElem$sendKeysToElement(list(end))


  webElem<-remDr$findElement("id", "pesquisar")
  webElem$clickElement()

  pagina1<-webElem$getPageSource() %>%
    unlist() %>%
    xml2::read_html()


  n<- pagina1 %>%
    xml2::xml_find_all("//*[@class='navLeft']") %>%
    xml2::xml_text() %>%
    stringr::str_extract("\\d+") %>%
    as.numeric() %>%
    `/`(20) %>%
    ceiling()

  ini <- stringr::str_replace_all(start,"/","_")
  fin <- stringr::str_replace_all(end,"/","_")

  arquivos<-paste0(diretorio,"/pagina_",1:n,".html")

  xml2::write_html(pagina1,arquivos[1])

  purrr::walk(arquivos[2:n],purrr::possibly(~{
    Sys.sleep(2)
    webElem<-remDr$findElement("class","arrowNextOn")
    webElem$clickElement()
    webElem$getPageSource() %>%
      unlist() %>%
      xml2::read_html() %>%
      xml2::write_html(.x)

  },NULL))

  remDr$close()
}

