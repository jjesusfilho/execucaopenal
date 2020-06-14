#' Baixa pdfs
#'
#' @param usuario Usuário
#' @param senha  Senha
#' @param id_proc Id do processo
#' @param processo Número do processo
#' @param diretorio Onde colocar os pdfs
#'
#' @return pdf
#' @export
#'
baixar_pdf <- function(usuario = NULL, senha = NULL, id_proc = NULL, processo = NULL, diretorio = ".") {

ses <- autentica(usuario, senha)

if (is.null(id_proc)){

url <- "https://www3.tjrj.jus.br/projudi/processo/buscaProcesso.do?actionType=pesquisaSimples"

body <- list(
  page = "1",
  flagNumeroUnico = "true",
  flagNumeroFisicoAntigo = "false",
  numeroProcesso = processo
)

id_proc <- httr::POST(url,body= body,encode = "form",handle= ses$handle) %>%
  httr::content("text") %>%
  stringr::str_extract("(?<=numeroProcesso = )\\d+")


}

arquivo <- file.path(diretorio,paste0(id_proc,".pdf"))


 ses <-   navegar(ses, id_proc)

  url_post <- ses %>% extrai_post()

  valores <- ses %>% poe_valores()

  res <- ses %>% postar(url_post, valores)

  if (res$response$headers$`content-type` != "application/pdf") {
    return("sem_pdf")

  } else {
    escrever(res, arquivo)
    "ok"
  }
}




autentica <- function(usuario = NULL, senha = NULL) {


  if(is.null(usuario)){

    usuario <-  Sys.getenv("USER_UFRJ")

  }

  if(is.null(senha)){

    senha <-  Sys.getenv("PASSWORD_UFRJ")
  }



  agente <-
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36"

  url <-
    "https://www3.tjrj.jus.br/projudi/usuario/logon.do?actionType=inicio&r=0.13796233039447725"

  user_agent <- httr::user_agent(agente)

  ses <- rvest::html_session(url, user_agent)

  form <- ses %>% rvest::html_form()

  form <- form[[1]] %>%
    rvest::set_values(login = usuario, senha = senha)
  rvest::submit_form(ses, form, httr::timeout(60))
}


extrai_post <- function(ses) {
  pat <- "/projudi/processo/exportar.do\\?_tj=[[:alnum:]]+"
  url <- ses %>%
    rvest::html_nodes(xpath = '//*[@id="content"]') %>%
    rvest::html_nodes("script") %>%
    purrr::detect(function(x)
      stringr::str_detect(x, "voltar")) %>%
    stringr::str_extract(pat)

  paste0("https://www3.tjrj.jus.br", url)
}

poe_valores <-
  function(ses,
           mov = "false",
           lim = "false",
           tamanho = 300) {

    form <- ses %>%
      rvest::html_form() %>%
      purrr::pluck(1)

    pega_valores <- function(x) {
      form$fields[[x]]$value
    }

    valores <-
      sapply(1:length(form$fields),
             pega_valores,
             simplify = F,
             USE.NAMES = T)
    names(valores) <- names(form$fields)

    valores$gerarMovimentacoes <- mov
    valores$usuarioLimiteExportacao <- lim
    valores$tamanhoLimiteExportacao <- tamanho
    valores


  }

postar <- function(ses, url_post, valores) {
  ses %>%
    request_POST(
      url_post,
      httr::config(referer = ses$url),
      httr::user_agent(ses$config$options$useragent),
      body = valores,
      encode = "form"
    )


}

escrever <- function(res, file) {
  writeBin(res$response$content, con = file)
}



# importa_pdf1 ------------------------------------------------------------

# Essa função funciona com o número do id do processo. Com ele, dá para
# pular da página de autenticação direto pra de exportação. Essa função
# navegar é o que a distingue. Joga da autenticação para a página de
# exportação

navegar <- function(ses, id_proc) {
  url <-
    paste0(
      "https://www3.tjrj.jus.br/projudi/processo/navegarProcesso.do?actionType=carregarFrameMenu&numeroProcesso=",
      id_proc
    )

  ses <- ses %>% rvest::jump_to(url)

  url <- ses %>% rvest::html_nodes("input") %>%
    purrr::pluck(2) %>%
    rvest::html_attr("onclick") %>%
    stringr::str_extract("/projudi/processo/.+?(?=')") %>%
    xml2::url_absolute("https://www3.tjrj.jus.br")

  ses %>% rvest::jump_to(url)

}

request_POST <- function (x, url, ...)
{
  x$response <- httr::POST(url, x$config, ..., handle = x$handle)
  x$html <- new.env(parent = emptyenv(), hash = FALSE)
  x$url <- x$response$url
  x$back <- character()
  httr::warn_for_status(x$response)
  x
}


id_processo <- function(ses, processo) {
  url <-
    "https://www3.tjrj.jus.br/projudi/processo/buscaProcesso.do?actionType=iniciarSimples&pMRnd=1589225341920"
  ses <- ses %>% rvest::jump_to(url)
  form <- ses %>%
    rvest::html_form() %>%
    purrr::pluck(1) %>%
    rvest::set_values(numeroProcesso = processo)
  ses %>%
    rvest::submit_form(form, httr::timeout(60)) %>%
    purrr::pluck("response") %>%
    httr::content("text") %>%
    stringr::str_extract("(?<=numeroProcesso = )\\d+")

}



