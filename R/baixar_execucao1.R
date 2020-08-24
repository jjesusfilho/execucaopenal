#' Baixar execução penal do TJRJ (apenas execucao)
#'
#' @param usuario usuário
#' @param senha   senha
#' @param processo Número do processo da execução penal
#' @param diretorio Diretório onde colocar os htmls
#'
#' @return html da execução
#'
#' @export
#'
baixar_execucao1 <- function(usuario = NULL, senha  = NULL, processo = NULL, diretorio = "."){

  agente <- "Mozilla/5.0 (Macintosh;Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36"

  url <- "https://www3.tjrj.jus.br/projudi/usuario/logon.do?actionType=inicio&r=0.24277328459145298"

  h <- httr::handle(url)

  s <- httr::GET(handle = h)

  conteudo <- s %>%
    httr::content()

  tj <- xml2::xml_find_first(conteudo,"//*[@name='formLogin']") %>%
    xml2::xml_attr("action") %>%
    stringr::str_extract("(?<=_tj\\=).+")

  md5 <- xml2::xml_find_first(conteudo,"//script[contains(.,'hex_md5')]") %>%
    xml2::xml_text() %>%
    stringr::str_extract('(?<=md5...).+?(?=\\W)')

  md5 <- digest::digest(paste0(md5,usuario),"md5")

  url2 <-paste0("https://www3.tjrj.jus.br/projudi/usuario/logon.do?_tj=",tj)

  s2 <- httr::POST(url2,
                   body= list(login=usuario,senha=senha,tck=md5),
                   encode = "form",
                   httr::user_agent(agente),
                   handle = s$hanlde)

  url3 <- "https://www3.tjrj.jus.br/projudi/consultor/frameCentroConsultor.jsp?p=0.9858354673921179"

  s3 <- httr::GET(url3)

  # url4 <- "https://www3.tjrj.jus.br/projudi/processo/buscaProcesso.do?actionType=iniciarSimples&pMRnd=1596950580424"

  url4 <- "https://www3.tjrj.jus.br/projudi/processo/buscaProcesso.do?actionType=pesquisaSimples"

  purrr::walk(processo,purrr::possibly(~{

    x <- stringr::str_remove_all(.x,"\\D")

    x1 <- x %>%
      abjutils::build_id()

    body <- list(
      page =  1,
      flagNumeroUnico =  TRUE,
      flagNumeroFisicoAntigo = TRUE,
      numeroProcesso =  x1

    )

    a_execucao <- file.path(diretorio,paste0("execucao_",x, ".html"))

    httr::POST(url4,body = body, encode = "form", httr::write_disk(a_execucao,overwrite = T))


  },NULL))



}




