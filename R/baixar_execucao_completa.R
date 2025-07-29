#' Baixa html, pdfs e tabela com indice de docs
#'
#' @param usuario Usuário
#' @param senha  Senha
#'
#' @param diretorio Diretório. Default para atual.
#'
#' @return htmls, pdfs e rds
#' @export
#'
baixar_execucao_completa <- function(usuario = NULL,
                            senha = NULL,
                            processo = NULL,
                            diretorio = ".") {
  # Prompt for information if necessary
  if (is.null(usuario) || is.null(senha)) {
    usuario <- Sys.getenv("SEEU_USUARIO")
    senha <- Sys.getenv("SEEU_SENHA")

    if (usuario == "" || senha == "") {
      usuario <- as.character(getPass::getPass(msg = "Entre com usuário: "))
      senha <- as.character(getPass::getPass(msg = "Entre com senha: "))
    }
  }



  agente <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36"

  url <- "https://seeu.pje.jus.br/seeu/usuario/logon.do?actionType=inicio&r=0.994791702103693"

  h <- httr::handle(url)

  s <- httr::GET(handle = h)

  conteudo <- s |>
    httr::content()

  tj <- xml2::xml_find_first(conteudo, "//*[@name='formLogin']") |>
    xml2::xml_attr("action") |>
    stringr::str_extract("(?<=_tj\\=).+")

  md5 <- xml2::xml_find_first(conteudo, "//script[contains(.,'hex_md5')]") |>
    xml2::xml_text() |>
    stringr::str_extract('(?<=md5...).+?(?=\\W)')

  md5 <- digest::digest(paste0(md5, usuario), "md5")

  url2 <- paste0("https://seeu.pje.jus.br/seeu/usuario/logon.do?_tj=", tj)

  s2 <- httr::POST(
    url2,
    body = list(
      login = usuario,
      senha = senha,
      tck = md5
    ),
    encode = "form",
    httr::user_agent(agente),
    handle = h
  )

  # url3 <- "https://www3.tjrj.jus.br/projudi/usuario/controleAreaAtuacao.do?actionType=inicio&noCache=SP2152171592104632960"
  #
  # s3 <- httr::GET(url3,handle = s$handle)

  url4 <- "https://seeu.pje.jus.br/seeu/consultor/frameCentroConsultor.jsp?p=0.9858354673921179"


  s4 <- httr::GET(url4)


  url5 <- "https://seeu.pje.jus.br/seeu/processo/buscaProcesso.do?actionType=pesquisaSimples"


  body <- list(
    page = "1",
    flagNumeroUnico = "true",
    flagNumeroFisicoAntigo = "false",
    numeroProcesso = processo
  )


  p <- stringr::str_remove_all(processo, "\\D+")

  arquivo <- file.path(diretorio,
                       paste0("execucao_",p,".html"))

  r5  <- httr::POST(
    url5,
    body = body,
    encode = "form"
  )

writeBin(r5$content,arquivo)

navegador <- r5 |>
      httr::content() |>
     xml2::xml_find_first("//input[@value='Navegar']") |>
     xml2::xml_attr("onclick") |>
     stringr::str_extract("seeu\\X+(?=')") |>
     xml2::url_absolute("https://seeu.pje.jus.br/")

r6 <- httr::GET(navegador)

url_processo <- r6 |>
      httr::content() |>
      xml2::xml_find_first("//iframe[@name='menumov']") |>
      xml2::xml_attr("src") |>
      xml2::url_absolute("https://seeu.pje.jus.br/seeu/processo/")


r7 <- httr::GET(url_processo) |>
      httr::content("text") |>
      stringr::str_extract("seeu/ajax.+(?=\")") |>
      xml2::url_absolute("https://seeu.pje.jus.br") |>
      httr::GET()

conteudo <- httr::content(r7)

nome_arquivo <- conteudo |>
      xml2::xml_find_all("//input[contains(@name,'nomeArquivo')]") |>
      xml2::xml_attr("value")

url_completa <- conteudo |>
  xml2::xml_find_all("//input[contains(@name,'urlCompleta')]") |>
  xml2::xml_attr("value") |>
  xml2::url_absolute("https://seeu.pje.jus.br")

descricao_arquivo <- conteudo |>
  xml2::xml_find_all("//input[contains(@name,'descricaoArquivo')]") |>
  xml2::xml_attr("value")


numero_ordem <- conteudo |>
  xml2::xml_find_all("//div[@class='arquivo opcaoExibicaoArquivo']/span[@class='number']") |>
  xml2::xml_text()

tb <- tibble::tibble(processo = p, url_processo, nome_arquivo, url_completa, descricao_arquivo, numero_ordem)

tb <- tb |>
   dplyr::mutate(arquivo = stringr::str_replace(numero_ordem,'\\D',"_") |>
                   paste0("execucao_processo_",p, "_numero_ordem_",... = _, ".pdf") |>
                   file.path(diretorio,... = _))

arquivo <- file.path(diretorio, paste0("tabela_docs_", p,".rds"))

saveRDS(tb, arquivo)


purrr::walk2(tb$url_completa, tb$arquivo, ~{

  httr::GET(.x, httr::write_disk(.y, overwrite= T))


})


}

