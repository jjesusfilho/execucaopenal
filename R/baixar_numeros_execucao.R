#' Baixar números das execuções
#'
#' @param usuario Usuário
#' @param senha  Senha
#' @param data_inicial Data inicial no formato dd/mm/yyyy
#' @param data_final Data final no formato dd/mm/yyyy
#' @param diretorio Diretório. Default para atual.
#'
#' @return htmls
#' @export
#'
baixar_numeros_execucao <- function(usuario = NULL, senha = NULL, data_inicial = NULL, data_final = NULL, diretorio = "."){



  agente <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36"

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

  # url3 <- "https://www3.tjrj.jus.br/projudi/usuario/controleAreaAtuacao.do?actionType=inicio&noCache=SP2152171592104632960"
  #
  # s3 <- httr::GET(url3,handle = s$handle)

  url4 <- "https://www3.tjrj.jus.br/projudi/consultor/frameCentroConsultor.jsp?p=0.9858354673921179"

  s4 <- httr::GET(url4)


  url5 <- "https://www3.tjrj.jus.br/projudi/processo/buscaProcesso.do?actionType=pesquisar"


  body <-
    list(
      processoPageSize = "100",
      processoPageNumber = "1",
      processoSortColumn = "p.numeroUnico",
      processoSortOrder = "asc",
      nomeParte = "",
      nomeMae = "",
      tipoPesqNomeMae = "F",
      nomePai = "",
      tipoPesqNomePai = "F",
      cpfCnpj = "",
      rg = "",
      opcao = "ambos",
      codTipoPrioridade = "-1",
      codVara = "3067",
      idClassificacaoProcessual = "-1",
      idClasseProcessual = "",
      descricaoClasseProcessual = "",
      idAssuntoPrincipal = "",
      descricaoAssuntoPrincipal = "",
      idObjetoAcao = "-1",
      origemProcesso = "-1",
      idStatus = "-1",
      segredo = "-1",
      idLocalizador = "-1",
      dataInicio = data_inicial,
      dataFim = data_final,
      siglaAdvogado = "",
      codTipoAcaoVinculada = "-1",
      numeroAcaoVinculada = "",
      codDelegacia = "",
      numeroPecaOrigem = "",
      numProcessoCriminal = "",
      descricao = ""
    )

  di <- stringr::str_replace_all(data_inicial,"\\D","_")
  df <- stringr::str_replace_all(data_final,"\\D","_")

  arquivo <- file.path(diretorio,paste0("execucao_",di,"_",df,"_processos_pagina_1.html"))

  s5 <- httr::POST(url5,body = body, encode = "form",httr::write_disk(arquivo,overwrite = TRUE))

  paginas <- s5 %>%
    httr::content() %>%
    xml2::xml_find_first('//*[@class="navLeft"]') %>%
    xml2::xml_text() %>%
    stringr::str_extract("\\d+") %>%
    as.numeric() %>%
    `/`(100) %>%
    ceiling()

  if (paginas > 1){

    purrr::walk(2:paginas,purrr::possibly(~{

      body$processoPageNumber <- as.character(.x)

      arquivo <- stringr::str_replace(arquivo,"(?<=pagina_)\\d+",as.character(.x))

      httr::POST(url5,body = body, encode = "form",httr::write_disk(arquivo,overwrite = TRUE))

    },NULL))

  }


}
