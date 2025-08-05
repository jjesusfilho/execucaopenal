#' Baixar números das execuções
#'
#' @param usuario Usuário
#' @param senha  Senha
#' @param data_inicial Data inicial no formato dd/mm/yyyy
#' @param data_final Data final no formato dd/mm/yyyy
#' @param codigo_tribunal Veja dataframe tribunal. Padrão para todos.
#' @param sexo Opções "M" para masculino, "F" para feminino e "I" para "intersexo".
#'        Deixar em branco para todos.
#' @param  situacao_rua Coloque Default FALSE
#' @param diretorio Diretório. Default para atual.
#'
#' @return htmls
#' @export
#'
baixar_numeros_execucao <- function(usuario = NULL,
                                    senha = NULL,
                                    data_inicial = NULL,
                                    data_final = NULL,
                                    codigo_tribunal = "-1",
                                    sexo = "",
                                    situacao_rua = FALSE,
                                    diretorio = "."){



  # Prompt for information if necessary
  if (is.null(usuario) || is.null(senha)) {

    usuario <- Sys.getenv("SEEU_USUARIO")
    senha <- Sys.getenv("SEEU_SENHA")

    if ( usuario == "" || senha == "") {

      usuario <- as.character(getPass::getPass(msg = "Entre com usuário: "))
      senha <- as.character(getPass::getPass(msg = "Entre com senha: "))
    }
}



  if(situacao_rua){

    situacao_rua <- "on"

  } else {

    situacao_rua = ""
  }


agente <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36"

url <- "https://seeu.pje.jus.br/seeu/usuario/logon.do?actionType=inicio&r=0.994791702103693"

  h <- httr::handle(url)

  s <- httr::GET(handle = h)

  conteudo <- s |>
    httr::content()

  tj <- xml2::xml_find_first(conteudo,"//*[@name='formLogin']") |>
    xml2::xml_attr("action") |>
    stringr::str_extract("(?<=_tj\\=).+")

  md5 <- xml2::xml_find_first(conteudo,"//script[contains(.,'hex_md5')]") |>
    xml2::xml_text() |>
    stringr::str_extract('(?<=md5...).+?(?=\\W)')

  md5 <- digest::digest(paste0(md5,usuario),"md5")

url2 <-paste0("https://seeu.pje.jus.br/seeu/usuario/logon.do?_tj=",tj)

  s2 <- httr::POST(url2,
                   body= list(login=usuario,senha=senha,tck=md5),
                   encode = "form",
                   httr::user_agent(agente),
                   handle = s$hanlde)

  # url3 <- "https://www3.tjrj.jus.br/projudi/usuario/controleAreaAtuacao.do?actionType=inicio&noCache=SP2152171592104632960"
  #
  # s3 <- httr::GET(url3,handle = s$handle)

url4 <- "https://seeu.pje.jus.br/seeu/consultor/frameCentroConsultor.jsp?p=0.9858354673921179"


  s4 <- httr::GET(url4)


url5 <- "https://seeu.pje.jus.br/seeu/processo/criminal/execucao/buscaProcessoExecucao.do?actionType=pesquisar"



  body <- list(
    processoPageSize = "100",
    processoPageNumber = "1",
    processoSortColumn = "p.numeroUnico",
    processoSortOrder = "asc",
    disabled = "false",
    codTribunalCorrente = "",
    codVaraCorrente = "",
    listaDeVaras = "",
    nomeParte = "",
    tipoPesqNome = "F",
    nomeSocial = "",
    tipoPesqNomeSocial = "F",
    nomeMae = "",
    tipoPesqNomeMae = "F",
    nomePai = "",
    tipoPesqNomePai = "F",
    sexo = sexo,
    dataNascimento = "",
    faixaEtariaInicial = "",
    faixaEtariaFinal = "",
    grupoRacial = "-1",
    idEtnia = "0",
    idLinguaFalada = "0",
    numeroRji = "",
    cpfCnpj = "",
    rg = "",
    orgaoRG = "",
    estadoRG = "",
    nacionalidade = "",
    ufNascimento = "-1",
    naturalidade = "",
    pessoaEmSituacaoDeRua = situacao_rua,
    codTribunal = codigo_tribunal,
    descricaoPesq = "",
    descricaoPesqTex = "",
    idClasseProcessual = "386",
    descricaoClasseProcessual = "Execu\xe7\xe3o+da+Pena",
    idAssuntoPrincipal = "",
    descricaoAssuntoPrincipal = "",
    idStatus = "-1",
    idLocalizador = "-1",
    idSituacao = "-1",
    dataInicio = "",
    dataFim = "",
    siglaAdvogado = "",
    tipoProcesso = "",
    regime = "-1",
    temCalculo = "null",
    temExecucaoProvisoria = "null",
    estahEmMedidaDeSeguranca = "null",
    erro = "null",
    estahEmComputoDiferenciado = "null",
    idEntidadeAcolhedora = "",
    descricaoEntidadeAcolhedora = "",
    descricaoUnidadePrisional = "",
    temBeneficioArtigo75 = "null",
    extinto = "null",
    progressaoRegimeVencida = "null",
    livramentoCondicionalVencido = "null",
    dataInicioProgressaoRegime = "",
    dataFimProgressaoRegime = "",
    dataInicioLivramentoCondicional = "",
    dataFimLivramentoCondicional = "",
    pendenteLivramento = "null",
    indeferidoLivramento = "null",
    dataInicioTerminoPena = "",
    dataFimTerminoPena = "",
    pendentePrescricao = "null",
    indeferidoPrescricao = "null",
    dataInicioPrescricao = "",
    dataFimPrescricao = ""
  )

  di <- stringr::str_replace_all(data_inicial,"\\D","_")
  df <- stringr::str_replace_all(data_final,"\\D","_")

  hora <- Sys.time() |> as.numeric()
  
  arquivo <- file.path(diretorio,paste0("execucao_",di,"_",df,"_hora_",hora,"_processos_pagina_1.html"))

  s5 <- httr::POST(url5,body = body, encode = "form",httr::write_disk(arquivo,overwrite = TRUE))

  dividir <- `/`

  paginas <- s5 |>
    httr::content() |>
    xml2::xml_find_first('//*[@class="navLeft"]') |>
    xml2::xml_text() |>
    stringr::str_extract("\\d+") |>
    as.numeric() |>
    dividir(100) |>
    ceiling()

  if (paginas > 1){

    purrr::walk(2:paginas,purrr::possibly(~{

      body$processoPageNumber <- as.character(.x)

      arquivo <- stringr::str_replace(arquivo,"(?<=pagina_)\\d+",as.character(.x))

      httr::POST(url5,body = body, encode = "form",httr::write_disk(arquivo,overwrite = TRUE))

    },NULL))

  }


}

