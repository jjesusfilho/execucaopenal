#' Extrair penas dos atestados
#'
#' @param indice data.frame extraído com ler_indice
#'
#' @return tibble
#' @export
#'
extrair_penas <- function(indice = NULL){

  indice <-  indice %>%
    dplyr::filter(stringr::str_detect(doc,"atestado de pena"))

  processo <- unique(indice$processo)

  id_execucao <- unique(indice$numero)

  gerado_em <- stringr::str_extract(indice$texto[1],"(?<=Gerado em:).+?(?=por)") %>%
    stringr::str_trim()

  textos <- indice$texto %>%
    stringr::str_subset("AÇÃO PENAL:\\X+PEÇAS") %>%
    stringr::str_extract_all("AÇÃO PENAL:\\X+?PEÇAS") %>%
    unlist()

  purrr::map_dfr(textos,~{


    acao_penal <- stringr::str_extract(.x,"(?<=AÇÃO PENAL:).+") %>%
      stringr::str_trim()

    codigo_legado <- stringr::str_extract(.x,"(?<=Legado:).+") %>%
      stringr::str_trim()

    vara_origem <- stringr::str_extract(.x,"(?<=Origem:).+") %>%
      stringr::str_trim()

    data_autuacao <- stringr::str_extract(.x,"(?<=Autuação:).+(?=Data)") %>%
      stringr::str_trim() %>%
      lubridate::dmy()

    data_infracao <- stringr::str_extract(.x,"(?<=Infração).+") %>%
      stringr::str_trim() %>%
      lubridate::dmy()

    data_rec_denuncia <- stringr::str_extract(.x,"(?<=Denúncia:).+(?=Data)") %>%
      stringr::str_trim() %>%
      lubridate::dmy()

    data_transito_julgado <- stringr::str_extract(.x,"(?<=Julgado:).+?(?=Reincidente)") %>%
      stringr::str_trim() %>%
      lubridate::dmy()
    artigo_condenacao <- stringr::str_extract(.x,"(?<=Condenação).+") %>%
      stringr::str_trim()

    complemento_artigo = stringr::str_extract(.,"(?<=Complemento do Artigo:).+") %>%
      stringr::str_trim()

    observacao = stringr::str_extract(.x,"(?<=Observação:).+") %>%
      stringr::str_trim()

    pena_originaria <- stringr::str_extract(.x,"(?<=Pena:).+") %>%
      stringr::str_trim()

    valor_multa <- stringr::str_extract(.x,"(?<=Valor da Multa:).+?(?=Dias)") %>%
      stringr::str_trim()

    dias_multa <- stringr::str_extract(.x,"(?<=Dias/Multa:).+") %>%
      stringr::str_trim()

    regime_inicial <- stringr::str_extract(.x,"(?<=Regime:).+?(?=Ativa)") %>%
      stringr::str_trim()

    ativa <- stringr::str_extract(.x,"(?<=Ativa:).+") %>%
      stringr::str_trim()

    artigos <- stringr::str_extract(.x,"(?<=Artigos:).+") %>%
      stringr::str_trim()



    tibble::tibble(
      acao_penal = acao_penal,
      codigo_legado = codigo_legado,
      vara_origem = vara_origem,
      data_autuacao = data_autuacao,
      data_infracao = data_infracao,
      data_rec_denuncia = data_rec_denuncia,
      data_transito_julgado = data_transito_julgado,
      artigo_condenacao = artigo_condenacao,
      complemento_artigo = complemento_artigo,
      observacao = observacao,
      pena = pena_originaria,
      valor_multa = valor_multa,
      dias_multa = dias_multa,
      regime_inicial = regime_inicial,
      ativa = ativa,
      artigos = artigos
    )


  }) %>%
    tibble::add_column(processo  = processo,
                       id_execucao = id_execucao,
                       data_atestado= gerado_em,
                       .before = 1)



}
