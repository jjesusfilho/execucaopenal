#' Extrair dados da carta de sentença
#'
#' @param strings carta de sentença lido com pdftools
#'
#' @return tibble
#' @export
#'
extrair_carta_sentenca <- function(strings = NULL){

  purrr::map_dfr(strings, purrr::possibly(purrrogress::with_progress(~{

    processo <- stringr::str_extract(carta,"(?<=Processo:).+?(?=\\s)") %>%
      stringr::str_remove_all("\\D")

    data_emissao <- stringr::str_extract(carta,"(?<=Emissão:).+?(?=\\s)") %>%
      stringr::str_trim() %>%
      lubridate::dmy()

    nome <- stringr::str_extract(carta,"(?<=Nome:).+") %>%
      stringr::str_trim()


    mae <- stringr::str_extract(carta,"(?<=Mãe:).+") %>%
      stringr::str_trim()

    pai <- stringr::str_extract(carta,"(?<=Pai:).+") %>%
      stringr::str_trim()

    data_nascimento <- stringr::str_extract(carta,"(?<=Nasc\\.:).+?\\s") %>%
      stringr::str_trim() %>%
      lubridate::dmy()

    local_nascimento <- stringr::str_extract(carta,"(?<=Cidade/UF:).+?(?=País)") %>%
      stringr::str_trim()

    pais_nascimento <- stringr::str_extract(carta,"(?<=País:).+") %>%
      stringr::str_trim()

    sexo <- stringr::str_extract(carta,"(?<=Sexo:).+") %>%
      stringr::str_trim()

    cor <- stringr::str_extract(carta,"(?<=Cor:).+") %>%
      stringr::str_trim()

    instrucao <- stringr::str_extract(carta,"(?<=Instrução:).+") %>%
      stringr::str_trim()

    estado_civil <- stringr::str_extract(carta,"(?<=Civil:).+") %>%
      stringr::str_trim()

    logradouro_residencia <- stringr::str_extract(carta,"(?<=Logradouro:).+") %>%
      stringr::str_trim()

    bairro_residencia <- stringr::str_extract(carta,"(?<=Bairro:).+") %>%
      stringr::str_trim()

    cidade_residencia <- stringr::str_extract(carta,"(?<=Cidade:).+") %>%
      stringr::str_trim()

    uf_residencia <- stringr::str_extract(carta,"(?<=UF:).+") %>%
      stringr::str_trim()


    processo_criminal <- stringr::str_extract(carta,"(?<=na V\\.O\\.:).+?(?=V)") %>%
      stringr::str_remove_all("\\D")

    vara_criminal <- stringr::str_match(carta,"(?:V\\.O\\.\\:)(?:.+?)(?:V\\.O\\.\\:)(.+)") %>%
      .[,2] %>%
      stringr::str_trim()


    data_delito <- stringr::str_extract(carta,"(?<=Data do Delito:\\s).+?(?=\\s)") %>%
      lubridate::dmy()

    data_denuncia <- stringr::str_extract(carta,"(?<=Data da Denúncia:\\s).+?(?=\\s)") %>%
      lubridate::dmy()

    data_condencao <- stringr::str_extract(carta,"(?<=Cond\\.:).+") %>%
      lubridate::dmy()



  }),NULL))
}
