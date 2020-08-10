#' Extrair dados da carta de sentença
#'
#' @param strings carta de sentença lido com pdftools
#'
#' @return tibble
#' @export
#'
extrair_carta_sentenca <- function(strings = NULL){

  purrr::map_dfr(strings, purrr::possibly(~{

    processo <- stringr::str_extract(.x,"(?<=Processo:).+?(?=\\s)") %>%
      stringr::str_remove_all("\\D")

    data_emissao <- stringr::str_extract(.x,"(?<=Emissão:).+?(?=\\s)") %>%
      stringr::str_trim() %>%
      lubridate::dmy()

    nome <- stringr::str_extract(.x,"(?<=Nome:).+") %>%
      stringr::str_trim()


    mae <- stringr::str_extract(.x,"(?<=Mãe:).+") %>%
      stringr::str_trim()

    pai <- stringr::str_extract(.x,"(?<=Pai:).+") %>%
      stringr::str_trim()

    data_nascimento <- stringr::str_extract(.x,"(?<=Nasc\\.:).+?\\s") %>%
      stringr::str_trim() %>%
      lubridate::dmy()

    local_nascimento <- stringr::str_extract(.x,"(?<=Cidade/UF:).+?(?=País)") %>%
      stringr::str_trim()

    pais_nascimento <- stringr::str_extract(.x,"(?<=País:).+") %>%
      stringr::str_trim()

    sexo <- stringr::str_extract(.x,"(?<=Sexo:).+?(?=\\s)") %>%
      stringr::str_trim()

    cor <- stringr::str_extract(.x,"(?<=Cor:).+?(?=\\s)") %>%
      stringr::str_trim()

    instrucao <- stringr::str_extract(.x,"(?<=Instrução:).+?(?=Profiss)") %>%
      stringr::str_trim()

    profissao <- stringr::str_extract(.x,"(?<=Profissão:).+?(?=\\s)") %>%
      stringr::str_trim()

    estado_civil <- stringr::str_extract(.x,"(?<=Civil:).+") %>%
      stringr::str_trim()

    logradouro_residencia <- stringr::str_extract(.x,"(?<=Logradouro:).+") %>%
      stringr::str_trim()

    bairro_residencia <- stringr::str_extract(.x,"(?<=Bairro:).+") %>%
      stringr::str_trim()

    cidade_residencia <- stringr::str_extract(.x,"(?<=Cidade:).+?(?=UF:)") %>%
      stringr::str_trim()

    uf_residencia <- stringr::str_extract(.x,"(?<=\\sUF:).+?(?=CEP)") %>%
      stringr::str_trim()

    cep_residencia <-stringr::str_extract(.x,"(?<=\\sCEP:).+") %>%
      stringr::str_trim()

    processo_criminal <- stringr::str_extract(.x,"(?<=na V\\.O\\.:).+?(?=V)") %>%
      stringr::str_remove_all("\\D")

    vara_criminal <- stringr::str_match(.x,"(?:V\\.O\\.\\:)(?:.+?)(?:V\\.O\\.\\:)(.+)") %>%
      .[,2] %>%
      stringr::str_trim()


    data_delito <- stringr::str_extract(.x,"(?<=Data do Delito:\\s).+?(?=\\s)") %>%
      lubridate::dmy()

    data_denuncia <- stringr::str_extract(.x,"(?<=Data da Denúncia:\\s).+?(?=\\s)") %>%
      lubridate::dmy()

    data_condenacao <- stringr::str_extract(.x,"(?<=Cond\\.:).+") %>%
      lubridate::dmy()

    tibble::tibble(processo = processo,
                   data_emissao = data_emissao,
                   nome = nome,
                   mae = mae,
                   pai = pai,
                   data_nascimento = data_nascimento,
                   local_nascimento = local_nascimento,
                   pais_nascimento = pais_nascimento,
                   sexo = sexo,
                   instrucao = instrucao,
                   profissao = profissao,
                   estado_civil = estado_civil,
                   logradouro_residencia = logradouro_residencia,
                   bairro_residencia = bairro_residencia,
                   cidade_residencia = cidade_residencia,
                   uf_residencia = uf_residencia,
                   cep_residencia = cep_residencia,
                   processo_criminal = processo_criminal,
                   vara_criminal = vara_criminal,
                   data_delito = data_delito,
                   data_condencao = data_condenacao)

  },NULL))
}
