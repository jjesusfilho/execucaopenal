#' Extrair qualificação do atestado de pena
#'
#' @param indice dataframe extraído com ler_índice
#'
#' @return tibble
#' @export
#'
extrair_qualificacao <- function (indice)
{
  indice <- indice %>% dplyr::filter(stringr::str_detect(doc,
                                                         "atestado de pena"))
  processo <- unique(indice$processo)

  id_execucao <- unique(indice$numero)

  atestado <- indice$texto[1]

  gerado_em <-
    stringr::str_extract(indice$texto[1], "(?<=Gerado em:).+?(?=por)") %>%
    stringr::str_trim()

  local_prisao <-
    stringr::str_extract(atestado, "Local de Prisão.+") %>%
    stringr::str_remove(".+?:") %>%
    stringr::str_trim()

  codigo <- stringr::str_extract(atestado, "(?<=Código:).+") %>%
    stringr::str_trim()

  nome <- stringr::str_extract(atestado, "(?<=Nome:).+") %>%
    stringr::str_trim()

  mae <- stringr::str_extract(atestado, "(?<=Mãe:).+") %>%
    stringr::str_trim()

  pai <- stringr::str_extract(atestado, "(?<=Pai:).+") %>%
    stringr::str_trim()

  data_nascimento <-
    stringr::str_extract(atestado, "(?<=Nascimento:).+(?=Naturalidade)") %>%
    stringr::str_trim() %>% lubridate::dmy()

  sexo <- stringr::str_extract(atestado, "(?<=Sexo:).+") %>%
    stringr::str_trim()

  naturalidade <-
    stringr::str_extract(atestado, "(?<=Naturalidade:).+") %>%
    stringr::str_trim()

  nacionalidade <-
    stringr::str_extract(atestado, "(?<=Nacionalidade:).+?(?=Estado)") %>%
    stringr::str_trim()

  escolaridade <-
    stringr::str_extract(atestado, "(?<=Escolaridade:).+(?=Profissão)") %>%
    stringr::str_trim()

  profissao <-
    stringr::str_extract(atestado, "(?<=Profissão:).+") %>%
    stringr::str_trim()

  estado_civil <-
    stringr::str_extract(atestado, "(?<=Civil:).+") %>%
    stringr::str_trim()

  pena_imposta <-
    stringr::str_extract(atestado, "(?<=Imposta:).+") %>%
    stringr::str_trim()

  tibble::tibble(
    processo = processo,
    id_execucao = id_execucao,
    data_atestado = gerado_em,
    local_prisao = local_prisao,
    codigo = codigo,
    nome = nome,
    mae = mae,
    pai = pai,
    data_nascimento = data_nascimento,
    sexo = sexo,
    naturalidade = naturalidade,
    nacionalidade = nacionalidade,
    profissao = profissao,
    estado_civil = estado_civil,
    pena_imposta = pena_imposta,
  )
}
