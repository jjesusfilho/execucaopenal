#' Ler partes
#'
#' @param arquivos Arquivos
#' @param diretorio Diretório
#'
#' @return tibble
#' @export
#'
ler_partes <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,full.names = TRUE, pattern = "parte")

  }

  pb <- progress::progress_bar$new(total  = length(arquivos))


  dados <-  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    execucao <- stringr::str_extract(.x,"\\d{20}")

    x <- xml2::read_html(.x)


    var <- x %>%
      xml2::xml_find_all("//table[@class='form']//td[@class='label']") %>%
      xml2::xml_text(trim = TRUE)

    val <- x %>%
      xml2::xml_find_all("//table[@class='form']//td[@class='label']/following-sibling::td") %>%
      xml2::xml_text(trim = TRUE)

    var <- ifelse(dplyr::lag(var) == "Filiação:" & var =="", "pai",var)

    tibble::tibble(execucao  = execucao,
                   variavel = var,
                   valor = val) %>%
      dplyr::mutate(dplyr::across(.cols=dplyr::everything(),as.character))




  },NULL))

  dados <- dados %>%
    dplyr::group_by_at(dplyr::vars(-valor)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = variavel, value = valor) %>%
    dplyr::select(-row_id) %>%
    janitor::clean_names() %>%
    dplyr::rename(mae = "filiacao")

  if (any(names(dados)=="v1")){

    dados <- dados %>%
      dplyr::mutate(pai = dplyr::coalesce(pai, v1),
                    v1 = NULL)

  }

  ## Se eventualmente, o sexo for vazio, mas a informação estiver em gênero, junta os dois.

  if (any(names(dados)=="genero")){

    dados <- dados %>%
      dplyr::mutate(sexo = dplyr::coalesce(sexo, genero))

  }

  return(dados)
}

