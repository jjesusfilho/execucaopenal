#' Ler conhecimento
#'
#' @param arquivos arquivos
#' @param diretorio diretorio
#'
#' @return tibble
#' @export
#'
ler_conhecimento <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,full.names = TRUE, pattern="criminal")

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    crime <- stringr::str_extract(.x,"\\d{20}")
    execucao <- stringr::str_extract(.x,"(?<=execucao_)\\d+")

    x <- xml2::read_html(.x)

    var <-   x %>%
      xml2::xml_find_all("//table//td[@class='label']") %>%
      xml2::xml_text(trim=TRUE)

    val <- x %>%
      xml2::xml_find_all("//table//td[@class='label']/following-sibling::td[1]") %>%
      xml2::xml_text(trim=TRUE)

    tibble::tibble(processo_criminal = crime, execucao = execucao,variavel= var, valor = val)

  },NULL)) %>%
    dplyr::group_by_at(dplyr::vars(-valor)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = variavel, value = valor) %>%
    dplyr::select(-row_id) %>%
    janitor::clean_names()
}
