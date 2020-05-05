#' Ler movimentação processual da execução penal
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório
#'
#' @return tibble
#' @export
#'
ler_movimentacao <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "andamento",full.names = TRUE)

  }


  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{

    processo <- stringr::str_extract(.x,"\\d{20}")

    x <- .x %>%
      xml2::read_html()

    xml2::xml_find_all(x,'//tr[contains(@id,"mov1Grau")]//td') %>%
      xml2::xml_text() %>%
      stringr::str_squish() %>%
      stringr::str_replace("\\X+Ajax\\X+","") %>%
      stringr::str_replace("^$","@@") %>%
      stringr::str_c(collapse="\n") %>%
      stringr::str_split("@@") %>%
      .[[1]] %>%
      stringr::str_subset("^$",negate = TRUE) %>%
      stringr::str_trim() %>%
      tibble::tibble(mov = .) %>%
      tidyr::separate(mov,c("sequencia","data","principal","secundario"),sep = "\n") %>%
      tibble::add_column(processo,.before = 1)


  }),NULL))


}