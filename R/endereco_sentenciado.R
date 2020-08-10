#' Endereco do sentenciado
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar onde se encontram os arquivos, se param arquivos for NULL
#'
#' @return tibble
#' @export
#'
endereco_sentenciado <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "html$",full.names = TRUE)

  }


  purrr::map_dfr(arquivos,purrr::possibly(~{

    processo <- stringr::str_extract(.x,"\\d{20}")
    x <- .x %>%
      xml2::read_html()

    tb <- x %>%
      xml2::xml_find_all('//table//table[@class="form"]') %>%
      rvest::html_table(fill =TRUE) %>%
      .[[2]] %>%
      setNames(c("variavel","valor")) %>%
      #dplyr::mutate(variavel = ifelse(variavel =="",paste0(lag(variavel),"_2"),variavel)) %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>% dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id) %>%
      janitor::clean_names() %>%
      tibble::add_column(processo, .before = 1)


  },NULL))


}
