#' Processos criminais dos sentenciados
#'
#' @param arquivos Arquivos a serem lidos
#' @param diretorio Diretório, se arquivos for NULL
#'
#' @return tibble
#' @export
#'
ler_processo_criminal <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "html$",full.names = TRUE)

  }


  purrr::map_dfr(arquivos, purrr::possibly(~{

    processo <- stringr::str_extract(.x,"\\d{20}")

    x <- .x %>%
      xml2::read_html()

    tb <- x %>%
      xml2::xml_find_first('//table[@class="form"]') %>%
      rvest::html_table(fill = TRUE) %>%
      dplyr::select(1:2) %>%
      setNames(c("variavel","valor")) %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>% dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>% tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id) %>%
      janitor::clean_names() %>%
      tibble::add_column(processo, .before = 1) %>%
      dplyr::mutate(data_da_infracao = lubridate::dmy(data_da_infracao),
                    data_da_autuacao = lubridate::dmy(data_da_autuacao),
                    data_da_distribuicao = lubridate::dmy(data_da_distribuicao))

    tb$pena_imposta <- NULL
    pena <- x %>%
      xml2::xml_find_first("//td[@class='resultBorder']//table") %>%
      rvest::html_table(fill=TRUE) %>%
      janitor::clean_names()

    tb %>%
      tibble::add_column(pena_imposta = pena$pena_imposta, regime_da_pena = pena$regime_de_pena, concedido = pena$concedido,.after = "artigo_da_condenacao")



  },NULL))


}
