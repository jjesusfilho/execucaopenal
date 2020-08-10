#' Ler dados da execução penal
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar diretório, se arquivos for NULL
#'
#' @return tibble
#' @export
#'
ler_execucao <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,full.names = TRUE,pattern="execucao",recursive = TRUE)
  }

  purrr::map_dfr(arquivos,purrr::possibly(~{

    processo <- stringr::str_extract(.x,"\\d{20}")


    x <- xml2::read_html(.x)

    id_execucao <- x %>%
      xml2::xml_find_first("//form[@name='processoForm']")  %>%
      xml2::xml_attr("action") %>%
      stringr::str_extract("\\d+$")

    variavel <- xml2::xml_find_all(x,"//td[@class='label']/label") %>%
      xml2::xml_text()

    valor <- xml2::xml_find_all(x,"//td[@class='label'][label]/following-sibling::td[1]") %>%
      xml2::xml_text()

    tibble::tibble(processo = processo,id_execucao = id_execucao, variavel = variavel, valor = valor) %>%
      dplyr::mutate(valor = stringr::str_trim(valor) %>%
                      stringr::str_remove("\n\\X+")) %>%
      dplyr::filter(!stringr::str_detect(variavel,"Complemento")) %>%
      dplyr::mutate(variavel = ifelse(variavel == "Logradouro:",
                                      paste(variavel,
                                            dplyr::lag(valor)),variavel)) %>%
      dplyr:: mutate(variavel = ifelse(variavel == "Bairro:",paste(variavel,dplyr::lag(valor,2)),variavel)) %>%
      dplyr::mutate(variavel = ifelse(variavel == "Cidade:",paste(variavel,dplyr::lag(valor,3)),variavel)) %>%
      dplyr::filter(!stringr::str_detect(variavel,"Não Cadastrada|CEP|Descrição|Exibir Decretos:")) %>%
      dplyr::mutate(variavel = stringr::str_remove(variavel,"Endereço\\s?")) %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id) %>%
      janitor::clean_names() %>%
      dplyr::select(-c(v1,x)) %>%
      tidyr::separate(sentenciado,
                      c("nome_sentenciado","dados_sentenciado"),sep = "\\s\\(",
                      extra= "merge") %>%
      dplyr::select(-dplyr::matches("cidade_\\d+"))


  },NULL))

}

