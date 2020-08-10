#' Ler números dos processos baixas a partir do script /Inst/baixar_numeros
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Inforrmar diretório se arquivos for NULL
#'
#' @return tibble
#' @export
#'
ler_numeros <- function(arquivos = NULL,diretorio="."){

  if (is.null(arquivos)){

    arquivos <-list.files(diretorio,pattern="pagina",full.names=T,recursive=T)

  }

  purrr::map_dfr(arquivos, purrr::possibly(~{


    x <- xml2::read_html(.x)

    processo <- x %>%
      xml2::xml_find_all("//em") %>%
      xml2::xml_text()

    situacao <- x %>%
      xml2::xml_find_all("//em") %>%
      xml2::xml_attr("class")

    apenado <- x %>%
      xml2::xml_find_all("//tr//td//li") %>%
      xml2::xml_text() %>%
      stringr::str_subset("Rio de Janeiro",negate=T)

    data_distribuicao <- x %>%
      xml2::xml_find_all("//td[@style='text-align:center'][1]") %>%
      xml2::xml_text(trim=TRUE)

    classe_processual <- x %>%
      xml2::xml_find_all("//td[@style='text-align:center'][2]") %>%
      xml2::xml_text(trim=TRUE)



    tibble::tibble(processo,apenado,situacao,data_distribuicao,classe_processual) %>%
      tidyr::separate(classe_processual,c("classe1","classe2"),sep = "\\(") %>%
      dplyr::mutate(classe2 =  stringr::str_remove(classe2,"\\)")) %>%
      dplyr::mutate_all(stringr::str_trim) %>%
      dplyr::mutate(data_distribuicao = lubridate::dmy(data_distribuicao))


  },NULL))



}
