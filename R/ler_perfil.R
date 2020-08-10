#' Extrai perfil do sentenciado
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se arquivos for NULL
#' @param organizar Exclui colunas pouco relevantes e transforma
#'     dados, normalizando nomes e padronizando datas
#'
#' @details O argumento organizar pode provocar erro se forem lidos um ou poucos
#'     processos, pois nem todas as colunas aparecem. Assim, para ler um ou dois
#'     ajuste para FALSE. No entanto, os dados não estarão bem padronizados.
#'
#' @return tibble
#' @export
#'
ler_perfil <- function(arquivos = NULL, diretorio = ".", organizar = TRUE){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "html$",full.names = TRUE)

  }


  df <- purrr::map_dfr(arquivos,purrr::possibly(~{

    processo <- stringr::str_extract(.x,"\\d{20}")
    x <- .x %>%
      xml2::read_html()

    x %>%
      xml2::xml_find_first('//table[@class="form"]') %>%
      rvest::html_table(fill =TRUE) %>%
      setNames(c("variavel","valor")) %>%
      dplyr::mutate(variavel = ifelse(variavel =="",paste0(lag(variavel),"_2"),variavel)) %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>% dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>% tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id) %>%
      janitor::clean_names() %>%
      tibble::add_column(processo, .before = 1)


  },NULL))

  if (organizar == TRUE){

    df <-  df  %>%
      dplyr::mutate(data_de_nascimento = stringr::str_remove(data_de_nascimento,"\\s.+") %>%
                      lubridate::dmy()) %>%
      tidyr::separate(naturalidade,c("naturalidade","uf_naturalidade"),
                      sep = "/",
                      remove = TRUE,
                      fill= "right") %>%

      #  dplyr::select(processo,nome, dplyr::everything()) %>%
      dplyr::select(processo,nome,cpf_cnpj,escolaridade,estado_civil,mae = filiacao,
                    pai = filiacao_2,naturalidade,uf_naturalidade,raca,sexo,
                    tipo_parte = tipo_da_parte, data_nascimento = data_de_nascimento)

  }


}
