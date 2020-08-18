#' Ler execucao2
#'
#' @param arquivos Arquivos
#' @param diretorio Diretório
#'
#' @return tibble
#' @export
#'
ler_execucao2 <- function(arquivos = NULL, diretorio = "."){

  if(is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern="^execucao", full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()


    execucao <- stringr::str_extract(.x,"(?<=execucao_)\\d{20}")

    x <- xml2::read_html(.x)

    var <- x %>%
      xml2::xml_find_all("//td[@class='labelRadio']//label|//td[@class='label']//label") %>%
      xml2::xml_text() %>%
      stringr::str_squish()

    val <- x %>%
      xml2::xml_find_all("//td[@class='labelRadio']//label/../following-sibling::td[1]|//td[@class='label']//label/../following-sibling::td[1]") %>%
      xml2::xml_text() %>%
      stringr::str_squish()

    dados <-  tibble::tibble(execucao = execucao,
                             variavel = var,
                             valor = val)


    ## Não há, na página, algo que distiga a cidade, bairro, logradouro, descrição e cep
    ## do fórum dos mesmos dados do sentenciado. A sequência abaixo faz uma pequeno mala
    ## barismo para remover os dados do fórum e manter os do sentenciado.

    dados <-  dados %>%
      dplyr::mutate(id = 1:nrow(dados)) %>%
      dplyr::arrange(desc(id)) %>%
      dplyr::distinct(variavel,.keep_all = TRUE) %>%
      dplyr::filter(stringr::str_detect(variavel,"(?i)Cep",negate=T)) %>%
      dplyr::arrange(id) %>%
      dplyr::mutate(id =NULL) %>%
      dplyr::mutate(valor = stringr::str_remove(valor,"(?i)ajax\\X+")) %>%
      dplyr::filter(stringr::str_detect(valor,"function",negate=TRUE)) %>%
      dplyr::filter(stringr::str_detect(variavel,"^$",negate=TRUE))

  },NULL)) %>%
    dplyr::group_by_at(dplyr::vars(-valor)) %>%
    dplyr::mutate(row_id = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = variavel, value = valor) %>%
    dplyr::select(-row_id) %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(c("assunto_principal","assuntos_secundarios","nivel_de_sigilo"), ~stringr::str_remove(.x,"\\snew\\s$"))) %>%
    tidyr::separate(assunto_principal,c("codigo_assunto_principal","assunto_principal"),sep = " - ",extra="merge") %>%
    tidyr::separate(classe_processual,c("codigo_classe_procesual","classe_processual"),sep = " - ",extra="merge") %>%
    tidyr::separate(assuntos_secundarios,c("codigo_assuntos_secundarios","assuntos_secundarios"),sep = " - ") %>%
    tidyr::separate(cidade, c("cidade","uf"),sep = "/") %>%
    dplyr::relocate(cidade, .after = uf) %>%
    dplyr::mutate(sentenciado = stringr::str_remove(sentenciado,"\\(.+")) %>%
    dplyr::mutate(autuacao = stringr::str_extract(autuacao,"\\S+") %>% lubridate::ymd(.)) %>%
    dplyr::mutate(distribuicao = stringr::str_extract(distribuicao,"\\S+") %>% lubridate::ymd(.)) %>%
    dplyr::mutate(data_de_arquivamento = stringr::str_extract(data_de_arquivamento,"\\S+") %>% lubridate::ymd(.)) %>%
    dplyr::relocate(bairro, .after = cidade) %>%
    dplyr::relocate(logradouro, .after = bairro) %>%
    dplyr::relocate(complemento, .after = logradouro) %>%
    dplyr::relocate(descricao, .before = uf) %>%
    dplyr::relocate(sentenciado, .after =1) %>%
    dplyr::relocate(distribuicao, .after = autuacao)
}
