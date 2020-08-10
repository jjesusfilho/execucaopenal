#' Constrói índice das páginas dos pdfs baixados
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Diretório de arquivos, se arquivos for NULL
#'
#' @return tibble
#' @export
#'
ler_doc_indice <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "pdf$",full.names = TRUE)

  }


  purrr::map_dfr(arquivos, purrr::possibly(~{


    numero <- stringr::str_extract(.x,"\\d+(?=\\.pdf)")

    x <- pdftools::pdf_text(.x)

    processo <- stringr::str_squish(x[1]) %>%
      stringr::str_extract("(?<=Processo\\s).+?(?=\\sComarca)") %>%
      stringr::str_remove_all("\\D+")


    purrr::map_dfr(x[-1],purrr::possibly(~{

      p <-  stringr::str_match(.x,"(\\d{2}/\\d{2}/\\d{4})(?:\\:)(.+)")[2:3]

      pagina <- stringr::str_extract(.x,"(?<=Página\\s)\\d+") %>%
        as.integer()

      data <-lubridate::dmy(p[1])

      doc <- stringr::str_squish(p[2])

      tibble::tibble(pagina = pagina, data = data, doc = p[2])

    },NULL)) %>%
      tibble::add_column(processo = processo, numero = numero, .before = 1) %>%
      tibble::add_row(processo = processo,
                      numero = numero,
                      pagina = 1L,
                      data = NA,
                      doc = "capa",
                      .before = 1) %>%
      tibble::add_column(texto = x) %>%
      tidyr::fill(data,.direction="up")



  },NULL))


}
