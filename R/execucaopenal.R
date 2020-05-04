#' \code{execucaopenal} package
#'
#' Baixa  e organiza decisões da vec do TJRJ
#'
#'
#' @docType package
#' @name execucaopenal
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    c("cpf_cnpj", "data_de_nascimento", "escolaridade", "estado_civil",
      "filiacao", "filiacao_2", "naturalidade", "nome", "processo",
      "raca", "sexo", "tipo_da_parte", "uf_naturalidade")))
}
