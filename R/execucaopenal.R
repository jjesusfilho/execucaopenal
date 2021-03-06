#' \code{execucaopenal} package
#'
#' Baixa  e organiza execução penal do Rio de Janeiro
#'
#'
#' @docType package
#' @name execucaopenal
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("doc","cpf_cnpj", "data_de_nascimento", "escolaridade", "estado_civil",
                           "filiacao", "filiacao_2", "naturalidade", "nome", "processo",
                           "raca", "sexo", "tipo_da_parte", "uf_naturalidade",
                           ".", "assunto_principal", "assuntos_secundarios", "autuacao",
                           "bairro", "cidade", "classe_processual", "complemento", "data_de_arquivamento",
                           "descricao", "distribuicao", "genero", "logradouro", "pai", "row_id",
                           "sentenciado", "uf", "v1", "valor", "variavel"))
}
