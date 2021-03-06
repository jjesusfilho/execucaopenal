
<!-- README.md is generated from README.Rmd. Please edit that file -->

# execucaopenal

<!-- badges: start -->

[![R build
status](https://github.com/jjesusfilho/execucaopenal/workflows/R-CMD-check/badge.svg)](https://github.com/jjesusfilho/execucaopenal/actions)
<!-- badges: end -->

O objetivo deste projeto/pacote é coletar e organizar os dados da
execução penal do Rio de Janeiro.

## Instalação

Para instalar o pacote, rodar o seguinte comando no R:

``` r
devtools::install_github("jjesusfilho/execucaopenal")
```

## Coleta

A coleta está sendo realizada por intervalos de datas. Para a realizar a
coleta, foi necessário instalar uma versão conteneirizada do Selenium.
Para informações sobre como instalar tal versão, seguir [este
tutorial](https://rpubs.com/johndharrison/RSelenium-Docker).

Além disso, é necessário configurar o Docker para rodar o conteiner sem
sudo. Instruções podem ser obtidas
[aqui](https://docs.docker.com/engine/install/linux-postinstall/)

A coleta se dá em duas etapas. Primeiramente, baixamos os números dos
processos conforme a data. Para baixar os processos, é necessário
iniciar o conteiner do Selenium, carregar o RSelenium e chamar a função
baixar\_numeros, que é interna. Esta função baixará os números dos
processos conforme as dadas indicadas. Depois disso, é necessário
converter os htmls baixados para tabela, como os respectivos números dos
processos e nomes dos sentenciados.

Somente com os números dos processos, é possível passar para a etapa
seguinte que é baixar os dados da execução penal de cada um dos
sentenciados. Para tanto, há um script chamado baixar\_processos, que
está na pasta Inst. Assim como a função baixar\_numeros, ele depende de
o conteiner do Selenium estar rodando e do RSelenium carregado.

Este escript cria quatro diretórios para cada processo com nomes
similares a esses: dados\_execucao, perfil, andamento,
processo\_criminal. De tais pastas, iniciamos o processo de leitura e
organização.

## Leitura e organização dos dados

A leitura dos dados é realizada por meio das funções que iniciam com a
palavar ler, exceto ler\_numeros que já foi utilizada anteriormente.

### Execução

Para ler os dados da execução penal, é recomendável antes criar um vetor
dos arquivos. Para tanto, rode o seguinte comando:

``` r
diretorio = "data-raw/processos"
arquivos <- list.files(diretorio, pattern = "execucao", recursive = TRUE, full.names = TRUE)
```

Depois disso, basta chamar a função:

``` r
execucao <- ler_execucao(arquivos)
```

### Perfil

O procedimento é similar para ler os dados do perfil:

``` r
diretorio = "data-raw/processos"
arquivos <- list.files(diretorio, pattern = "perfil", recursive = TRUE, full.names = TRUE)
```

Depois disso, basta chamar a função:

``` r
perfil <- ler_perfil(arquivos)
```

### Processos criminais

Para ler os processos criminais que geraram as condenações, é necessário
usar a função ler\_processo\_criminal:

``` r
diretorio = "data-raw/processos"
arquivos <- list.files(diretorio, pattern = "processo", recursive = TRUE, full.names = TRUE)
```

Depois disso, basta chamar a função:

``` r
processos <- ler_processo_criminal(arquivos)
```

### Movimentação

``` r
diretorio = "data-raw/processos"
arquivos <- list.files(diretorio, pattern = "andamento", recursive = TRUE, full.names = TRUE)
```

Depois disso, basta chamar a função:

``` r
movimentacao <- ler_movimentacao(arquivos)
```

## Base de dados

Para este projeto, estamos armazenando todas as tabelas criadas em uma
base de dados SQL (PostgreSQL), a fim de que os dados possam ser
consumidos por uma aplicação.

## Afazeres

Há uma série de tarefas que precisam ser realizadas. Listamos algumas.

1.  Criar aplicativo simples para realizar a validação humana da leitura
    dos dados.
2.  As informações sobre os crimes não estão estruturadas adequadamente.
    Estou montando uma função para classificar adequadamente a natureza
    do crime. Esta classificação necessitará da validação humana.
3.  Verifiquei que o sexo de muitas pessoas parece estar errado. Será
    necessário verificar o sexo conforme a base do IBGE e, para aqueles
    nomes em que o IBGE não informar o sexo ou for diferente da base do
    vec do TJRJ, será necessário realizar a verificação não
    automatizada.
4.  Realizar o georreferenciamento dos endereços dos sentenciados. O
    geocode do Google permite consultas de apenas 2500 endereços por
    dia.
5.  Montar um desenho da base de dados que corresponda à necessidade dos
    pesquisadores.
6.  Montar um aplicativo para visualização dos resultados.
