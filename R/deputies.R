#' Get biographic information on all deputies since the foundation of the Chamber
#'
#' @export

pega_parlamentares <- function(){

  suppressMessages(

    "https://dadosabertos.camara.leg.br/arquivos/deputados/csv/deputados.csv" %>%
      readr::read_csv2(col_types = readr::cols(
        uri = readr::col_character(),
        nome = readr::col_character(),
        idLegislaturaInicial = readr::col_double(),
        idLegislaturaFinal = readr::col_double(),
        nomeCivil = readr::col_character(),
        cpf = readr::col_character(),
        siglaSexo = readr::col_character(),
        urlRedeSocial = readr::col_character(),
        urlWebsite = readr::col_character(),
        dataNascimento = readr::col_date(format = ""),
        dataFalecimento = readr::col_date(format = ""),
        ufNascimento = readr::col_character(),
        municipioNascimento = readr::col_character()
      )) %>%
      janitor::clean_names() %>%
      dplyr::rename("id" = .data$uri, "sexo" = .data$sigla_sexo) %>%
      dplyr::mutate("id" = stringr::str_remove_all(.data$id, "https://dadosabertos.camara.leg.br/api/v2/deputados/")) %>%
      dplyr::mutate("sexo" = ifelse(.data$sexo == "M", "Masculino", "Feminino")) %>%
      dplyr::select(.data$id, .data$nome:.data$nome_civil, .data$sexo, .data$data_nascimento)
  )
}
