#' Get bills' topics
#'
#' @param anos A vector of years.
#' @param wide If \code{TRUE}, returns a wide table
#'
#' @export

pega_temas <- function(anos, wide = TRUE){

  # Baixa os temas
  temas <- suppressMessages(

    "https://dadosabertos.camara.leg.br/arquivos/proposicoesTemas/csv/proposicoesTemas-%s.csv" %>%
      sprintf(anos) %>%
      purrr::map(readr::read_csv2) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate("uriProposicao" = stringr::str_remove(.data$uriProposicao, "https://dadosabertos.camara.leg.br/api/v2/proposicoes/") %>%
                      as.numeric) %>%
      dplyr::select(.data$uriProposicao, .data$tema)
  )

  # Ajusta o formato
  if(wide){

    temas <- temas %>%
      dplyr::mutate(valor = 1) %>%
      tidyr::pivot_wider(names_from = .data$tema, values_from = .data$valor) %>%
      janitor::clean_names() %>%
      dplyr::mutate_if(is.numeric, ~ ifelse(is.na(.), 0, .))

  } else {

    temas <- temas %>%
      janitor::clean_names()
  }

  # Retorna
  return(temas)
}


#' Get bills
#'
#' @param anos A vector of years
#' @param filtra_tipo If \code{TRUE}, returns only PL, PLP, MPV and PEC
#' @param temas If \code{TRUE}, returns bills' topics as variables
#' @param autores If \code{TRUE}, returns authors' informations as variables
#'
#' @export

pega_proposicoes <- function(anos, filtra_tipo = TRUE, temas = FALSE, autores = FALSE){

  # Baixa as proposicoes
  props <- suppressMessages(

    "https://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-%s.csv" %>%
      sprintf(anos) %>%
      purrr::map(readr::read_csv2) %>%
      dplyr::bind_rows() %>%
      dplyr::select(.data$dataApresentacao, .data$id, .data$siglaTipo,
                    .data$descricaoTipo, .data$ultimoStatus_descricaoTramitacao,
                    .data$ementa, .data$ultimoStatus_dataHora, .data$ultimoStatus_siglaOrgao,
                    .data$ultimoStatus_regime, .data$ultimoStatus_descricaoSituacao,
                    .data$ultimoStatus_despacho) %>%
      janitor::clean_names()
  )

  # Filtro
  if(filtra_tipo) props <- dplyr::filter(props, .data$sigla_tipo %in% c("PLP", "MPV", "PEC", "PL"))

  # Autores
  if(autores){

    autores <- pega_autor(anos)
    props <- dplyr::left_join(props, autores, by = c("id" = "id_proposicao"))
  }

  # Temas
  if(temas){

    temas <- pega_temas(anos)
    props <- dplyr::left_join(props, temas, by = c("id" = "uri_proposicao"))
  }

  # Retorna
  return(props)
}


#' Get bills' authors
#'
#' @param anos A vector of years
#' @param filtra_deputados If \code{TRUE}, returns only deputies
#' @param proponente_principal If \code{TRUE}, returns only main authors
#'
#' @export

pega_autor <- function(anos, filtra_deputados = FALSE, proponente_principal = TRUE){

  autores <- suppressMessages(

    "https://dadosabertos.camara.leg.br/arquivos/proposicoesAutores/csv/proposicoesAutores-%s.csv" %>%
      sprintf(anos) %>%
      purrr::map(readr::read_csv2) %>%
      dplyr::bind_rows() %>%
      dplyr::select(.data$idProposicao, .data$uriAutor, .data$idDeputadoAutor,
                    .data$tipoAutor, .data$nomeAutor, .data$siglaPartidoAutor,
                    .data$siglaUFAutor, .data$proponente)
  )

  # Filtros
  if(filtra_deputados){

    autores <- dplyr::filter(autores, .data$tipoAutor == "Deputado") %>%
      dplyr::select(-.data$tipoAutor)
  }

  if(proponente_principal){

    autores <- dplyr::filter(autores, .data$proponente == 1) %>%
      dplyr::select(-.data$proponente)
  }

  # Retorna
  autores %>%
    dplyr::distinct(.data$idProposicao, .keep_all = TRUE) %>%
    janitor::clean_names()
}


#' Pega tramitacao
#'
#' @param id ID da proposicao
#'
#' @export

pega_tramitacao <- function(id){

  message("ID: ", id)
  httr::modify_url("https://dadosabertos.camara.leg.br/", path = paste0("api/v2/proposicoes/", id, "/tramitacoes")) %>%
    get_data(data_frame = FALSE)
}


#' Pega a tramitacao de mais de uma proposicao
#'
#' @param ids ID da proposicao
#'
#' @export

pega_tramitacoes <- function(ids){

  pega_tramitacao_safe <- purrr::possibly(pega_tramitacao, NULL)
  ids %>%
    purrr::map(pega_tramitacao_safe) %>%
    dplyr::bind_rows()
}



