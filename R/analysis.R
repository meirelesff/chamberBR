#' Calculate Parties' Rice Index
#'
#' @param ano Year.
#' @param unidade Unit to aggregate data.
#'
#' @export

rice_camara <- function(ano, unidade = "bimonth"){

  # Baixa os dados
  votos <- pega_votos(ano)
  votacoes <- pega_votacoes(ano) %>%
    dplyr::select(.data$id, .data$data)

  # Calculo
  res <- suppressMessages(

    dplyr::left_join(votos, votacoes, by = c("id_votacao" = "id")) %>%
      dplyr::filter(.data$voto != "Artigo 17") %>%
      dplyr::filter(!.data$orientacao_bancada %in% c("Liberado", "Obstrução")) %>%
      dplyr::mutate(mes = lubridate::round_date(.data$data, unidade)) %>%
      dplyr::group_by(.data$deputado_sigla_partido, .data$mes, .data$id_votacao) %>%
      dplyr::summarise(rice = abs(sum(.data$voto == "Sim") - sum(.data$voto != "Sim")) / (sum(.data$voto == "Sim") + sum(.data$voto != "Sim")) ) %>%
      dplyr::group_by(.data$deputado_sigla_partido, .data$mes) %>%
      dplyr::summarise(rice = mean(.data$rice)) %>%
      dplyr::arrange(-.data$rice)
  )

  # Retorna
  return(res)
}
