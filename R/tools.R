#' Transform vote data into rollcall objects
#'
#' @param votos An object returned from \code{\link{pega_votos}}
#' @param obstrucao_nao If \code{TRUE}, codes obstructions as nay votes
#' @param ideal If \code{TRUE}, returns and object suited to analysis using Bayesian IRT
#'
#' @note The function uses the deputy-party-uf unity
#'
#' @export

votos_para_rollcall <- function(votos, obstrucao_nao = TRUE, ideal = TRUE){

  # Codifica variaveis
  if(obstrucao_nao){

    votos <- votos %>%
      dplyr::mutate("voto" = dplyr::case_when(

        .data$voto == "Sim" ~ 1L,
        .data$voto == "Não" ~ 0L,
        .data$voto == "Obstrução" ~ 0L
      ))

  } else {

    votos <- votos %>%
      dplyr::mutate("voto" = dplyr::case_when(

        .data$voto == "Sim" ~ 1L,
        .data$voto == "Não" ~ 0L
      ))
  }

  # Cria um ID partidario
  votos <- votos %>%
    dplyr::mutate("deputado_id" = paste0(.data$deputado_id, "-", .data$deputado_sigla_partido, "-", .data$deputado_sigla_uf))

  # Cria o objeto rollcall
  rc <- congressbr::vote_to_rollcall(
    votos$voto,
    votos$deputado_id,
    votos$id_votacao,
    ideal = ideal)

  # Retorna
  return(rc)
}


#' Merge OC or w-nominate estimates with biographic information on deputies
#'
#' @param rc A OC or W-nominated object with estimates
#'
#' @export

completa_scores <- function(rc){

  deps <- pega_parlamentares()

  rc$legislators %>%
    dplyr::as_tibble(rownames = "id") %>%
    tidyr::separate(.data$id, c("id", "partido", "uf"), sep = "-") %>%
    dplyr::select(.data$id, .data$partido, .data$uf, .data$coord1D, .data$coord2D) %>%
    janitor::clean_names() %>%
    dplyr::left_join(deps, by = "id")
}
