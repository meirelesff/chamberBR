#' Get a list of all rollcalls in a given year
#'
#' @param ano Year
#' @param plenario If \code{TRUE}, returns only rollcalls in the plenary
#'
#' @export

pega_votacoes <- function(ano, plenario = TRUE){

# Pega as votacoes
votacoes <- suppressMessages(

  "https://dadosabertos.camara.leg.br/arquivos/votacoes/csv/votacoes-%s.csv" %>%
    sprintf(ano) %>%
    readr::read_csv2() %>%
    dplyr::select(.data$id, .data$data, .data$siglaOrgao, .data$idEvento,
                  .data$aprovacao, .data$votosSim, .data$votosNao,
                  .data$votosOutros, .data$descricao,
                  .data$ultimaApresentacaoProposicao_descricao) %>%
    janitor::clean_names()
)

# Filtra as que foram pra plenario, caso habilitado
if(plenario) votacoes <- dplyr::filter(votacoes, .data$sigla_orgao == "PLEN")

# Retorna
return(votacoes)
}


#' Get parties' whip positions in rollcall votes
#'
#' @param ano Year.
#'
#' @export

pega_orientacao <- function(ano){

  # Pega e retorna as votacoes
  suppressMessages(

    "https://dadosabertos.camara.leg.br/arquivos/votacoesOrientacoes/csv/votacoesOrientacoes-%s.csv" %>%
      sprintf(ano) %>%
      readr::read_csv2() %>%
      dplyr::select(.data$idVotacao, .data$siglaBancada, .data$orientacao) %>%
      janitor::clean_names() %>%
      dplyr::rename("orientacao_bancada" = .data$orientacao)
  )
}


#' Get deputies' rollcall votes in a given year
#'
#' @param ano Year.
#' @param plenario If \code{TRUE}, returns only rollcalls in the plenary
#' @param orientacao IF \code{TRUE}, adds parties' whip orientations
#'
#' @export

pega_votos <- function(ano, plenario = TRUE, orientacao = TRUE){

  # Pega os votos
  votos <- suppressMessages(

    "https://dadosabertos.camara.leg.br/arquivos/votacoesVotos/csv/votacoesVotos-%s.csv" %>%
      sprintf(ano) %>%
      readr::read_csv2() %>%
      dplyr::select(.data$idVotacao, .data$voto, .data$deputado_id, .data$deputado_nome,
                    .data$deputado_siglaPartido, .data$deputado_siglaUf) %>%
      janitor::clean_names()
  )

  # Filtra votos em plenario, se habilitado
  if(plenario){

    votacoes <- pega_votacoes(ano)$id
    votos <- dplyr::filter(votos, .data$id_votacao %in% votacoes)
  }

  # Adiciona orientacao de bancada, se habilitado
  if(orientacao){

    orientacoes <- pega_orientacao(ano)
    votos <- dplyr::left_join(votos, orientacoes,
                              by = c("id_votacao" = "id_votacao",
                                     "deputado_sigla_partido" = "sigla_bancada"))
  }

  # Retorna
  return(votos)
}

