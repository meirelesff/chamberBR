#' Retrieve legislators' speeches
#'
#' @param id_deputado A deputy's ID
#' @param legis A chamber identification
#'
#' @export

pega_discursos <- function(id_deputado, legis){


  # Primeira requisicao
  cat("Pegando discursos de:", id_deputado, "\n")
  pag <- "https://dadosabertos.camara.leg.br/api/v2/deputados/%s/discursos?idLegislatura=%s&itens=100" %>%
    sprintf(id_deputado, legis) %>%
    httr::GET()

  # Aborta se nao der certo
  if(httr::status_code(pag) != 200) return(NULL)

  # Parseia a primeira pagina
  res <- httr::content(pag) %>%
    purrr::pluck("dados") %>%
    purrr::map(unlist) %>%
    purrr::map(data.frame) %>%
    purrr::map(t) %>%
    purrr::map(as.data.frame) %>%
    purrr::map(dplyr::as_tibble) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(id_deputado = id_deputado)

  # Checa o numero de paginas
  pag <- httr::content(pag)

  # Apenas uma pagina?
  if(length(pag$links) < 4) return(res)

  # Pega as demais paginas
  n_pags <- stringr::str_match(pag$links[[4]]$href, "pagina=(.*?)&itens")[, 2]
  if(n_pags == 1) return(res)
  paginas <- vector("list", n_pags)
  paginas[[1]] <- res

  for(i in 2:n_pags){

    # Requisicao
    pag <- pag$links[[2]]$href %>%
      httr::GET()

    # Aborta se nao der certo
    if(httr::status_code(pag) != 200) return(NULL)

    paginas[[i]] <- httr::content(pag) %>%
      purrr::pluck("dados") %>%
      purrr::map(unlist) %>%
      purrr::map(data.frame) %>%
      purrr::map(t) %>%
      purrr::map(as.data.frame) %>%
      purrr::map(dplyr::as_tibble) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(id_deputado = id_deputado)

    pag <- httr::content(pag)
  }

  # Retorna
  dplyr::bind_rows(paginas)
}
