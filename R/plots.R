#' OLB's ggplot2
#'
#' @param posicao Legend position
#' @param base_size Text size
#' @param base_family Text font (uses showtext)

tema_olb <- function(posicao = "none", base_size = 14, base_family = "worksans"){

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line.x = ggplot2::element_line(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = base_size - 2, vjust = 1),
                   axis.text.y = ggplot2::element_text(size = base_size - 2, hjust = 1),
                   legend.position = posicao,
                   plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0, vjust = 1,
                                                      margin = ggplot2::margin(0, 0, 5, 0)),
                   plot.subtitle = ggplot2::element_text(vjust = 0, hjust = 0),
                   axis.text = ggplot2::element_text(color = "black"))
}


#' OLB's ggplot2 without Y axis
#'
#' @param posicao Legend position
#' @param base_size Text size
#' @param base_family Text font (uses showtext)

tema_olb_y <- function(posicao = "none", base_size = 14, base_family = "worksans"){

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line.x = ggplot2::element_line(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   legend.position = posicao,
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = base_size - 2),
                   plot.title = ggplot2::element_text(size = base_size + 2, face = "bold", hjust = 0, vjust = 1,
                                                      margin = ggplot2::margin(0, 0, 5, 0)),
                   plot.subtitle = ggplot2::element_text(vjust = 0, hjust = 0),
                   axis.text = ggplot2::element_text(color = "black"))
}
