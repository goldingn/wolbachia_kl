#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data
#' @param path
#' @param width
#' @param height
#' @return
#' @author njtierney
#' @export
base_plot_write_path <- function(
  data,
  path,
  width,
  height
    ) {

  png(
    filename = path,
    width = width,
    height = height,
    units = "in",
    bg = "white",
    res = 300
    )

  plot(data)

  dev.off()

  path

}
