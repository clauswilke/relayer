#' Remap existing aesthetic mapping in the geom of a layer
#'
#' @param layer The layer on which to work
#' @param old_aes The old aes (which the geom uses internally)
#' @param new_aes The new aes (which we now want the geom to use)
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' df1 <- data.frame(x = c(1, 2, 3))
#'
#' ggplot(df1, aes(x = x)) +
#'   geom_tile(aes(fill = x, y = 2)) +
#'   geom_tile(aes(fill.alt = x, y = 1)) %>% remap_geom_aes("fill", "fill.alt") +
#'   scale_fill_viridis_c(option = "A", guide = "legend", name = "viridis A") +
#'   scale_colour_viridis_c(aesthetics = "fill.alt", guide = "legend", name = "viridis D")
#' @export
remap_geom_aes <- function(layer, old_aes, new_aes) {
  geom <- layer$geom

  # create new default mappings
  x <- list2(!!new_aes := geom$default_aes[[old_aes]])
  new_default_aes <- do.call(aes, plyr::defaults(do.call(aes, x), geom$default_aes))

  # create new draw key function
  draw_key_new <- function(data, params, size) {
    # remap data
    data[[old_aes]] <- data[[new_aes]] %||% data[[old_aes]]
    # call old legend drawing function
    geom$draw_key(data, params, size)
  }

  # create new geom
  new_geom <- ggproto(
    "remapped", geom,
    default_aes = new_default_aes,

    draw_key = draw_key_new,

    draw_panel = function(self, data, panel_params, coord, ...) {
      # remap data
      data[[old_aes]] <- data[[new_aes]] %||% data[[old_aes]]
      # call parent drawing function
      ggproto_parent(geom, self)$draw_panel(data, panel_params, coord, ...)
    },

    aesthetics = function(self) {
      union(ggproto_parent(geom, self)$aesthetics(), new_aes)
    }
  )

  layer$geom <- new_geom
  layer
}

