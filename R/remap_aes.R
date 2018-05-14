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
remap_geom_aes_string <- function(layer, old_aes, new_aes) {
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

#' Remap existing aesthetic mapping in the geom of a layer
#'
#' @param layer The layer on which to work
#' @param mapping Aesthetic mapping from plot aesthetics to geom aesthetics
#' @param new_aes Character vector of the names of any new aesthetics this geom can now handle,
#'   needs to be provided for proper legend drawing
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' df1 <- data.frame(x = c(1, 2, 3))
#'
#' ggplot(df1, aes(x = x)) +
#'   geom_tile(aes(fill = x, y = 2)) +
#'   geom_tile(aes(fill2 = x, y = 1)) %>% remap_geom_aes(aes(fill = fill2), new_aes = "fill2") +
#'   scale_colour_viridis_c(aesthetics = "fill", guide = "legend", name = "viridis A", option = "A") +
#'   scale_colour_viridis_c(aesthetics = "fill2", guide = "legend", name = "viridis D")
#'
#'
#' dist_scale <- function(aesthetics, palette, name) {
#'   scale_colour_distiller(
#'     aesthetics = aesthetics,
#'     palette = palette,
#'     name = name,
#'     guide = guide_legend(order = palette))
#' }
#'
#' ggplot(mapping = aes(Sepal.Length, Sepal.Width)) +
#'   (geom_point(data = filter(iris, Species == "setosa"), aes(colour1 = Sepal.Width)) %>%
#'      remap_geom_aes(aes(colour = colour1), new_aes = "colour1")) +
#'   (geom_point(data = filter(iris, Species == "versicolor"), aes(colour2 = Sepal.Width)) %>%
#'      remap_geom_aes(aes(colour = colour2), new_aes = "colour2")) +
#'   (geom_point(data = filter(iris, Species == "virginica"), aes(colour3 = Sepal.Width)) %>%
#'      remap_geom_aes(aes(colour = colour3), new_aes = "colour3")) +
#'   facet_wrap(~Species) +
#'   dist_scale("colour1", 5, "setosa") +
#'   dist_scale("colour2", 6, "versicolor") +
#'   dist_scale("colour3", 7, "virginica") +
#'   theme_bw() +
#'   theme(legend.position = "bottom", legend.key.width = grid::unit(6, "pt"))
#'
#' @export
remap_geom_aes <- function(layer, mapping, new_aes = NULL) {
  geom <- layer$geom

  # create new default aes, needed to properly register the aes with the geom
  # we set the defaults to zero; this requires some juggling
  x <- NULL
  for (nae in new_aes) {
    x <- c(x, rlang::list2(!!nae := NULL))
  }
  if (is.null(x)) {
    default_aes_new <- geom$default_aes # no new aes provided
  }
  else {
    default_aes_new <- do.call(aes, plyr::defaults(do.call(aes, x), geom$default_aes))
  }

  # create new draw key function
  draw_key_new <- function(data, params, size) {
    # remap data
    data <- remap_data(mapping, data)
    # call old legend drawing function
    geom$draw_key(data, params, size)
  }

  # create new geom
  new_geom <- ggproto(
    "remapped", geom,

    default_aes = default_aes_new,

    draw_key = draw_key_new,

    draw_panel = function(self, data, panel_params, coord, ...) {
      # remap data
      data <- remap_data(mapping, data)
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


#' @examples
#' library(ggplot2)
#' library(rlang)
#'
#' df <- data.frame(x = c(1, 2, 3),
#'                  y = c(3, 2, 1),
#'                  z = c(5, 6, 7))
#'
#' remap_data(aes(x = y - x, y = 2*x + y), df)
#' @noRd
remap_data <- function(mapping, data) {
  # evaluate mapping
  evaled <- lapply(mapping, rlang::eval_tidy, data = data)
  evaled <- lapply(evaled, unname)

  # now merge, potentially overwriting previous columns
  for (col in names(evaled)) {
    data[[col]] <- evaled[[col]]
  }
  data
}

