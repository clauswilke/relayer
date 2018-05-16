#' Remap existing aesthetic mapping in the geom of a layer
#'
#' @param layer The layer on which to work
#' @param new_aes Named character vector listing how the plot aesthetics relate to the geom aesthetics
#' @param mapping Optional aesthetic mapping from plot aesthetics to geom aesthetics
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # make a layer with its own fill scale
#' df1 <- data.frame(x = c(1, 2, 3))
#'
#' ggplot(df1, aes(x = x)) +
#'   geom_tile(aes(fill = x, y = 2)) +
#'   geom_tile(aes(fill2 = x, y = 1)) %>% rename_geom_aes(new_aes = c("fill" = "fill2")) +
#'   scale_colour_viridis_c(aesthetics = "fill", guide = "legend", name = "viridis A", option = "A") +
#'   scale_colour_viridis_c(aesthetics = "fill2", guide = "legend", name = "viridis D")
#'
#' # second example of layers with their own scales
#' scale_distiller <- function(aesthetics, palette, name, ...) {
#'   scale_colour_distiller(
#'     aesthetics = aesthetics,
#'     palette = palette,
#'     name = name, ...,
#'     guide = guide_legend(order = palette))
#' }
#'
#' ggplot(mapping = aes(Sepal.Length, Sepal.Width)) +
#'   (geom_point(data = filter(iris, Species == "setosa"), aes(colour1 = Sepal.Width)) %>%
#'      rename_geom_aes(new_aes = c("colour" = "colour1"))) +
#'   (geom_point(data = filter(iris, Species == "versicolor"), aes(colour2 = Sepal.Width)) %>%
#'      rename_geom_aes(new_aes = c("colour" = "colour2"))) +
#'   (geom_point(data = filter(iris, Species == "virginica"), aes(colour3 = Sepal.Width)) %>%
#'      rename_geom_aes(new_aes = c("colour" = "colour3"))) +
#'   facet_wrap(~Species) +
#'   scale_distiller("colour1", 5, "setosa") +
#'   scale_distiller("colour2", 6, "versicolor") +
#'   scale_distiller("colour3", 7, "virginica") +
#'   theme_bw() +
#'   theme(legend.position = "bottom", legend.key.width = grid::unit(6, "pt"))
#'
#' # multiple scales within a single layer
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point(aes(col1 = Sepal.Width, col2 = Sepal.Width, col3 = Sepal.Width, group = Species)) %>%
#'     rename_geom_aes(
#'       new_aes = c("colour" = "col1", "colour" = "col2", "colour" = "col3"),
#'       aes(colour = case_when(group == 1 ~ col1, group == 2 ~ col2, TRUE ~ col3))
#'     ) +
#'     facet_wrap(~Species) +
#'     scale_distiller("col1", 5, "setosa") +
#'     scale_distiller("col2", 6, "versicolor") +
#'     scale_distiller("col3", 7, "virginica") +
#'     theme_bw() + theme(legend.position = "bottom", legend.key.width = grid::unit(6, "pt"))
#'
#' # swap aesthetics in a geom
#' ggplot(iris, aes(Sepal.Length, fill = Species)) +
#'   geom_density(colour = "gray70", size = 3, alpha = 0.7) %>%
#'   rename_geom_aes(new_aes = c("fill" = "colour", "colour" = "fill"))
#'
#' @export
rename_geom_aes <- function(layer, new_aes, mapping = NULL) {
  # make mapping if needed
  mapping <- mapping %||% make_aes_mapping(new_aes)

  # get the geom
  geom <- layer$geom

  # create new default aes, needed to properly register the aes with the geom
  # we set the defaults to NULL
  default_aes_new <- do.call(aes, plyr::defaults(make_aes_null_mapping(new_aes), geom$default_aes))

  # create new draw key function
  draw_key_new <- function(data, params, size) {
    # remap data
    #data <- remap_data(data, mapping)

    # rename data
    data <- rename_data(data, new_aes)
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
      data <- remap_data(data, mapping)
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
remap_data <- function(data, mapping) {
  # evaluate mapping
  evaled <- lapply(mapping, rlang::eval_tidy, data = data)
  evaled <- lapply(evaled, unname)

  # now merge, potentially overwriting previous columns
  for (col in names(evaled)) {
    data[[col]] <- evaled[[col]]
  }
  data
}


#' @noRd
rename_data <- function(data, aes_names) {
  old_aes <- names(aes_names)
  if (is.null(old_aes)) return(data) # no old names provided, nothing to be done

  new_aes <- unname(aes_names)
  new_names <- names(data)

  for (i in seq_along(new_aes)) {
    new_names[names(data) == new_aes[i]] <- old_aes[i]
  }
  names(data) <- new_names

  data
}

#' @noRd
make_aes_mapping <- function(aes_vect) {
  old_aes <- names(aes_vect)
  if (is.null(old_aes)) return(NULL)

  x <- NULL
  for (i in seq_along(old_aes)) {
    x <- c(x, rlang::list2(!!old_aes[i] := sym(aes_vect[i])))
  }

  do.call(aes, x)
}

#' @noRd
make_aes_null_mapping <- function(aes_vect) {
  x <- NULL
  for (name in aes_vect) {
    x <- c(x, rlang::list2(!!name := NULL))
  }
  if (is.null(x)) {
    NULL
  } else {
    do.call(aes, x)
  }
}

