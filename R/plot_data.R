#' Plot Deming data
#'
#' @export
#' @examples
#' df <- sim_data(
#'   Jx = c(5, 5, 5, 5, 5, 7, 4, 4),
#'   Jy = c(6, 6, 6, 6, 6, 6, 6, 6),
#'   sdx = sqrt(((1/50)/2)^.5), lambda = 2
#' )
#'
#' plot_data(df)
plot_data <- function(df, formula = y ~ x, rug = TRUE, elipses = TRUE, a = NULL, b = NULL, fixed = TRUE) {

  rlang::check_installed(
    c("ggplot2", "ggdensity", "tidyselect", "tidyr"),
    "in order to plot the data"
  )

  if (! "name" %in% names(df)) df$name <- paste0("V", 1:nrow(df))

  x_name <- deparse(formula[[3]])
  y_name <- deparse(formula[[2]])

  df <- dplyr::rename(df,
    x = tidyselect::all_of(x_name),
    y = tidyselect::all_of(y_name)
  )

  df <- dplyr::mutate(df,
    mean_x = vapply(x, mean, numeric(1)),
    mean_y = vapply(y, mean, numeric(1)),
    sd_x = vapply(x, sd, numeric(1)),
    sd_y = vapply(y, sd, numeric(1))
  )

  if (! (is.null(a) || is.null(b))) {
    ab_line <- ggplot2::geom_abline(intercept = a, slope = b, color = "red")
  } else {
    ab_line <- NULL
  }

  if (elipses) {

    f_joint <- function(x, y, mean_x, mean_y, sd_x, sd_y) {
      dnorm(x, mean_x, sd_x) * dnorm(y, mean_y, sd_y)
    }

    elipse_layer <- function(var) {

      ests <- df[df$name == var, c("mean_x", "mean_y", "sd_x", "sd_y")]

      ggdensity::geom_hdr_fun(
        ggplot2::aes(fill = var), alpha = .5, probs = .8, fun = f_joint,
        args = ests,
        xlim = qnorm(c(.001, .999), ests$mean_x, ests$sd_x),
        ylim = qnorm(c(.001, .999), ests$mean_y, ests$sd_y),
        show.legend = FALSE, inherit.aes = FALSE
      )

    }


    elipse_layers <- lapply(df$name, elipse_layer)

  } else {
    elipse_layers <- NULL
  }

  if (rug) {

    df_unnested_x <- tidyr::unnest(df, cols = x)
    df_unnested_y <- tidyr::unnest(df, cols = y)

    rug_layers <- list(
      ggplot2::geom_rug(data = df_unnested_x, ggplot2::aes(x = x, color = name), show.legend = FALSE),
      ggplot2::geom_rug(data = df_unnested_y, ggplot2::aes(y = y, color = name), show.legend = FALSE)
    )

  } else {
    rug_layers <- NULL
  }

  ggplot2::ggplot(df, ggplot2::aes(x = mean_x, y = mean_y, color = name)) +
    # Layers created above
    ab_line +
    elipse_layers +
    rug_layers +
    # Crosshairs
    ggplot2::geom_linerange(
      ggplot2::aes(color = name, x = mean_x, ymin = mean_y - sd_y, ymax = mean_y + sd_y),
      size = 1, inherit.aes = FALSE, key_glyph = draw_key_crosshair
    ) +
    ggplot2::geom_linerange(
      ggplot2::aes(color = name, y = mean_y, xmin = mean_x - sd_x, xmax = mean_x + sd_x),
      size = 1, inherit.aes = FALSE, show.legend = FALSE
    ) +
    # Formatting
    ggplot2::scale_color_brewer(type = "qual", palette = 2) +
    ggplot2::scale_fill_brewer(type = "qual", palette = 2) +
    (if (fixed) ggplot2::coord_fixed() else NULL) +
    ggplot2::labs(
      x = x_name,
      y = y_name,
      color = NULL
    )
}

# Custom draw_key function, creates key of nice crosshairs
draw_key_crosshair <- function(data, params, size) {
  data$shape <- 3
  data$size <- 2
  data$stroke <- 1.5

  ggplot2::draw_key_point(data, params, size)
}





