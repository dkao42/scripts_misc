library(rlang)

ggfp_dk <- function(df,
                    name = name,
                    estimate = estimate,
                    se = se,
                    pvalue = NULL,
                    colour = NULL,
                    shape = NULL,
                    null_clr = "black",
                    null_num = 1,
                    ftn=3,
                    lth=1.25,
                    logodds = FALSE,
                    psignif = 0.05,
                    ci = 0.95,
                    ...) {
  
  # Input checks
  stopifnot(is.data.frame(df))
  stopifnot(is.logical(logodds))
  
  # TODO: Add some warnings when name, estimate etc are missing from df and user
  # is not defining the name, estimate etc explicitly.
  
  # Quote input
  name <- enquo(name)
  estimate <- enquo(estimate)
  se <- enquo(se)
  pvalue <- enquo(pvalue)
  colour <- enquo(colour)
  shape <- enquo(shape)
  
  args <- list(...)
  
  # TODO: Allow color instead of colour. This will do it, but also breaks other
  # options at the end, so fix those before uncommenting this.
  # args <- enquos(...)
  # if (quo_is_null(colour) && "color" %in% names(args)) {
  #   colour <- args$color
  # }
  
  # Estimate multiplication const for CI
  const <- stats::qnorm(1 - (1 - ci) / 2)
  
  # Adjust data frame variables
  df <-
    df %>%
    # Convert to a factor to preserve order.
    dplyr::mutate(
      !!name := factor(
        !!name,
        levels = !!name %>% unique() %>% rev(),
        ordered = TRUE
      ),
      # Added here to estimate xbreaks for log odds later
      .xmin = !!estimate - const * !!se,
      .xmax = !!estimate + const * !!se,
      # Add a logical variable with the info on whether points will be filled.
      # Defaults to TRUE.
      .filled = TRUE,
      # Add a variable with the estimates to be printed on the right side of y-axis
      .label = sprintf("%.2f", !!estimate)
    )
  
  # Exponentiate the estimates and CIs if logodds
  if (logodds) {
    df <-
      df %>%
      mutate(
        .xmin = exp(.data$.xmin),
        .xmax = exp(.data$.xmax),
        !!estimate := exp(!!estimate)
      )
  }
  
  # If pvalue provided, adjust .filled variable
  if (!quo_is_null(pvalue)) {
    df <-
      df %>%
      dplyr::mutate(.filled = !!pvalue < !!psignif)
  }
  
  # Plot
  g <-
    ggplot2::ggplot(
      df,
      aes(
        x = !!estimate,
        y = !!name
      )
    )
  
  # If logodds, adjust axis scale
  if (logodds) {
    if ("xtickbreaks" %in% names(args)) {
      g <-
        g +
        scale_x_continuous(
          trans = "log2",
          breaks = args$xtickbreaks
        )
    } else {
      g <-
        g +
        scale_x_continuous(
          trans = "log2",
          breaks = scales::log_breaks(n = 7)
        )
    }
  }
  
  g <-
    g +
    # Add custom theme
    theme_forest() +
    # Add Nightingale colour palette
    scale_colour_ng_d() +
    scale_fill_ng_d() +
    # Add striped background
    geom_stripes() +
    # Add vertical line at null point
    geom_vline(
      xintercept = null_num,
      linetype = "solid",
      size = 0.4,
      colour = null_clr
    )
  
  g <-
    g +
    # And point+errorbars
    geom_effect(
      ggplot2::aes(
        xmin = .data$.xmin,
        xmax = .data$.xmax,
        colour = !!colour,
        shape = !!shape,
        filled = .data$.filled
      ),
      position = ggstance::position_dodgev(height = 1),
      fatten=ftn,lwd=lth
    ) +
    # Define the shapes to be used manually
    ggplot2::scale_shape_manual(values = c(21L, 22L, 23L, 24L, 25L)) +
    guides(
      colour = guide_legend(reverse = TRUE),
      shape = guide_legend(reverse = TRUE)
    )
  
  # Limits adjustment
  #
  # # Extend the shorter x-axis side to mirror the longer one
  # xext <-
  #   c(
  #     df[[quo_name(xmin)]],
  #     df[[quo_name(xmax)]]
  #   ) %>%
  #   abs() %>%
  #   max()
  # g <-
  #   g +
  #   ggplot2::expand_limits(x = c(-xext, xext))
  
  # If no groups specified (through either colour or shape), show estimate values.
  # ### Note: I had to switch back to row number as the y aesthetic in order to use
  # ### continuous scale and to be able to add a secondary axis for labels on the
  # ### right. Any other solution was too time consuming.
  # ### I also had to simplify the fi statement below cause when colour and/or
  # ### shape is specified a legend is added even if they are of length 1L and
  # ### messes up right side visuals.
  # if (
  #   (quo_is_null(colour) || length(unique(df[[quo_name(colour)]])) == 1L) &&
  #   (quo_is_null(shape) || length(unique(df[[quo_name(shape)]])) == 1L)
  # ) {
  # if ( quo_is_null(colour) && quo_is_null(shape)){
  #   g <-
  #     g +
  #     geom_text(
  #       aes(label = .label),
  #       x = 1.1 * xext,
  #       hjust = 1
  #     ) +
  #     expand_limits(x = 1.1 * xext)
  # } else {
  #   g <-
  #     g +
  #     ggplot2::scale_y_continuous(
  #       breaks = df %>% pull(.name_order),
  #       labels = df %>% pull(!!name)
  #     )
  # }
  
  # Pass through graphical parameters and define defaults values for some.
  if ("title" %in% names(args)) {
    g <- g + labs(title = args$title)
  }
  if ("subtitle" %in% names(args)) {
    g <- g + labs(subtitle = args$subtitle)
  }
  if ("caption" %in% names(args)) {
    g <- g + labs(caption = args$caption)
  }
  if ("xlab" %in% names(args)) {
    g <- g + labs(x = args$xlab)
  }
  if (!"ylab" %in% names(args)) {
    args$ylab <- ""
  }
  g <- g + labs(y = args$ylab)
  if ("xlim" %in% names(args)) {
    g <- g + coord_cartesian(xlim = args$xlim)
  }
  if ("ylim" %in% names(args)) {
    g <- g + ylim(args$ylim)
  }
  if ("xtickbreaks" %in% names(args) & !logodds) {
    g <- g + scale_x_continuous(breaks = args$xtickbreaks)
  }
  g
}


