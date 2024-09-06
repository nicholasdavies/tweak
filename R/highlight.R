#' Highlight areas on a plot
#'
#' Draws a series of highlights (translucent rectangles) onto a base R plot.
#'
#' \code{x} and \code{y} can either be: \code{NULL}, in which case the extent
#' of the plotted area is used; a numeric vector, interleaved such that the
#' elements are interpreted as e.g.
#' \code{c(x1_start, x1_end, x2_start, x2_end, ...)}; a list with two elements,
#' e.g. \code{list(c(x1_start, x2_start, ...), c(x1_end, x2_end, ...))}; or a
#' matrix with two columns (first column "starts", second column "ends").
#'
#' Arguments are recycled as needed.
#'
#' @param x x coordinates for highlight areas.
#' @param y y coordinates for highlight areas (usually leave \code{NULL}).
#' @param col fill colours for rectangles.
#' @param label text labels for each rectangle.
#' @param alpha opacity level (0 to 1) for rectangles.
#' @param shrink if \code{TRUE} or numeric and \code{y} is \code{NULL}, lowers
#'   the top of each subsequent rectangle by the specified number of user
#'   plotting units so that the labels don't overlap; \code{TRUE} uses the
#'   line height times 2, which is a decent default. Set to zero for no
#'   shrinking.
#' @param border colour for rectangle borders; \code{NA} (default) to omit.
#' @param ... other arguments to pass on to \code{rect()}.
#'
#' @export
#' @examples
#' \dontrun{
#' curve(dnorm(x), 0, 4)
#' highlight(x = c(0, 0, 1, 3, 2, 4), label = letters[1:3], col = c("red", "blue", "purple"))
#' }
highlight = function(x = NULL, y = NULL,
    col = "#ffff00", label = NA,
    alpha = 0.2, shrink = TRUE, border = NA, ...)
{
    # Handle list input
    if (is.list(x) && length(x) == 2)
        x = c(rbind(x[[1]], x[[2]]))
    if (is.list(y) && length(y) == 2)
        y = c(rbind(y[[1]], y[[2]]))

    # Handle matrix input
    if (is.matrix(x) && ncol(x) == 2)
        x = c(t(x))
    if (is.matrix(y) && ncol(y) == 2)
        y = c(t(y))

    # Get extent of current plot
    extent = par("usr")
    if (par("xlog"))
        extent[1:2] = 10 ^ extent[1:2]
    if (par("ylog"))
        extent[3:4] = 10 ^ extent[3:4]

    # Interpret shrink
    if (identical(shrink, TRUE) && is.null(y)) {
        shrink = strheight(" ") * 2
    } else if (is.numeric(shrink) && is.null(y)) {
        shrink = shrink[1]
    } else {
        shrink = 0
    }

    # Supply missing x and y
    if (!length(x))
        x = extent[1:2]
    if (!length(y))
        y = extent[3:4]

    # Check x and y arguments
    if (!is.numeric(x) || length(dim(x)) > 1)
        stop("Invalid format for x.")
    if (!is.numeric(y) || length(dim(y)) > 1)
        stop("Invalid format for y.")

    if (length(x) %% 2 == 1)
        stop("Length of x must be even.")
    if (length(y) %% 2 == 1)
        stop("Length of y must be even.")

    # Separate x and y components
    xl = x[seq(1, length(x), 2)]
    xr = x[seq(2, length(x), 2)]
    yb = y[seq(1, length(y), 2)]
    yt = y[seq(2, length(y), 2)]

    # Pad length of arguments
    ml = max(length(xl), length(yb), length(col),
        length(label), length(alpha), length(border))
    xl = rep_len(xl, ml)
    xr = rep_len(xr, ml)
    yb = rep_len(yb, ml)
    yt = rep_len(yt, ml)
    col = rep_len(col, ml)
    label = rep_len(label, ml)
    alpha = rep_len(alpha, ml)
    border = rep_len(border, ml)

    # Expand skinny rectangles (<1 px) and calculate shrink
    plot_size = dev.size("px") * par("pin") / par("din") # plot size in pixels
    xpixel = (extent[2] - extent[1]) / plot_size[1]
    ypixel = (extent[4] - extent[3]) / plot_size[2]
    xskinny = abs(xl - xr) < xpixel
    yskinny = abs(yb - yt) < ypixel
    xl[xskinny] = xl[xskinny] - xpixel / 2
    xr[xskinny] = xr[xskinny] + xpixel / 2
    yb[xskinny] = yb[xskinny] - ypixel / 2
    yt[xskinny] = yt[xskinny] + ypixel / 2
    shrink_y = shrink * 0:(length(yt) - 1)

    # Add alpha to colours, but don't override if specified already
    for (i in 1:ml) {
        if (is.character(col[i]) &&
                (substr(col[i], 1, 1) != "#" || nchar(col[i]) %in% c(4, 7))) {
            tmp = col2rgb(col[i])
            col[i] = rgb(tmp[1, 1] / 255, tmp[2, 1] / 255, tmp[3, 1] / 255, alpha[i])
        }
    }

    # Plot highlights
    rect(
        xleft = xl,
        xright = xr,
        ybottom = yb,
        ytop = yt - shrink_y,
        col = col,
        border = border,
        ...)

    # Labels
    if (any(!is.na(label)))
    {
        j = !is.na(label)
        offset_x = 5 * xpixel
        offset_y = 5 * ypixel
        text(xl[j] + offset_x, yt[j] - offset_y - shrink_y, label[j], adj = c(0, 1))
    }
}

#' Highlight areas on a plot
#'
#' Draws a series of highlights (translucent rectangles) onto a ggplot2 plot.
#'
#' \code{geom_highlight} understands the following aesthetics:
#' \itemize{
#'   \item \code{xmin} (required): "left" x values of highlight rectangles.
#'   \item \code{xmax} (required): "right" x values of highlight rectangles.
#'   \item \code{label}: text labels for each highlight rectangle.
#'   \item \code{fill}, \code{alpha}: of highlight rectangles (default alpha
#'       is 0.2).
#'   \item \code{size}, \code{family}, \code{fontface}, \code{lineheight}: for
#'       label text.
#'   \item \code{colour}, \code{linewidth}, \code{linetype}: of highlight
#'       rectangle outlines (default hidden)
#' }
#'
#' @inheritParams ggplot2::geom_line
#' @param shrink lower the top of each subsequent rectangle by the specified
#'   number of natural plot units (where 1.0 is the height of the plot area)
#'   so that the labels don't overlap. Set to zero for no shrinking.
#' @param offset two-element numeric vector giving x and y offset from top-left
#'   corner of rectangles for anchoring label text; in natural plot units
#'   (where 1.0 is the width/height of the plot area).
#'
#' @export
#' @examples
#' \dontrun{
#' df = data.frame(x1 = 0:10, x2 = 0:10 + 0.8, label = letters[1:11])
#' ggplot(df) +
#'     geom_highlight(aes(xmin = x1, xmax = x2, label = label, fill = label)) +
#'     geom_line(aes(x1, x1))
#' }
geom_highlight = function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    ...,
    shrink = 0.04,
    offset = c(0.01, 0.01),
    lineend = "butt",
    linejoin = "mitre",
    size.unit = "mm",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE)
{
    # Create GeomHighlight if needed. This is not created at global namespace
    # to avoid requiring the ggplot2 package if this function is never used.
    if (is.null(cache_env[["GeomHighlight"]])) {
        cache_env[["GeomHighlight"]] <- ggplot2::ggproto(
            "GeomHighlight",
            ggplot2::Geom,
            required_aes = c("xmin", "xmax"),
            default_aes = ggplot2::aes(colour = NA, fill = "grey35", alpha = 0.2,
                label = NA, size = 3.88, family = "", fontface = 1, lineheight = 1.2,
                linewidth = 0.5, linetype = 1),
            draw_key = ggplot2::draw_key_rect,
            draw_panel = function(data, panel_scales, coord,
                shrink = 0.04, offset = c(0.01, 0.01), size.unit = "mm",
                lineend = "butt", linejoin = "mitre") {
                # Transform the data
                coords = coord$transform(data, panel_scales)

                # Expand skinny rectangles and calculate shrink
                girth = abs(coords$xmax - coords$xmin)
                skinny = girth < 0.0025
                coords$xmin[skinny] = coords$xmin[skinny] - (0.0025 - girth[skinny]) / 2
                coords$xmax[skinny] = coords$xmax[skinny] + (0.0025 - girth[skinny]) / 2
                shrink_y = shrink * 0:(length(coords$xmin) - 1)

                # Fix up other parameters
                offset = rep_len(offset, 2)
                size.unit = ggplot2:::resolve_text_unit(size.unit)

                # Highlight rectangle
                rect = grid::rectGrob(
                        x = coords$xmin,
                        y = 0,
                        width = coords$xmax - coords$xmin,
                        height = 1 - shrink_y,
                        default.units = "native",
                        just = c("left", "bottom"),
                        gp = grid::gpar(
                            col = coords$colour,
                            fill = ggplot2::fill_alpha(coords$fill, coords$alpha),
                            lwd = coords$linewidth * ggplot2:::.pt,
                            lty = coords$linetype,
                            linejoin = linejoin,
                            lineend = lineend
                        )
                    )

                # Text label for top
                label = grid::textGrob(
                    label = coords$label,
                    x = coords$xmin + offset[1],
                    y = 1 - shrink_y - offset[2],
                    just = c("left", "top"),
                    gp = grid::gpar(
                        fontsize = coords$size * size.unit,
                        fontfamily = coords$family,
                        fontface = coords$fontface,
                        lineheight = coords$lineheight
                    )
                )

                # Assemble grob
                grobs = list(rect, label)
                class(grobs) = "gList"

                ggplot2:::ggname("geom_highlight",
                    grid::grobTree(children = grobs)
                )
            }
        )
    }

    ggplot2::layer(
        geom = cache_env[["GeomHighlight"]],
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            shrink = shrink,
            offset = offset,
            lineend = lineend,
            linejoin = linejoin,
            size.unit = size.unit,
            na.rm = na.rm,
            ...
        )
    )
}

# To hold GeomHighlight
cache_env = new.env()
