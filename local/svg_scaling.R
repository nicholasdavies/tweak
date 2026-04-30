library(shiny)
library(ggplot2)
library(svglite)  # for svg graphics device

ui <- fluidPage(
  plotOutput("plot", width = "100%", height = "auto")
)

server <- function(input, output, session) {
  output$plot <- renderImage({
    # Get browser width
    w_px <- session$clientData$output_plot_width
    if (is.null(w_px)) w_px <- 640

    # Reference physical size in inches
    ref_width_in  <- 640 / 72
    ref_height_in <- 480 / 72

    # Scale factor from current width
    ref_px <- 640
    k <- w_px / ref_px

    # Output file
    tmpfile <- tempfile(fileext = ".svg")

    # Open SVG device with scaled *physical* size
    svglite::svglite(
      file   = tmpfile,
      width  = ref_width_in,
      height = ref_height_in
    )

    # Base plot written once at reference size
    p <- ggplot(mpg, aes(displ, hwy)) +
      geom_point() +
      theme_minimal(base_size = 12) +
      labs(
        title = "Uniform scaling with SVG in Shiny",
        subtitle = sprintf("Scale factor: %.2fx", k)
      )
    print(p)

    dev.off()

    list(
      src = tmpfile,
      contentType = "image/svg+xml",
      width  = w_px,
      height = w_px * (480 / 640)
    )
  }, deleteFile = TRUE)
}

shinyApp(ui, server)
