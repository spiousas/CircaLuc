library(shiny)
library(gt)

# Here is a Shiny app (contained within
# a single file) that (1) prepares a
# gt table, (2) sets up the `ui` with
# `gt_output()`, and (3) sets up the
# `server` with a `render_gt()` that
# uses the `gt_tbl` object as the input
# expression

gt_tbl <-
  gtcars %>%
  gt() %>%
  cols_hide(contains("_"))

ui <- fluidPage(
  
  gt_output(outputId = "table")
)

server <- function(input,
                   output,
                   session) {
  
  output$table <-
    render_gt(
      expr = gt_tbl,
      height = px(600),
      width = px(600)
    )
}

if (interactive()) {
  shinyApp(ui, server)
}
