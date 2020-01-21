#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output, session) {
  dependency_plot <- eventReactive(c(
    input[["generate_plot"]],
    input[["plot_click"]]
  ), {
    plot_dependencies(input[["package"]],
                      type = tolower(input[["type"]]),
                      label_percentage = input[["label_percentage"]],
                      depth = input[["depth"]],
                      downloads = input[["label_percentage"]] != 1,
                      bioc = "bioc" %in% input[["options"]],
                      local = "local" %in% input[["options"]],
                      dependency_type = input[["dependency_type"]])
  })
  # Maybe use `need` or `validate` here?
  output[["depPlot"]] <- renderPlot({
    validate(
      need(input[["package"]], "Please pass package name.")
    )
    dependency_plot()
  })
  # Could we use ggsave here?
  output[["download"]] <- invisible(downloadHandler(
    filename = function() {
      paste0(input[["package"]], "_depth", input[["depth"]], ".png")
    },
    content = function(file) {
      png(file)
      print(dependency_plot())
      dev.off()
    }
  ))
  # Plot click listener
  observeEvent(input[["plot_click"]], {
    adjacency_df <- nearPoints(dependency_plot()[["data"]], input[["plot_click"]],
                               "x", "y", addDist = TRUE, allRows = TRUE)
    new_package <- adjacency_df[which.min(adjacency_df[["dist_"]]), "name"]
    updateTextInput(session, "package", value = new_package)
  })
})
