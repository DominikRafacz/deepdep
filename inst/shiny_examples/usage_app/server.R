#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output) {
  output[["depPlot"]] <- renderPlot({
    if (isTruthy(input[["package"]])) {
      plot_dependencies(input[["package"]],
                        type = tolower(input[["type"]]),
                        label_percentage = input[["label_percentage"]],
                        depth = input[["depth"]],
                        downloads = input[["label_percentage"]] != 1,
                        bioc = "bioc" %in% input[["options"]],
                        local = "local" %in% input[["options"]],
                        deps_types = input[["deps_types"]])
    }
  })
})
