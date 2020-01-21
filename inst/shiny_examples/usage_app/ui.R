#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyUI(fluidPage(
  titlePanel("Deepdep Usage Example"),
  sidebarLayout(
    sidebarPanel(
      textInput("package",
                "Package to visualize:",
                placeholder = "package name"),
      radioButtons("type",
                   "Plot type:",
                   choices = c("Circular", "Tree"),
                   selected = "Circular"),
      sliderInput("label_percentage",
                  "Percentage of labels:",
                  min = 0,
                  max = 1,
                  value = 1),
      sliderInput("depth",
                  "Depth:",
                  min = 1,
                  max = 5,
                  value = 1,
                  step = 1),
      checkboxGroupInput("deps_types",
                         "Types of dependencies:",
                         choices = c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo"),
                         selected = c("Depends", "Imports")),
      selectInput("options",
                  "Additional options:",
                  choices = list(Bioconductor = "bioc", Local = "local"),
                  multiple = TRUE),
      actionButton("generate_plot",
                   "Generate deepdep plot"),
      downloadButton("download",
                     "Download plot")
    ),
    mainPanel(
      shinycssloaders::withSpinner(plotOutput("depPlot", click = "plot_click"))
    )
  )
))
