library(shiny)
library(markdown)
library(ggplot2)
library(ggthemes)
repartition_sexe <- read.csv("./data/repartition_sexe_clean.csv")



ui <- navbarPage(
  "Tableau de bord Ressourse humaine: La Poste Tunisienne",
  tabPanel("Nombre d'agent", fluidPage(

    # App title ----
    titlePanel("Nombre d'agent"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        # Input: Slider for the number of bins ----
        sliderInput(
          inputId = "year",
          label = "Choisir l'année:",
          min = 2017L,
          max = 2021L,
          value = 2021L,
          step = 1,
          sep = ""
        ),
        radioButtons(
          "choix_dim_agent",
          "Choisir dimension:", list(
            "Sexe", "Region", "Corps", "Catégorie",
            "Grade", "Résidnece", "Position", "Age",
            "Ancienneté"
          )
        ),
      ),
      mainPanel(
        plotOutput(outputId = "distPlot")
      )
    )
  )),
  tabPanel("Absence", radioButtons(
    "choix_dim_abs",
    "Choisir dimension:", list(
      "Sexe", "Region", "Corps", "Catégorie",
      "Grade", "Résidnece", "Position", "Age",
      "Ancienneté"
    )
  ))
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    if (input$choix_dim_agent == "Sexe") {
      selected <- repartition_sexe[repartition_sexe$Annee == input$year, ]
      ggplot(selected, aes(
        x = factor(Mois, month.abb),
        y = NobmreAgent,
        fill = gender
      )) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = NobmreAgent), vjust = 0) +
        theme_fivethirtyeight() +
        xlab("Sexe") +
        ggtitle()
    }
  })
}

shinyApp(ui = ui, server = server)
