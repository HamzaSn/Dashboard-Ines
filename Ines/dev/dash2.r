library(shiny)
library(markdown)
library(ggplot2)
library(ggthemes)
library(shinydashboard)
library(reshape2)
library(dplyr)

repartition_sexe <- read.csv("./data/repartition_sexe_clean.csv")
repartition_cat <- read.csv("./data/repartition_cat_clean.csv")
repartition_cat_id <- read.csv("./data/repartition_cat_id_clean.csv")
repartition_grade <- read.csv("./data/repartition_grade_clean.csv")
repartition_grade_total <- read.csv("./data/repartition_grade_total_clean.csv")
repartition_age <- read.csv("./data/repartition_age_clean.csv")
repartition_corps <- read.csv("./data/repartition_corps_clean.csv")
repartition_residence <- read.csv("./data/repartition_residence_clean.csv")
repartition_age$n <- as.numeric(repartition_age$n)
repartition_age$age <- as.numeric(repartition_age$age)



ui <- dashboardPage(
    dashboardHeader(title = "La Poste Tunisienne : Ressource Humaine", titleWidth = 500),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Sexe", tabName = "sexe", icon = icon("user")),
            menuItem("Catégorie", tabName = "cat", icon = icon("dashboard")),
            menuItem("Grade", tabName = "grade", icon = icon("graduation-cap")),
            menuItem("Age", tabName = "age", icon = icon("users")),
            menuItem("Corps", tabName = "corps", icon = icon("user-circle")),
            menuItem("Residence", tabName = "residence", icon = icon("building"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "home",
                plotOutput("totalAgent"),
            ),
            tabItem(
                tabName = "sexe",
                fluidRow(
                  column(4,),
                  column(4,sliderInput(
                    inputId = "yearSexe",
                    label = "Choisir l'année:",
                    min = 2017L,
                    max = 2021L,
                    value = 2021L,
                    step = 1,
                    sep = ""
                  ))
                  ),
                fluidRow(
                    splitLayout(
                        cellWidths = c("70%", "30%"),
                        plotOutput("histSexe"),
                        plotOutput("piePlotSexe")
                    )
                )
            ),
            tabItem(
                tabName = "cat",
                fluidRow(column(4,),
                         column(4,sliderInput(
                           inputId = "yearCat",
                           label = "Choisir l'année:",
                           min = 2017L,
                           max = 2021L,
                           value = 2021L,
                           step = 1,
                           sep = ""
                         ))),
                fluidRow(
                    splitLayout(
                        cellWidths = c("70%", "30%"),
                        plotOutput("catDistPlot1"),
                        plotOutput("piePlotCat")
                    ),
                    br(),
                    br(),
                    sliderInput(
                        inputId = "id_cat",
                        label = "Choisir l'id de catégorie:",
                        min = 1L,
                        max = 11L,
                        value = 10L,
                        step = 1,
                        sep = ""
                    ),
                    plotOutput("catIdPlot"),
                )
            ),
            tabItem(
                tabName = "grade",
                fluidRow(
                    column(8, selectInput(
                        inputId = "selectedGrades",
                        label = "Choisir les grades a visualiser",
                        choices = unique(repartition_grade$position),
                        multiple = TRUE,
                        selected = "Gestionnaire en Chef"
                    )),
                    column(4, sliderInput(
                        inputId = "yearGrades",
                        label = "Choisir l'année:",
                        min = 2017L,
                        max = 2021L,
                        value = 2017L,
                        step = 1,
                        sep = ""
                    ))
                ),
                plotOutput("grades"),
                br(),
                br(),
                br(),
                br()
            ),
            tabItem(
                tabName = "age",
                fluidRow(
                  column(4,),
                    column(4, sliderInput(
                        inputId = "yearAge",
                        label = "Choisir l'année:",
                        min = 2017L,
                        max = 2021L,
                        value = 2017L,
                        step = 1,
                        sep = ""
                    ))
                ),
                column(
                    6,
                    plotOutput("distAge")
                ),
                column(
                    6,
                    plotOutput("distAge2")
                )
            ),
            tabItem(
                tabName = "corps",
                fluidRow(
                    column(8, selectInput(
                        inputId = "selectedCorps",
                        label = "Choisir les corps a visualiser",
                        choices = unique(repartition_corps$corps),
                        multiple = TRUE,
                        selected = "Administratif"
                    )),
                    column(4, sliderInput(
                        inputId = "yearCorps",
                        label = "Choisir l'année:",
                        min = 2017L,
                        max = 2021L,
                        value = 2017L,
                        step = 1,
                        sep = ""
                    ))
                ),
                fluidRow(
                    column(
                        8,
                        plotOutput("corps"),
                    ),
                    column(
                        4,
                        plotOutput("piePlotCorps")
                    )
                )
            ),
            tabItem(
                tabName = "residence",
                fluidRow(
                  column(4,),
                    column(4,sliderInput(
                      inputId = "yearResidence",
                      label = "Choisir l'année:",
                      min = 2017L,
                      max = 2021L,
                      value = 2017L,
                      step = 1,
                      sep = ""
                    ))
                ),
                fluidRow(
                  selectInput(
                    inputId = "selectedLIB",
                    label = "Choisir les Libellé de residence a visualiser",
                    choices = unique(repartition_residence$LIB),
                    multiple = TRUE,
                    selected = repartition_residence$LIB[1:10],
                    width = "95%"
                  )),
                fluidRow(
                  column(12,plotOutput("residence"))
                )
            )
        )
    )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
    output$distAge <- renderPlot({
        repartition_age[repartition_age$year == input$yearAge, ] %>%
            mutate(
                n = ifelse(gender == "Male", n * (-1),
                    n * 1
                )
            ) %>%
            ggplot(aes(x = age, y = n, fill = gender)) +
            coord_flip() +
            geom_bar(stat = "identity") +
            labs(
                title = "Pyramide des ages", x = "Age",
                y = "Population"
            ) +
            scale_y_continuous(labels = abs) +
            theme_hc(base_size = 18)
    })


    output$distAge2 <- renderPlot({
        repartition_age[repartition_age$year == input$yearAge, ] %>%
            ggplot(aes(x = age, y = n, fill = gender)) +
            geom_bar(stat = "identity") +
            labs(
                title = "Pyramide des ages", x = "Age",
                y = "Population"
            ) +
            scale_y_continuous(labels = abs) +
            theme_hc(base_size = 18)
    })

    output$gradesTotal <- renderPlot({
        library(scales)
        repartition_grade_total$position <- factor(repartition_grade_total$position, levels = repartition_grade_total$position)
        ggplot(repartition_grade_total[1:10, ], aes(x = Total.général, y = position, fill = Total.général)) +
            geom_col(show.legend = F) +
            scale_y_discrete(limits = rev) +
            xlab("nombre d'agent") +
            scale_x_continuous(labels = label_comma()) +
            theme_hc(base_size = 18) +
            ggtitle("Top 10 positions")
    })


    output$corps <- renderPlot({
        frame <- repartition_corps[repartition_corps$corps %in% input$selectedCorps & repartition_corps$year == input$yearCorps, ]
        frame$month <- as.Date(paste0("01-", frame$month, "-1999"), "%d-%m-%Y")

        ggplot(
            frame,
            aes(
                x = month,
                y = n,
                color = corps
            )
        ) +
            geom_line(size = 1.8) +
            scale_x_date(date_minor_breaks = "1 month", date_labels = "%B") +
            ylab("Nombre d'agent") +
            ggthemes::theme_hc(base_size = 18)
    })
    
    output$piePlotCorps <- renderPlot({
        frame <- repartition_corps[repartition_corps$month == "1" & repartition_corps$year == input$yearCorps & !is.na(repartition_corps$n) & repartition_corps$corps != "Total général" & repartition_corps$corps != "Unknown", ]

        data <- data.frame(
            category = frame$corps,
            count = frame$n
        )

        # Compute percentages
        data$fraction <- data$count / sum(data$count)

        # Compute the cumulative percentages (top of each rectangle)
        data$ymax <- cumsum(data$fraction)

        # Compute the bottom of each rectangle
        data$ymin <- c(0, head(data$ymax, n = -1))

        # Compute label position
        data$labelPosition <- (data$ymax + data$ymin) / 2

        # Compute a good label
        data$label <- paste0(data$category)

        # Make the plot
        ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
            geom_rect() +
            geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
            coord_polar(theta = "y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none")
    })
    
    output$grades <- renderPlot({
        frame <- repartition_grade[repartition_grade$position %in% input$selectedGrades & repartition_grade$year == input$yearGrades, ]
        frame$month <- as.Date(paste0("01-", frame$month, "-1999"), "%d-%m-%Y")

        ggplot(
            frame,
            aes(
                x = month,
                y = n,
                color = position
            )
        ) +
            geom_line(size = 1.8) +
            scale_x_date(date_minor_breaks = "1 month", date_labels = "%B") +
            ylab("Nombre d'agent") +
            ggthemes::theme_hc(base_size = 18)
    })

    output$residence <- renderPlot({
        frame <- repartition_residence[repartition_residence$LIB %in% input$selectedLIB & repartition_residence$year == input$yearResidence, ]
        frame$month <- as.Date(paste0("01-", frame$month, "-1999"), "%d-%m-%Y")

        ggplot(
          frame,
          aes(
            x = LIB,
            y = n,
            fill=ServCent
          )
        ) +
          coord_flip() +
          geom_bar(size = 1.8,stat="identity") +
          ggthemes::theme_hc(base_size = 18) + 
          scale_fill_discrete(name = "Service")
    })


    output$catIdPlot <- renderPlot({
        n <- as.numeric(gsub("X", "", input$id_cat))
        selected <- repartition_cat_id[
            repartition_cat_id$year == input$yearCat,
            c("month", paste0("X", input$id_cat))
        ]
        selected <- selected[order(selected$month), ]
        selected$month <- factor(selected$month,
            levels = c(1:12),
            labels = month.abb
        )
        data_reshape <- melt(selected, id = "month")
        ggplot(
            data_reshape,
            aes(
                x = month,
                y = value,
                color = variable, group = variable
            )
        ) +
            geom_line(size = 1.8) +
            ggthemes::theme_hc(base_size = 18) +
            xlab(ifelse(n <= 5, "Exécution", ifelse(n <= 7, "Maîtrise", "Cadre"))) +
            ylab("Nombre") +
            ggtitle(
                paste(
                    "Répartition de nombre d'agent selon la catégorie pour l'année",
                    input$yearCat
                )
            ) +
            theme(plot.title = element_text(size = 15))
    })

    output$histSexe <- renderPlot({
        selected <- repartition_sexe[repartition_sexe$Annee == input$yearSexe, ]
        ggplot(selected, aes(
            x = factor(selected$Mois, month.abb),
            y = NobmreAgent,
            fill = gender
        )) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_text(aes(label = NobmreAgent), vjust = 0) +
            theme_hc() +
            xlab("Sexe") +
            ggtitle(paste("Répartition de nombre d'agent selon le Sexe pour l'année", input$yearSexe))
    })


    output$piePlotSexe <- renderPlot({
        selected <- repartition_sexe[repartition_sexe$Annee == input$yearSexe, ]
        h <- round(mean(selected[selected$gender == "H", "NobmreAgent"]))
        f <- round(mean(selected[selected$gender == "F", "NobmreAgent"]))
        data <- data.frame(
            category = c("Homme", "Femme"),
            count = c(h, f)
        )

        # Compute percentages
        data$fraction <- data$count / sum(data$count)

        # Compute the cumulative percentages (top of each rectangle)
        data$ymax <- cumsum(data$fraction)

        # Compute the bottom of each rectangle
        data$ymin <- c(0, head(data$ymax, n = -1))

        # Compute label position
        data$labelPosition <- (data$ymax + data$ymin) / 2

        # Compute a good label
        data$label <- paste0(data$category, "\n moyen du \n nombre d'agent: ", data$count)

        # Make the plot
        ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
            geom_rect() +
            geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
            coord_polar(theta = "y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none")
    })

    output$totalAgent <- renderPlot({
        ggplot(repartition_cat, aes(x = as.Date(repartition_cat$date), y = repartition_cat$Total.général, fill = "red")) +
            geom_col(width = 7, show.legend = F) +
            theme_hc(base_size = 18) +
            ggtitle("Nombre total des agents 2017-2021") +
            xlab("Période") +
            ylab("Nombre d'agent") +
            coord_cartesian(ylim = c(8000, 10000))
    })

    output$catDistPlot1 <- renderPlot({
        selected <- repartition_cat[
            repartition_cat$year == input$yearCat,
            c("Cadre", "Exécution", "Maîtrise", "month")
        ]
        selected <- selected[order(selected$month), ]
        selected$Cadre <- as.numeric(selected$Cadre)
        selected$Exécution <- as.numeric(selected$Exécution)
        selected$Maîtrise <- as.numeric(selected$Maîtrise)
        selected$month <- factor(selected$month,
            levels = c(1:12),
            labels = month.abb
        )
        data_reshape <- melt(selected, id = "month")
        ggplot(
            data_reshape,
            aes(
                x = month,
                y = value,
                color = variable, group = variable
            )
        ) +
            geom_line(size = 1.8) +
            ggthemes::theme_hc(base_size = 18) +
            ylim(1000, 5000) +
            xlab("Mois") +
            ylab("Nombre") +
            ggtitle(
                paste(
                    "Répartition de nombre d'agent selon la catégorie pour l'année",
                    input$yearCat
                )
            ) +
            theme(plot.title = element_text(size = 15))
    })



    output$piePlotCat <- renderPlot({
        selected <- repartition_cat[
            repartition_cat$year == input$yearCat,
            c("Cadre", "Exécution", "Maîtrise", "month")
        ]
        selected <- selected[order(selected$month), ]
        selected$Cadre <- as.numeric(selected$Cadre)
        selected$Exécution <- as.numeric(selected$Exécution)
        selected$Maîtrise <- as.numeric(selected$Maîtrise)
        selected$month <- factor(selected$month,
            levels = c(1:12),
            labels = month.abb
        )
        data_reshape <- melt(selected, id = "month")
        a <- round(mean(data_reshape[data_reshape$variable == "Cadre", "value"]))
        b <- round(mean(data_reshape[data_reshape$variable == "Exécution", "value"]))
        c <- round(mean(data_reshape[data_reshape$variable == "Maîtrise", "value"]))
        data <- data.frame(
            category = c("Cadre", "Exécution", "Maîtrise"),
            count = c(a, b, c)
        )
        data$fraction <- data$count / sum(data$count)

        # Compute the cumulative percentages (top of each rectangle)
        data$ymax <- cumsum(data$fraction)

        # Compute the bottom of each rectangle
        data$ymin <- c(0, head(data$ymax, n = -1))

        # Compute label position
        data$labelPosition <- (data$ymax + data$ymin) / 2

        # Compute a good label
        data$label <- paste0("moyen du \n nombre d'agent: ", data$count)

        # Make the plot
        ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
            geom_rect() +
            geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
            coord_polar(theta = "y") +
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "none")
    })
}

shinyApp(ui = ui, server = server)
