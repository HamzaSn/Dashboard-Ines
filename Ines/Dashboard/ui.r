source("data.r")


dbHeader <-
  dashboardHeader(title = "La Poste Tunisienne : Ressource Humaine", titleWidth = 500)

dbHeader$children[[4]] <-
  tags$img(src = 'logo.png',
           height = '100',
           class = "logo-img")

ui <- dashboardPage(dbHeader,
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Home", tabName = "home", icon = icon("home")),
                        menuItem("Sexe", tabName = "sexe", icon = icon("user")),
                        menuItem(
                          "Catégorie",
                          tabName = "cat",
                          icon = icon("building-user")
                        ),
                        menuItem(
                          "Grade",
                          tabName = "grade",
                          icon = icon("graduation-cap")
                        ),
                        menuItem("Age", tabName = "age", icon = icon("users")),
                        menuItem("Corps", tabName = "corps", icon = icon("user-circle")),
                        menuItem("Residence", tabName = "residence", icon = icon("building")),
                        menuItem(
                          "Position-depart",
                          tabName = "positionDepart",
                          icon = icon("user-doctor")
                        ),
                        menuItem(
                          "Position-administrative",
                          tabName = "positionAdmin",
                          icon = icon("user-doctor")
                        ),
                        menuItem("Recrutement", tabName = "recrutement", icon = icon("briefcase")),
                        menuItem("Metier", tabName = "metier", icon = icon("laptop"))
                        
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                      ),
                      tabItems(
                        tabItem(
                          tabName = "home",
                          fluidRow(
                            column(2, ),
                            column(10, tags$img(
                              src = "myPoste.jpg",
                              height = "60%" ,
                              width = "80%"
                            )),
                            column(2,),
                            column(
                              8,
                              h1("Tableau de board des ressources humaine", style = "margin:70px 0 70px 0")
                            ),
                            plotOutput("totalAgent"),
                          ),
                          
                        ),
                        tabItem(tabName = "sexe",
                                fluidRow(column(4,),
                                         column(
                                           4,
                                           sliderInput(
                                             inputId = "yearSexe",
                                             label = "Choisir l'année:",
                                             min = 2017L,
                                             max = 2021L,
                                             value = 2021L,
                                             step = 1,
                                             sep = ""
                                           )
                                         )),
                                fluidRow(
                                  splitLayout(
                                    cellWidths = c("70%", "30%"),
                                    plotOutput("histSexe"),
                                    plotOutput("piePlotSexe")
                                  )
                                )),
                        tabItem(
                          tabName = "cat",
                          fluidRow(column(4,),
                                   column(
                                     4,
                                     sliderInput(
                                       inputId = "yearCat",
                                       label = "Choisir l'année:",
                                       min = 2017L,
                                       max = 2021L,
                                       value = 2021L,
                                       step = 1,
                                       sep = ""
                                     )
                                   )),
                          fluidRow(
                            splitLayout(
                              cellWidths = c("70%", "30%"),
                              plotOutput("catDistPlot1"),
                              plotOutput("piePlotCat")
                            ),
                            br(),
                            br(),
                            fluidRow(column(4,),
                                     column(
                                       4,
                                       sliderInput(
                                         inputId = "id_cat",
                                         label = "Choisir l'id de catégorie:",
                                         min = 1L,
                                         max = 11L,
                                         value = 10L,
                                         step = 1,
                                         sep = ""
                                       )
                                     )),
                            plotOutput("catIdPlot"),
                          )
                        ),
                        tabItem(tabName = "grade",
                                fluidRow(
                                  column(
                                    8,
                                    selectInput(
                                      inputId = "selectedGrades",
                                      label = "Choisir les grades a visualiser",
                                      choices = unique(repartition_grade$position),
                                      multiple = TRUE,
                                      selected = "Gestionnaire en Chef"
                                    )
                                  ),
                                  column(
                                    4,
                                    sliderInput(
                                      inputId = "yearGrades",
                                      label = "Choisir l'année:",
                                      min = 2017L,
                                      max = 2021L,
                                      value = 2017L,
                                      step = 1,
                                      sep = ""
                                    )
                                  )
                                ),
                                column(10, plotOutput("grades"))),
                        
                        tabItem(
                          tabName = "age",
                          h1("Pyramide des ages"),
                          fluidRow(
                            column(4,),
                            column(10, box(width = 10,
                                          fluidRow(
                                            column(5, h2("Moyenne d'age")),
                                            column(5, h2(textOutput("ageMoyen")))
                                          ))),
                            fluidRow(column(4, ),
                                     column(
                                       4,
                                       sliderInput(
                                         inputId = "yearAge",
                                         label = "Choisir l'année:",
                                         min = 2017L,
                                         max = 2021L,
                                         value = 2017L,
                                         step = 1,
                                         sep = ""
                                       )
                            
                          )
                          ),
                          fluidRow(
                            column(6,
                                   plotOutput("distAge")),
                            column(6,
                                   plotOutput("distAge2"))
                          ))
                        ),
                        tabItem(
                          tabName = "corps",
                          fluidRow(column(4, ),
                                   column(
                                     4,
                                     sliderInput(
                                       inputId = "yearCorps",
                                       label = "Choisir l'année:",
                                       min = 2017L,
                                       max = 2021L,
                                       value = 2017L,
                                       step = 1,
                                       sep = ""
                                     )
                                   )),
                          fluidRow(column(8,
                                          plotOutput("histCorps"), ),
                                   column(4,
                                          plotOutput("piePlotCorps")))
                        ),
                        tabItem(
                          tabName = "metier",
                          fluidRow(column(4, selectInput(
                            inputId = "selectedMetier",
                            label = "Choisir les metiers a visualiser",
                            choices = unique(repartition_metier$Metier),
                            multiple = TRUE,
                            selected = c("Back Office","Front Office","Tri")
                          )),
                                   column(
                                     4,
                                     sliderInput(
                                       inputId = "yearMetier",
                                       label = "Choisir l'année:",
                                       min = 2017L,
                                       max = 2021L,
                                       value = 2017L,
                                       step = 1,
                                       sep = ""
                                     )
                                   )),
                          fluidRow(
                            column(4,plotOutput("piePlotMetier")),
                            column(8,plotOutput("metier"))
                            ),
                          fluidRow(plotOutput("histMetier"))
                        ),
                        tabItem(
                          tabName = "residence",
                          fluidRow(column(6, plotOutput("residenceTotal1")),
                                   column(6, plotOutput("residenceTotal2"))),
                          fluidRow(column(4,),
                                   column(
                                     4,
                                     sliderInput(
                                       inputId = "yearResidence",
                                       label = "Choisir l'année:",
                                       min = 2017L,
                                       max = 2021L,
                                       value = 2017L,
                                       step = 1,
                                       sep = ""
                                     )
                                   )),
                          fluidRow(
                            selectInput(
                              inputId = "selectedLIB",
                              label = "Choisir les Libellé de residence a visualiser",
                              choices = unique(repartition_residence$LIB),
                              multiple = TRUE,
                              selected = repartition_residence$LIB[1:10],
                              width = "95%"
                            )
                          ),
                          fluidRow(column(12, plotOutput("residence")))
                        ),
                        tabItem(tabName = "positionDepart",
                                h1("Depart"),
                                fluidRow(
                                  column(
                                    8,
                                    selectInput(
                                      inputId = "selectedDepart",
                                      label = "Choisir les grades a visualiser",
                                      choices = unique(repartition_position_depart$lib.depart),
                                      multiple = TRUE,
                                      selected = repartition_position_depart$lib.depart[1:3]
                                    )
                                  ),
                                  column(
                                    4,
                                    sliderInput(
                                      inputId = "yearDepart",
                                      label = "Choisir l'année:",
                                      min = 2017L,
                                      max = 2021L,
                                      value = 2021,
                                      step = 1,
                                      sep = ""
                                    )
                                  ),
                                  
                                  column(10, plotOutput("positionDepart"))
                                )),
                        tabItem(
                          tabName = "positionAdmin",
                          h1("Administrative"),
                          fluidRow(
                            column(
                              8,
                              selectInput(
                                inputId = "selectedAdmin",
                                label = "Choisir les position administrative a visualiser",
                                choices = unique(repartition_position_admin$lib.position.administrative),
                                multiple = TRUE,
                                selected = "En activité"
                              )
                            ),
                            column(
                              4,
                              sliderInput(
                                inputId = "yearAdmin",
                                label = "Choisir l'année:",
                                min = 2017L,
                                max = 2021L,
                                value = 2021,
                                step = 1,
                                sep = ""
                              )
                            ),
                            column(10, plotOutput("positionAdmin"))
                          )
                          
                        ),
                        
                        tabItem(
                          tabName = "recrutement",
                          fluidRow(column(4, ),
                                   column(
                                     4,
                                     sliderInput(
                                       inputId = "yearRecrutement",
                                       label = "Choisir l'année:",
                                       min = 2017L,
                                       max = 2021L,
                                       value = 2017L,
                                       step = 1,
                                       sep = ""
                                     )
                                   )),
                          fluidRow(column(8,
                                          plotOutput("histRecrutement"), ),
                                   column(4,
                                          plotOutput("piePlotRecrutement")))
                        )
                      )
                    ))
