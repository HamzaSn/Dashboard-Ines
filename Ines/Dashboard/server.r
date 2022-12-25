source("./data.r")
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
        x = "Age",
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
        x = "Age",
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
  
  output$ageMoyen <- renderText({ 
    ages = repartition_age[repartition_age$year == input$yearAge, ]$age
    ns = repartition_age[repartition_age$year == input$yearAge, ]$n
    res = sum(ages*ns,na.rm = T)/sum(ns,na.rm = T)
    res= as.character(round(res))
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
  
  output$histCorps <- renderPlot({
    selected <- repartition_corps[repartition_corps$year == input$yearCorps & !(repartition_corps$corps %in% c("Unknown","Total général")), ]
  
    ggplot(selected, aes(
      x = factor(month.abb[selected$month],month.abb ),
      y = n,
      fill = corps
    )) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_hc(base_size = 18) +
      xlab("Corps") +
      ggtitle(paste("Répartition de nombre d'agent selon le Corps pour l'année", input$yearSexe))
  })
  
  output$histMetier <- renderPlot({
    selected <- repartition_metier[ repartition_metier$mois == 12 & repartition_metier$annee == input$yearMetier, ]
    
    ggplot(selected, aes(
      x = factor(month.abb[selected$mois],month.abb ),
      y = Nombre,
      fill = Metier
    )) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_hc(base_size = 18) +
      xlab("Metier") +
      ggtitle(paste("Répartition selon les metiers", input$yearMetier))
  })
  
  
  output$piePlotMetier <- renderPlot({
    frame <- repartition_metier[ repartition_metier$Metier %in% input$selectedMetier & repartition_metier$mois == "12" & repartition_metier$annee == input$yearMetier & !is.na(repartition_metier$Nombre), ]
    
    data <- data.frame(
      category = frame$Metier,
      count = frame$Nombre
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
    data$label <- paste0(data$category,"\n",ceiling(data$count/sum(data$count)*100),"%")
    
    # Make the plot
    ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      geom_rect() +
      geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void(base_size = 18) +
      theme(legend.position = "none")
  })
  
  output$metier <- renderPlot({
    frame <- repartition_metier[repartition_metier$Metier %in% input$selectedMetier & repartition_metier$annee == input$yearMetier, ]
    frame$mois <- as.Date(paste0("01-", frame$mois, "-1999"), "%d-%m-%Y")
    
    ggplot(
      frame,
      aes(
        x = mois,
        y = Nombre,
        color = Metier
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
    data$label <- paste0(data$category,"\n",round(data$count/sum(data$count)*100),"%")
    
    # Make the plot
    ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      geom_rect() +
      geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  
  
  output$histRecrutement <- renderPlot({
    selected <- repartition_recrutement[repartition_recrutement$annee == input$yearRecrutement, ]
    ggplot(selected, aes(
      x = factor(month.abb[selected$mois],month.abb ),
      y = NombreAgent,
      fill = lib.type
    )) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_hc(base_size = 18) +
      xlab("Type de recrutement") +
      ggtitle(paste("Répartition de nombre d'agent selon le type de recrutement l'année", input$yearSexe))
  })
  
  
  output$piePlotRecrutement <- renderPlot({
    frame <- repartition_recrutement[repartition_recrutement$mois == "12" & repartition_recrutement$annee == input$yearRecrutement & !is.na(repartition_recrutement$NombreAgent), ]
    
    data <- data.frame(
      category = frame$lib.type,
      count = frame$NombreAgent
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
    data$label <- paste0(data$category,"\n",ceiling(data$count/sum(data$count)*100),"%")
    
    # Make the plot
    ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      geom_rect() +
      geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void(base_size = 18) +
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
  
  
  output$positionDepart <- renderPlot({
    frame <- repartition_position_depart[repartition_position_depart$lib.depart %in% input$selectedDepart & repartition_position_depart$annee == input$yearDepart, ]
    frame$mois <- as.Date(paste0("01-", frame$mois, "-1999"), "%d-%m-%Y")
    
    ggplot(
      frame,
      aes(
        x = mois,
        y = Nombre,
        color = lib.depart
      )
    ) +
      geom_line(size = 1.8) +
      scale_x_date(date_minor_breaks = "1 month", date_labels = "%B") +
      ylab("Nombre d'agent") +
      ggthemes::theme_hc(base_size = 18)
  })
  
  
  output$positionAdmin <- renderPlot({
    frame <- repartition_position_admin[repartition_position_admin$lib.position.administrative %in% input$selectedAdmin & repartition_position_admin$annee == input$yearAdmin, ]
    frame$mois <- as.Date(paste0("01-", frame$mois, "-1999"), "%d-%m-%Y")
    
    ggplot(
      frame,
      aes(
        x = mois,
        y = Nombre,
        color = lib.position.administrative
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
  
  
  output$residenceTotal1 <- renderPlot({
    data <- data.frame(
      category = repartition_residence_globale1$X.1,
      count = repartition_residence_globale1$nomber.de.residence
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
    data$label <- paste0(data$category,"\n",round(data$count/sum(data$count)*100),"%")
    
    # Make the plot
    ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      geom_rect() +
      geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none") + 
      ggtitle("Appartenance geographique")
    
    
  })
  
  
  output$residenceTotal2 <- renderPlot({
    data <- data.frame(
      category = repartition_residence_globale2$X.1,
      count = repartition_residence_globale2$nomber.de.residence
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
    data$label <- paste0(data$category,"\n",round(data$count/sum(data$count)*100),"%")
    
    # Make the plot
    ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      geom_rect() +
      geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none") + 
      ggtitle("Domaine de pilotage")
    
    
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
      theme_hc(base_size = 18) +
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
    data$label <- paste0(data$category,"\n",round(data$count/sum(data$count)*100),"%")
    
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