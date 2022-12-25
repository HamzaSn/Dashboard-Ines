residence_globale <- read.csv("./data/residence-global.csv")

df1 <- residence_globale[1:2,]
df2 <- residence_globale[4:6,]


write.csv(df1,"./data/residence_globale_clean1.csv",col.names = T,row.names = F)
write.csv(df2,"./data/residence_globale_clean2.csv",col.names = T,row.names = F)

data <- data.frame(
  category = df1$X.1,
  count = df1$nomber.de.residence
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


####

data <- data.frame(
  category = df2$X.1,
  count = df2$nomber.de.residence
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

library(ggplot2)
# Make the plot
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") + 
  ggtitle("Domaine de pilotage")

