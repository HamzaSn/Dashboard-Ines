corps <- read_csv("data/corps.csv")
corps$`Total général` = NULL

n= nrow(corps)*(ncol(corps))
df = data.frame(year = rep(NA,n) , month = rep(NA,n) , corps = rep(NA,n), n = rep(NA,n) )


k=1
for (row in 1:nrow(corps)){
  
  curr_corps = corps[row,1]
  
  for(col in 2:ncol(corps)){
    split = strsplit(names(corps[row,col]),"_")[[1]]
    month = as.numeric(split[1])
    year = as.numeric(split[2])
    n = corps[row,col]
    df[k,] = c(year,month,curr_corps,n)
    k=k+1
    
  }
  
}
df = as.data.frame(df[1:300,])

df = df[order(df$year,df$month),]


write.csv(df,"./data/repartition_corps_clean.csv",col.names = T)



frame = df[df$corps %in% c("Administratif","Technique") & df$year == "2017",]

frame$month <- as.Date(paste0("01-", frame$month, "-2014"), "%d-%m-%Y")

library(ggplot2)
ggplot(
  frame,
  aes(
    x = month,
    y = n,
    color = corps
  )
) +
  geom_line(size = 1.8) +
  scale_x_date(date_minor_breaks = "1 month",date_labels = "%B")+
  ggthemes::theme_hc(base_size = 18) 



frame = df[df$month == "1" & df$year == "2021" & !is.na(df$n) & df$corps != "Total général",]



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
