data = read.csv("data/categorie.csv")
data = t(data)
colnames(data) = data[1,]
data = data[-1,]
data = as.data.frame(data)
x = row.names(data)
for(i in 1:nrow(data)){
  spl = strsplit(x[i],"_")[[1]]
  data$year[i] = as.numeric(spl[2])
  data$month[i] = as.numeric(gsub("X","",spl[1]))
  data$date[i] = format.Date(make_date(year = data$year[i], month = data$month[i], day = 1))
  }

data_title = data[,c(1,6,12,15,16,17,18)]
data_title = data_title[order(data_title$year),]
data_title$`Total général` =as.numeric(data_title$`Total général`)

data_title = na.omit(data_title)


data_title$`Total général`

ggplot(data_title, aes(x=as.Date(data_title$date), y=`Total général`)) + geom_line(size= 1.5) + ggthemes::theme_hc()





write.csv(data_title,"./data/repartition_cat_clean.csv",col.names = T)



selected <- data_title[data_title$year == "2018", c("Cadre","Exécution","Maîtrise","month") ]
selected = selected[order(selected$month),]
selected$Cadre = as.numeric(selected$Cadre)
selected$Exécution = as.numeric(selected$Exécution)
selected$Maîtrise = as.numeric(selected$Maîtrise)
selected$month =  factor(selected$month,levels = c(1:12),labels = month.abb)
      
library("reshape2")

data_reshape <- melt(selected, id = "month")


ggplot(data_reshape,            
       aes(x = month,
           y = value,
           color = variable, group = variable)) + 
  geom_line( size = 1)+ 
  ggthemes::theme_hc() + 
  ylim(1000,5000) 


ggplot(selected,aes(month,group = 1))   + ylim(1000,5000) +
geom_line(aes(y = Cadre), color = "blue" , size = 1) +
  geom_line(aes(y = Exécution), color = "red",size = 1) +
  geom_line(aes(y = Maîtrise), color = "green",size = 1) + ggthemes::theme_hc()



data_cat = data[,-c(1,6,12,15)]

data_cat = data_cat[1:(nrow(data_cat)-1),]




write.csv(data_cat,"./data/repartition_cat_id_clean.csv",col.names = T)


ggplot(data_cat,            
       aes(x = month,
           y = value,
           color = variable, group = variable)) + 
  geom_line( size = 1)+ 
  ggthemes::theme_hc() + 
  ylim(1000,5000)











repartition_cat_id = read.csv("data/repartition_cat_id_clean.csv")

selected <- repartition_cat_id[
  repartition_cat_id$year == "2018",
  c("month", paste0("X", 1))
]
print(selected)
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
  xlab("Mois") +
  ylab("Nombre") +
  ggtitle(
    paste(
      "Répartition de nombre d'agent selon la catégorie pour l'année"
    )
  ) +
  theme(plot.title = element_text(size = 15))  












