grade <- read_csv("data/grade.csv")

grade = grade[,!grepl("Total",colnames(grade))]

l = length(grade[1,])

colnames(grade) = c("periode",grade[1,][2:l])

grade = grade[-1,]



totals = grade[c(1,14,27,40,53,66),]

grade = grade[-c(1,14,27,40,53,66),]

length(grade$periode)/12

j = 1
for(i in 2017:2021){
  for(k in 1:12){
    grade$periode[j] = paste0(grade$periode[j],"_",i)
    j = j + 1
  }
}
n= nrow(grade)*(ncol(grade)-1)
df = data.frame(year = rep(NA,n) , month = rep(NA,n) , position = rep(NA,n), n = rep(NA,n) )

df$month = as.numeric((df$month))
df$n = as.numeric((df$n))

write.csv(df,"./data/repartition_grade_clean.csv",col.names = T)


k = 1
for(i in 1:nrow(grade)){
  date = grade[i,1][[1]]
   s=strsplit(date,"_")[[1]]
   m=as.numeric(s[1])
   y=as.numeric(s[2])
   
   for(j in 2:(ncol(grade))){
     df$year[k] = y
     df$month[k] = m
     df$position[k] = names(grade[i,j])
     df$n[k] = grade[i,j]
     k = k +1
   }
   
   
}


frame = df[df$position %in% c("Gestionnaire Général","Gestionnaire en Chef") & df$year == "2017",]

frame$year = as.numeric((frame$year))

frame$n = as.numeric((frame$n))

frame$month <- as.Date(paste0("01-", frame$month, "-2014"), "%d-%m-%Y")

ggplot(
  frame,
  aes(
    x = month,
    y = n,
    color = position
  )
) +
  geom_line(size = 1.8) +
  scale_x_date(date_minor_breaks = "1 month",date_labels = "%B")+
  ggthemes::theme_hc(base_size = 18) 
  



tdf = t(totals)

colnames(tdf) = tdf[1,]
tdf= tdf[-1,]

tdf = data.frame(tdf)
tdf$Total.général = as.numeric(tdf$Total.général)

tdf = tdf[order(tdf$Total.général,decreasing = T),]
write.csv(tdf,"./data/repartition_grade_total_clean.csv",col.names = T)



rownames(tdf[1:10,])
tdf$position = rownames(tdf)
tdf$position <- factor(tdf$position, levels = tdf$position)
library(scales)
ggplot(tdf[1:10,], aes(x = Total.général, y = position,fill = Total.général)) +geom_col() +
  scale_y_discrete(limits=rev) + scale_x_continuous(labels = label_comma())

