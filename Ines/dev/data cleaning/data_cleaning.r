data = read.csv("./data/repartition_sexe.csv")
data_f = data.frame(t(data[1,]))

data_f$id_time = row.names((data_f))
data_f = data_f[-1,]
row.names(data_f) = NULL

for (i in c(1:length(data_f$X1))){
  a = strsplit(data_f$id_time[i] ,"_")[[1]]
  data_f$mois[i] = month.abb[as.numeric(gsub("X","",a[1]))]
  data_f$annee[i] = a[2]
}
data_f$id_time = NULL

colnames(data_f) = c("NobmreAgent","Mois","Annee")
data_f <- data_f[order(data_f$Annee),]


data_f$gender = rep("F",nrow(data_f))

####

data_h = data.frame(t(data[2,]))

data_h$id_time = row.names((data_h))
data_h = data_h[-1,]
row.names(data_h) = NULL

for (i in c(1:nrow(data_h))){
  print(i)
  a = strsplit(data_h$id_time[i] ,"_")[[1]]
  data_h$mois[i] = month.abb[as.numeric(gsub("X","",a[1]))]
  data_h$annee[i] = a[2]
}
data_h$id_time = NULL

colnames(data_h) = c("NobmreAgent","Mois","Annee")
data_h <- data_h[order(data_h$Annee),]

data_h$gender = rep("H",nrow(data_h))

data_clean = rbind(data_h,data_f)

data_clean <- data_clean[order(data_clean$Annee),]
data_clean = (head(data_clean,nrow(data_clean)-2))

data_clean$NobmreAgent = as.numeric((data_clean$NobmreAgent))

write.csv(data_clean,"./data/repartition_sexe_clean.csv",col.names = T)

str(data_clean)
