residence <- residence_ann_mois


residence = residence[,!grepl("Total",colnames(residence))]

colnames(residence) = c("periode",colnames(residence)[2:ncol(residence)])


totals = residence[c(1,14,27,40,53,66),]

residence = residence[-c(1,14,27,40,53,66),]

length(residence$periode)/12

j = 1
for(i in 2017:2021){
  for(k in 1:12){
    residence$periode[j] = paste0(residence$periode[j],"_",i)
    j = j + 1
  }
}
n= nrow(residence)*(ncol(residence)-1)
df = data.frame(year = rep(NA,n) , month = rep(NA,n) , id_res = rep(NA,n), n = rep(NA,n) )

df <- apply(df,2,as.character)

write.csv(as.data.frame(df),"./data/repartition_residence_clean.csv",col.names = T,row.names = F)


k = 1
for(i in 1:nrow(residence)){
  date = residence[i,1][[1]]
  s=strsplit(date,"_")[[1]]
  m=as.numeric(s[1])
  y=as.numeric(s[2])
  
  for(j in 2:(ncol(residence))){
    df$year[k] = y
    df$month[k] = m
    df$id_res[k] = names(residence[i,j])
    df$n[k] = residence[i,j]
    k = k +1
    print(k)
  }
  
  
}


for (i in 1:nrow(df)){
  df$LIB[i] = residenceName[residenceName$CODE_RESIDENCE == df$id_res[i],]$LIB
  df$AppGeo[i] =  residenceName[residenceName$CODE_RESIDENCE == df$id_res[i],]$AppGeo
  df$ServCent[i] =  residenceName[residenceName$CODE_RESIDENCE == df$id_res[i],]$ServCent
  print(i)
}
df = as.data.frame(df)

frame = df[df$LIB %in% c("Pôle Inf Sousse","Pôle Inf Tunis")  & df$year == "2018"& df$month == "1" ,]

frame$year = as.numeric((frame$year))

frame$n = as.numeric((frame$n))

frame$month <- as.Date(paste0("01-", frame$month, "-2014"), "%d-%m-%Y")



barplot(table(frame$LIB))
ggplot(
  frame,
  aes(
    x = LIB,
    y = n,
    fill=ServCent
  )
) +
  geom_bar(size = 1.8,stat="identity") +
  scale_y_discrete(1:100)+
  ggthemes::theme_hc(base_size = 18) 




tdf = t(totals)

colnames(tdf) = tdf[1,]
tdf= tdf[-1,]

tdf = data.frame(tdf)
tdf$Total.général = as.numeric(tdf$Total.général)

tdf = tdf[order(tdf$Total.général,decreasing = T),]
write.csv(tdf,"./data/repartition_residence_total_clean.csv",col.names = T)



rownames(tdf[1:10,])
tdf$position = rownames(tdf)
tdf$position <- factor(tdf$position, levels = tdf$position)
library(scales)
ggplot(tdf[1:10,], aes(x = Total.général, y = position,fill = Total.général)) +geom_col() +
  scale_y_discrete(limits=rev) + scale_x_continuous(labels = label_comma())

