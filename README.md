# R-Final-Project
result<-read.csv(file.choose())
result_ratio <- data.frame(result$year,result$city,result$smoke_total)
colnames(result_ratio)<-c("year","city","smoke_rate")
result_ratio<-result_ratio%>%
  mutate(`20_65`=rowSums(result[,25:34])/result$total
         ,`0_20`=rowSums(result[,20:25])/result$total
         ,`0_15`=rowSums(result[,20:24])/result$total
         ,`65up`=rowSums(result[,34:41])/result$total
         ,gender=result$gender
         ,`gender_20-65`=result$wgender
         ,hedu_nv = rowSums(result[,c(7,11)])/result$total_edu
         ,hedu = rowSums(result[,c(7,11,9)])/result$total_edu
         ,vocatoinal = rowSums(result[,c(9,16,17)])
         ,p_income = result$person.avg)
write.csv(result_ratio,file = "result_ratio.csv")

{
  relate_year <- data.frame(year = 93:106)
  for(i in 1:14){
   y = 92+i
   mld1<-filter(result_ratio,year == y)
   for(j in 1:10){
    relate_year[i,1+j]<-cor(mld1$smoke_rate,mld1[,3+j])
   }
  }
  colnames(relate_year)[2:11]<-colnames(result_ratio[,4:13])
}


result_ratio%>%
  gather("factor","relate",4:13)%>%
  ggplot(aes(smoke_rate,relate))+
  geom_point()+
  facet_wrap(~factor)
test
