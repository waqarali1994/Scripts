library(ggplot2)
ggplot(data = scatter_, aes(x=   , y=))+geom_jitter()+
  geom_smooth(method = "lm", se=T, level=0.95)
nrow(scatter_)

Q1<-quantile(scatter_$Relative_Chlorophyll, .25);Q1
Q3<-quantile(scatter_$Relative_Chlorophyll, .75);Q3
IQR<-IQR(scatter_$Relative_Chlorophyll);IQR

Up<-73.4375+(1.5*IQR)
Low<-11.6775-(1.5*IQR)

subset(scatter_,scatter_$Relative_Chlorophyll>(11.6775-(1.5*IQR))&scatter_$Relative_Chlorophyll<(73.4375+(1.5*IQR))
       
 IQR_value <- IQR(scatter_$Relative_Chlorophyll)
 subset_data <- subset(scatter_, scatter_$Relative_Chlorophyll > (11.6775 - (1.5*IQR_value)) & scatter_$Relative_Chlorophyll < (73.4375 + (1.5*IQR_value)))
 View(subset_data)
boxplot(subset_data$Relative_Chlorophyll) 
