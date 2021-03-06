install.packages("ggfortify")
library(ggfortify)
attach(df_93)
cor(smoke_total,master.PHD)
summary(aov(smoke_total~master.PHD))
attach(df_106)
cor(smoke_total,smoke_male)
summary(aov(smoke_total~smoke_male))
cor(smoke_total,smoke_female)
summary(aov(smoke_total~smoke_female))
cor(smoke_total,vocational_high)
summary(aov(smoke_total~vocational_high))
cor(smoke_total,college)
summary(aov(smoke_total~college))
cor(smoke_total,junior_college)
summary(aov(smoke_total~junior_college))
cor(smoke_total,df_106$children)
summary(aov(smoke_total~df_106$children))
cor(smoke_total,df_106$adult)
summary(aov(smoke_total~df_106$adult))
cor(smoke_total,df_106$elder)
summary(aov(smoke_total~df_106$elder))
cor(smoke_total,gender_ratio)
summary(aov(smoke_total~gender_ratio))
cor(smoke_total,master.PHD)
summary(aov(smoke_total~master.PHD))
detach(df_106)

summary(lm(smoke_total ~ college + master.PHD, data = df_93))
autoplot(df_93.lm)
shapiro.test(df_93.lm$residuals)#???????????????????????????????????????
df_95.lm<-lm(smoke_total ~ smoke_male + college+junior_college+adult+master.PHD, data = df_95)
summary(lm(smoke_total ~ smoke_male + college+junior_college+adult+master.PHD, data = df_95))
autoplot(df_95.lm)
shapiro.test(df_95.lm$residuals)

