library(dplyr)
library(readxl)
library(stats)
library(ggplot2)
suicide = read_excel("C:/Users/Administrator/Desktop/suicide.xlsx")

#1. How many children (<= 18 years) have died of which how many by 
#(a) hanging, (b) Poisson,(c) Burns, and (d) drowning

a = c("Hanging","Poisson","Burns","Drowning")

suicide_attempts = suicide%>%filter(`Age (yrs)`<= 18,`Method of attempt to suicide` %in% a,`Outcome of suicide` == "Died")%>%
  group_by(`Age (yrs)`,`Method of attempt to suicide`)%>%
  summarise(total = n())
View(suicide_attempts)

#2. How many students died due to (a) depression (b) love failure, 
#(c) Pain during menstruation (Dysmenorrhea), (d) failure in studies, 
#(e) threatned by some one to marry

b = c("Depression","Love failure","Abdominal pain (Dysmenorrhea)",
      "Chronic abdomen pain (Dysmenorrhea)",
      "Dysmmenorhea",
      "Failure in studies", 
      "Threatned by some one to marry")
View(b)

Suicide_reasons = suicide%>%filter(`Cause of suicide` %in% c("Depression",
                                                             "Love failure",
                                                             "Abdominal pain (Dysmenorrhea)",
                                                             "Chronic abdomen pain (Dysmenorrhea)",
                                                             "Dysmmenorhea",
                                                             "Failure in studies",
                                                             "Threatned by some one to marry"))%>%
  group_by(`Cause of suicide`) %>% summarise(Total =  n())
View(Suicide_reasons)

# 3. What is the ratio of male and female died to love failure?

love_failure_male = suicide%>%filter(`Cause of suicide` == "Love failure" & suicide$Sex == "Male")%>%group_by(`Cause of suicide`)%>%
  summarise(total_male = n())
View(love_failure_male)

love_failure_female = suicide%>%filter(`Cause of suicide` == "Love failure" & suicide$Sex == "Female")%>%group_by(`Cause of suicide`)%>%
  summarise(total_female = n())
View(love_failure_female)

ratio_love_failure = love_failure_male$total_male/love_failure_female$total_female
View(ratio_love_failure)

#4. How many students died due to Poisson?

suicide_poison = suicide%>%filter(`Method of attempt to suicide` == "Poisson")%>%
  summarise(total_poisson = n())
View(suicide_poison)

#5How many died due to love failure among 
#(a) upper middle class Hindu, 
#(b) lower class christian, 
#(c) Middle class Muslim

suicide_hindu = suicide%>%filter(SES == "Upper Middle" , Religion == "Hindu" , `Cause of suicide`== "Love failure" , `Outcome of suicide` == "Died")%>%
  group_by(SES,Religion,`Cause of suicide`,`Outcome of suicide`)%>%
  summarize(total_hindu = n())
View(suicide_hindu)


suicide_christian = suicide%>%filter(SES == "Lower" & Religion == "Christian",`Cause of suicide`== "Love failure" , `Outcome of suicide` == "Died")%>%
  group_by(SES,Religion)%>%summarize(total_christian = n())
View(suicide_christian)

suicide_Muslim= suicide%>%filter(SES == "Middle" & Religion == "Muslim",`Cause of suicide`== "Love failure" , `Outcome of suicide` == "Died")%>%
  group_by(SES,Religion)%>%
  summarize(total_Muslim = n())
View(suicide_Muslim)

#6. How many between the age of 19 - 20 years have died due to 
#(a) love failure, 
#(b) dowry death, 
#(c) quarreling with husband, 
#(d) failure in studies


suicide_a = suicide%>%filter(`Cause of suicide` == "Love failure",`Age (yrs)` == 19 | `Age (yrs)` == 20,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_a = n())
View(suicide_a)

suicide_b = suicide%>%filter(`Cause of suicide` == "Dowry death",`Age (yrs)` == 19 | `Age (yrs)` == 20,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_b = n())
View(suicide_b)

suicide_c = suicide%>%filter(`Cause of suicide` == "Querreling with husband",`Age (yrs)` == 19 | `Age (yrs)` == 20,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_c = n())
View(suicide_c)

suicide_d = suicide%>%filter(`Cause of suicide` == "Failure in studies",`Age (yrs)` == 19 | `Age (yrs)` == 20,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_d = n())
View(suicide_d)

#7. How many between at the age of 21 years have died by taking sleeping pills due to love failure

suicide_lovepills = suicide%>%filter(`Cause of suicide` == "Love failure",`Age (yrs)` < 21,`Outcome of suicide`=="Died")%>%
  group_by(`Cause of suicide`,`Outcome of suicide`)%>%
  summarise(total_d = n())
View(suicide_lovepills)

#8.How many have died death of husband? What is the age of the victim and belonging to which religion and SES?

suicide_husband = suicide%>%filter(Sex == "Male" , `Marital status` == "Married", `Outcome of suicide` == "Died")%>%
  group_by(Religion,SES)%>%
  summarise(total_hu = n())
View(suicide_husband)

#9. What is the ratio of married to unmarried persons who have died? How many married women died due to dowry deaths? 
#How many of these are Hindus and Muslims?

suicide_married = suicide%>%filter(`Marital status` == "Married", `Outcome of suicide` == "Died")%>%group_by(`Marital status`,`Outcome of suicide`)%>%
  summarise(total_married = n())
View(suicide_married)

suicide_unmarried = suicide%>%filter(`Marital status` == "Unmarried",`Outcome of suicide` == "Died")%>%group_by(`Marital status`,`Outcome of suicide`)%>%
  summarise(total_unmarried = n())
View(suicide_unmarried)

suicide_ratio = suicide_married$total_married/suicide_unmarried$total_unmarried
View(suicide_ratio)

 suicide_women = suicide%>%filter(Sex == "Female", `Cause of suicide` == "Dowry death",`Outcome of suicide` == "Died",Religion == "Hindu" | Religion =="Muslim")%>%
   group_by(Sex,`Cause of suicide`,Religion)%>%
   summarise(total_women = n())
 View(suicide_women)

road_accident = read_excel("C:/Users/Administrator/Desktop/Road Accident.xlsx")
 
# 10.What is the ratio of suicide and accidental deaths? What is the distribution of time of deaths? 
#How many persons belonging to Jains died due to depression?
 
suicide_death = suicide%>%filter(`Outcome of suicide`== "Died")%>%group_by(`Outcome of suicide`)%>%
  summarise(total_accident = n())
View(suicide_death)

road_death = road_accident%>%summarise(total_death = n())
View(road_death)

ratio = (suicide_death$total_accident/road_death$total_death)
View(ratio)
 
#11.State some of the hypothesis which may be formulated based on this data? 																

View(suicide)

ggplot(suicide,aes(x=suicide$`Outcome of suicide`))+geom_bar(aes(fill=suicide$`Outcome of suicide`))+facet_grid(suicide$Sex~sd$`Marital status`) #no of suicide death case under unmarried male and female are way to high as compared to married ones #H0:Unmarried people commit more suicide

ggplot(sd,aes(x=suicide$`Outcome of suicide`))+geom_bar(aes(fill=suicide$`Outcome of suicide`))+facet_grid(suicide$Sex~sd$`Marital status`+suicide$Occupation) #no of suicide death case under unmarried male and female students is way far thn any other occupation #married housewife commited suicide is more as compare to other #H0:Unmarried Students commited more suicide #H0:Married Housewife commit more suicide as compaired to married men
#H0: unmarried bussiness male comit more suicide followed by male coolie and machanic

ggplot(suicide,aes(x=suicide$`Outcome of suicide`,y=suicide$`Cause of suicide`))+
  geom_point(aes(fill =suicide$`Method of attempt to suicide`),col='Black')+
  theme_bw() + facet_wrap(~suicide$`Method of attempt to suicide`, scales="free",ncol=3)+
  geom_point(col="black",size=0.8)+
  theme(legend.position ='')+
  theme(legend.title = element_blank())
#on the basis of CAUSE OF SUICIDE AND METHOD OF ATTEMPT OF SUICIDE I CONCLUDED THAT

#hanging is mostly practiced way of suicide #poision is secound modtly used way of suicide #then burn comes just below poision #fall from height and drowning are almost eqaul in no of suicide but in drawning no of people survied is more
#electicity   is least practised


#12.	Is there sufficient data to test these hypothesis? 
#If so, use the available and test those hypothesis. 																

#Null Hypothesis: True difference is equal to 0
#alternate hypothesis : True difference is not equal to zero.
suicide$`Age (yrs)`[which(suicide$`Age (yrs)` == "18m")] = "18"
suicide$`Age (yrs)` <- as.numeric(suicide$`Age (yrs)`)
t.test(`Age (yrs)`~Sex,data=suicide)


#13.	What proportion have survived even after attempt to suicide?																

suicide_survived = suicide%>%filter(`Outcome of suicide` == "Survived")%>%group_by(`Outcome of suicide`)%>%
  summarise(total_survied = n())
View(suicide_survived)


# 14.	How many of the survivors brought within 60 minutes to hospital	


suicide_hospital = suicide%>%filter(`Time interval between attempt to suicide and bring to hospital`<= "60 minutes",
                                    `Time interval between attempt to suicide and bring to hospital`!= "1 hour"&
                                      `Time interval between attempt to suicide and bring to hospital` != "2 hours"&
                                      `Time interval between attempt to suicide and bring to hospital` != "2 days"&
                                      `Time interval between attempt to suicide and bring to hospital` != "3 hours"&
                                      `Time interval between attempt to suicide and bring to hospital` != "4 hours"&
                                      `Time interval between attempt to suicide and bring to hospital` != "5 hours")%>%
  group_by(`Time interval between attempt to suicide and bring to hospital`)%>%
  summarise(total_hospital = n())
View(suicide_hospital)



#15.	Considering outcome of suicide as dependent fit a logistic regression model taking all 
#other variables as independent variables.																

suicide$`Outcome of suicide`=ifelse(suicide$`Outcome of suicide`=="Died"|suicide$`Outcome of suicide`== "Survied","1","0")
View(suicide)
str(suicide$`Outcome of suicide`)

suicide$`Outcome of suicide` = as.factor(suicide$`Outcome of suicide`)


# Set train and Test data 

pd = sample(2, nrow(suicide), prob = c(.8,.2), replace = TRUE)

train= suicide[pd==1,]
View(train)
test = suicide[pd== 2,]
View(test)

## create Model 

model = glm(suicide$`Outcome of suicide`~ suicide$`Age (yrs)` +
              suicide$Sex+
              suicide$Religion+
              suicide$SES+
              suicide$Occupation+
              suicide$`Marital status`+
              suicide$`Method of attempt to suicide`+
              suicide$`Time of event`+
              suicide$`Time interval between attempt to suicide and bring to hospital`+
              suicide$`Cause of suicide`,
            data=train, family =binomial("logit"))
model

#### Lets predict with the model

test$admit = as.factor(test$`Outcome of suicide`)

test$Pred = predict(model, test[,c(2,3,4,5,6,7,8,9,10,11,12)], type = "response")

test$Pred = round(test$Pred,2)
View(test$Pred)



  