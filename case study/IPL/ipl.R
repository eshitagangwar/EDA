library(readxl)
library(dplyr)
cric1=read_excel("C:/Users/Administrator/Desktop/r notes/R Programming/case study/Case study_2/Data Set for Case study/Ball_by_Ball.xlsx",sheet=1)
cric2=read_excel("C:/Users/Administrator/Desktop/r notes/R Programming/case study/Case study_2/Data Set for Case study/Match.xlsx",sheet=1)
cric3=read_excel("C:/Users/Administrator/Desktop/r notes/R Programming/case study/Case study_2/Data Set for Case study/Player.xlsx",sheet=1)
cric4=read_excel("C:/Users/Administrator/Desktop/r notes/R Programming/case study/Case study_2/Data Set for Case study/Player_Match.xlsx",sheet=1)
cric5=read_excel("C:/Users/Administrator/Desktop/r notes/R Programming/case study/Case study_2/Data Set for Case study/Season.xlsx",sheet=1)
cric6=read_excel("C:/Users/Administrator/Desktop/r notes/R Programming/case study/Case study_2/Data Set for Case study/Team.xlsx",sheet=1)

View(cric1)
View(cric2)
View(cric3)
View(cric4)
View(cric5)
View(cric6)

##1st question

data1=merge(cric1,cric3[,c(1,2)],by.x='Striker_Id',by.y = 'Player_Id',all.x = T)
View(data1)

d= data1%>% group_by(Player_Name)%>% 
  summarise(total_runs=sum(as.integer (Batsman_Scored) , na.rm = TRUE ))%>%
  arrange(-total_runs)%>%head(10) %>% View()

e= data1%>% group_by(Player_Name) %>% filter(Batsman_Scored==6)%>% 
  summarise(total_runs=n()) %>% arrange(-total_runs)%>%
  head(10)%>% View()

f=data1%>% group_by(Player_Name) %>% filter(Batsman_Scored==4)%>% 
  summarise(total_runs=n()) %>% arrange(-total_runs)%>%
  head(10)%>% View()

g=data1%>% group_by(Player_Name,Match_Id) %>%
  summarise(total_runs=sum(as.integer (Batsman_Scored) , na.rm = TRUE )) %>%
  filter(total_runs>99)%>%summarise(total_runs=n())%>% arrange(-total_runs)%>%
  head(10)%>% View()

#2nd question
data2=merge(cric6,cric2[,c(2:3,10,14)],by.x = "Team_Id",by.y = "Match_Winner_Id",all.x = T)
View(data2)

data3=merge(cric6,cric2[,c(2:3,7,14)],by.x = "Team_Id",by.y = "Team_Name_Id",all.x = T)
View(data3)

h=data2%>% group_by(Team_Name)%>% summarise(total_matches=sum(IS_Result)) %>% 
  arrange(-total_matches)%>%head(10)%>% View()

i=data3%>% group_by(Toss_Winner_Id,Match_Winner_Id,Team_Name)%>%
  summarise(total_sum=n())%>% group_by(Team_Name)%>% arrange(-total_sum)%>%
  head(10)%>% View()


##3rd question

data4=merge(cric3[,c("Player_Id","Player_Name")],cric5,by.x="Player_Id",by.y="Purple_Cap_Id")
View(data4)

j=data4%>% group_by(Season_Id,Player_Id)%>% summarise(orange_cap=print(Player_Name))%>% View()

k=data4%>% group_by(Season_Id,Player_Id)%>% summarise(purple_cap=print(Player_Name))%>% View()

##4th question
data5= merge(cric2[,c(5,12:14)],cric6,by.x="Match_Winner_Id",by.y="Team_Id")
View(data5)

l= data5%>%filter(Win_Type=='by runs')%>%group_by(Season_Id,Team_Name)%>% summarise(winn=max(as.integer(Won_By),na.rm = T))%>% top_n(n=1,wt=winn)%>%View()


#5th question

m=cric2%>% group_by(Season_Id)%>% summarise(c=n())%>%View()

n= cric1%>%group_by(Session_Id,Striker_Id,Match_Id)%>%
  summarise(cent=sum(as.integer(Batsman_Scored),na.rm = T))%>% 
  group_by(Session_Id) %>% summarise(total_cent=sum(cent>99))%>% View()

o= cric1%>%group_by(Session_Id,Striker_Id,Match_Id)%>%
  summarise(fift=sum(as.integer(Batsman_Scored),na.rm = T))%>% 
  group_by(Session_Id) %>% summarise(total_fift=sum(fift>49 & fift<100 ))%>% View()

p=cric1%>% group_by(Session_Id)%>% summarise(sixs=sum(as.integer(Batsman_Scored==6),na.rm = T))%>% View()

q=cric1%>% group_by(Session_Id)%>% summarise(fours=sum(as.integer(Batsman_Scored==4),na.rm = T))%>% View()

##6th question

data6=merge(cric6,cric2[,c(1:5,14)],by.x="Team_Id",by.y = "Match_Winner_Id",all.x = T)
View(data6)

r=data6%>%group_by(Season_Id)%>% filter(Match_Date==max(Match_Date))%>% 
  select(Season_Id,Match_Date,Team_Name)%>%
  arrange(Season_Id)%>% View()
