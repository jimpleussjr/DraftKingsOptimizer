### Draft Kings Salary Optimizer

# DK Scoring
# Passing: 4 per Pass TD, .04 per Pass YD, 3 for 300 YDs, -1 per INT
# Rushing: 6 per Rush TD, .1 per Rush YD, 3 per 100 YDs
# Receiving: 6 per Rec TD, .1 per Rec YD, 3 per 100 YDs, 1 per REC
# Special: 6 per ST TD (individual and DST), -1 per FUM, 2 per 2PT
# Defense: 1 per Sack, 2 per INT, 2 per FUM, 6 per INT TD, 6 per FUM TD, 2 per Safety
# Def Points: 10 for 0; 7 for 1-6; 4 for 7-13; 1 for 14-20; 0 for 21-27; -1 for 28-34; -4 for 35+

# Lineup Requirements: 1 QB, 2 RB, 3 WR, 1 TE, 1 Flex (RB,WR,TE), 1 DST


library(tidyverse)
library(stringr)
#install.packages("rvest")
library(rvest)
library(lpSolve)
library(lpSolveAPI)


espn=read.csv("ESPNWk3Ranks.csv", header=TRUE)
dk=read.csv("DKSalariesWk3.csv", header=TRUE)

##Clean draftkings data
dk=dk %>% select (Position, Name, Salary, TeamAbbrev) %>% mutate(Name=trimws(Name))
dk=dk %>% mutate(Name=ifelse(dk$Name=="Michael Thomas" & dk$TeamAbbrev=="LAR","Michael Thomas LAR",Name))



### Try to get the ranking to populate... not working.

#url = "http://www.espn.com/fantasy/football/story/_/page/18ranksWeeklyQBPPR/fantasy-football-weekly-quarterback-rankings-2018"

espnurl="http://games.espn.com/ffl/tools/projections"

url=NA
for (i in 1:5) {
  url[i]=paste0("http://games.espn.com/ffl/tools/projections?startIndex=", i*40)
}

espnFFPts = url[1] %>%
  read_html() %>%
  html_nodes(".playertableStat") %>%
  html_text() %>%
  matrix(ncol = 11, byrow = TRUE)

espnPlayerPts = url[1] %>%
  read_html() %>%
  html_nodes(".playertablePlayerName") %>%
  html_text() %>%
  as.data.frame()

for (i in 2:length(url)) {
  espnFFTemp = url[i] %>%
    read_html() %>%
    html_nodes(".playertableStat") %>%
    html_text() %>%
    matrix(ncol = 11, byrow = TRUE)
  espnFFTemp=espnFFTemp[-1,]
  espnFFPts = rbind(espnFFPts,espnFFTemp)
  
  espnPlayerTemp = url[i] %>%
    read_html() %>%
    html_nodes(".playertablePlayerName") %>%
    html_text() %>%
    as.data.frame()
  espnPlayerPts=rbind(espnPlayerPts,espnPlayerTemp)
  
}
espnFFPts=espnFFPts[-1,]
#espnPlayerPts$Name=espnPlayerPts[-1,]


#readurl=espnurl%>%read_html()
  
#tbls = html_nodes(readurl,"table")

#stuff = espnurl %>% read_html() %>% html_nodes("") %>% html_text()

  # espnFF = espnurl %>%
  # read_html() %>%
  # html_nodes(".playertable") %>%
  # html_text()
  # #matrix(ncol = 8, byrow = TRUE)

  espnFFRank = espnurl %>%
  read_html() %>%
  html_nodes(".playertableStat") %>%
  html_text() %>%
  matrix(ncol = 11, byrow=TRUE) 
espnFFRank=espnFFRank[-1,]
  
  espnPlayersRank = espnurl %>%
    read_html() %>%
    html_nodes(".playertablePlayerName") %>%
    html_text() %>%
    as.data.frame()
    #matrix(ncol = 11, byrow = TRUE)

  #Combine all pages of player stats and names together
  espnPlayersRank=rbind(espnPlayersRank,espnPlayerPts)
  names(espnPlayersRank)="Name"
  espnFFRank=rbind(espnFFRank, espnFFPts)
  espnFFRank=espnFFRank[,-1]
  
  #espnFFRank=as.numeric(as.character(espnFFRank))
  str(espnFFRank)
  ## Add position column
  espnPlayersRank$Position=espnPlayersRank$Name %>% str_extract("QB|RB|WR|D/ST|TE")
  espnPlayersRank= espnPlayersRank %>% mutate(Position=ifelse(Position=="D/ST","DST", Position))
  espnPlayersRank$Position= espnPlayersRank$Position %>% replace_na("K")
  #espnPlayersRank %>% select(Name) %>% mutate(Position=word(espnPlayersRank$Name,-1))
  #gsub(".*\\s", "","New England")
  
  #extract everything from Name before the first comma
  espnPlayersRank$Name=gsub("^(.*?),.*","\\1",espnPlayersRank$Name)
  #delete D/ST from Defense names
  espnPlayersRank$Name=gsub("D/ST","", espnPlayersRank$Name)
  espnPlayersRank$Name= espnPlayersRank$Name %>% str_trim()
  
# fix name for Melvin Gordon and Allen Robinson
  espnPlayersRank$Name=ifelse(espnPlayersRank$Name=="Melvin Gordon","Melvin Gordon III", ifelse(espnPlayersRank$Name=="Allen Robinson", "Allen Robinson II",ifelse(espnPlayersRank$Name=="Paul Richardson", "Paul Richardson Jr.",espnPlayersRank$Name)))
  espnPlayersRank$Name=ifelse(espnPlayersRank$Name=="DJ Moore","D.J. Moore", ifelse(espnPlayersRank$Name=="Willie Snead", "Willie Snead IV",espnPlayersRank$Name))
  
  #espnPlayersRank$Name %>% str_extract('^.[^D/ST]$')
#espnPlayersRank=espnPlayersRank %>% select(Name, Position)
espnFFComplete=cbind(espnPlayersRank,espnFFRank)
names(espnFFComplete)=c("Name", "Position","PassYds","PassTD","Int","Rushes","RushYds","RushTDs","Recept","RecYds","RecTds","Pts")


#convert factors to numeric
indx = sapply(espnFFComplete, is.factor)
espnFFComplete[indx]=lapply(espnFFComplete[indx], function(x) as.numeric(as.character(x)))

str(espnFFComplete)  
str(playerComplete)
#playerComplete %>% filter(is.na(Salary)) 
### Join with Draft Kings
playerComplete=espnFFComplete %>% filter(Position!="K") %>% left_join(dk, by=c("Name", "Position"))
#options(digits=10)
#playerComplete$Pts=as.numeric(as.character(playerComplete$Pts))
## Use only complete cases
playerComplete=playerComplete %>% filter(complete.cases(.))
#Remove the second Michael Thomas Receiver
#playerComplete=playerComplete[-15,]
playerComplete=playerComplete %>% mutate(Ratio=Pts/(Salary/1000))


##Develop LP Frame
LPdataframe=playerComplete %>% select(Pts, Salary,Position) %>% filter(!(playerComplete$Position!="DST" & playerComplete$Pts<=13))
ColNames = playerComplete %>% select(Name) %>% filter(!(playerComplete$Position!="DST" & playerComplete$Pts<=13))


#LPdataframe$Pts=as.double(LPdataframe$Pts)
LPdataframe=LPdataframe %>% mutate("QB"=ifelse(Position=="QB",1,0),"RB"=ifelse(Position=="RB",1,0), "WR"=ifelse(Position=="WR",1,0), "TE"=ifelse(Position=="TE",1,0), "Flex"=ifelse(Position=="RB"|Position=="WR"|Position=="TE",1,0), "DST"=ifelse(Position=="DST",1,0))
LPdataframe=LPdataframe %>% select(QB,RB,WR,TE,Flex,DST,Salary,Pts)
#LPdataframe=LPdataframe[c(-127,-17),]
str(LPdataframe)

#Make LP with no constraints but a decision variable for every player
playerLP=make.lp(nrow=0, ncol=length(LPdataframe$Pts))
#Set objective function of the number of points scored
set.objfn(playerLP,obj=LPdataframe$Pts)
lp.control(playerLP,sense="maximize")
set.type(playerLP, columns=1:length(LPdataframe$Pts),type="binary")

#add constraint for having 1 QB
add.constraint(playerLP, LPdataframe$QB, "=", 1)
#add constraint for having 2 RB
add.constraint(playerLP, LPdataframe$RB, ">=", 2)
#add constraint for having 2 WR
add.constraint(playerLP, LPdataframe$WR, ">=", 3)
#add constraint for having 1 TE
add.constraint(playerLP, LPdataframe$TE, ">=", 1)
#add constraint for having 6 total flex
add.constraint(playerLP, LPdataframe$Flex, "=", 7)
#add constraint for DST
add.constraint(playerLP, LPdataframe$DST, "=", 1)
#add constraint for salary
add.constraint(playerLP, LPdataframe$Salary, "<=", 50000)

#set.bounds(playerLP, lower=rep(0,length(LPdataframe$Pts)))
#set.bounds(playerLP, upper=rep(1,length(LPdataframe$Pts)))
RowNames =c("QB","RB","WR","TE", "Flex","DST","Salary")
dimnames(playerLP)=list(RowNames,ColNames[,1])
print(playerLP)

solve(playerLP)
get.objective(playerLP)
get.variables(playerLP)
write.lp(playerLP,'model.lp',type='lp')

ColNames[get.variables(playerLP)==1,1]
#results=playerComplete %>% mutate(chosen= get.variables(playerLP))
#results %>% select(Name, chosen) %>% filter(chosen>0)

## remove the linear model 
rm(playerLP)


##Looking at ESPN Rankings .csv instead.

###Alter names
espnclean=espn
# Gets rid of all text after the comma
espnclean$Name=gsub("\\,.*","",espnclean$Name)
#Gets rid of all text before the first space
espnclean =espnclean %>% mutate(Name=str_extract(Name,"(?<=\\s)(.*)")) 
espnclean$Karabell[espnclean$Karabell=="NR"]=NA
espnclean$Berry[espnclean$Berry=="NR"]=NA
espnclean$Cockcroft[espnclean$Cockcroft=="NR"]=NA
espnclean$Bell[espnclean$Bell=="NR"]=NA
espnclean$Yates[espnclean$Yates=="NR"]=NA
espnclean$Clay[espnclean$Clay=="NR"]=NA
#espnclean %>% select(Karabell, Name) %>% filter(Name=="Patrick Mahomes") %>% mutate(Karabell=ifelse(Karabell=="NR",NA,Karabell)) %>%
  # mutate(Cockcroft=ifelse(Cockcroft=="NR",NA,Cockcroft)) %>%
  # mutate(Yates=ifelse(Yates=="NR",NA,Yates)) %>%
  # mutate(Bell=ifelse(Bell=="NR",NA,Bell)) %>%
  # mutate(Clay=ifelse(Clay=="NR",NA,Clay))

# fix name for Melvin Gordon and Allen Robinson
espnclean$Name=ifelse(espnclean$Name=="Melvin Gordon","Melvin Gordon III", ifelse(espnclean$Name=="Allen Robinson", "Allen Robinson II",ifelse(espnclean$Name=="Paul Richardson", "Paul Richardson Jr.",espnclean$Name)))
espnclean$Name=ifelse(espnclean$Name=="DJ Moore","D.J. Moore", ifelse(espnclean$Name=="Willie Snead", "Willie Snead IV",espnclean$Name))

#Get rid of city names for DST
def=espnclean %>% filter(Position=="DST")
off=espnclean %>% filter(Position!="DST")
def=def %>% separate(Name,c("City1","Name","Team"))
def=def%>% mutate(Name=ifelse(is.na(Team),Name,Team )) %>% select(-Team,-City1)
espnclean=rbind(off,def)

#Add sd column
espnclean=espnclean %>% rowwise() %>%  mutate(ranksd=sd(c(Yates,Karabell, Cockcroft,Bell, Clay),na.rm=TRUE))

# join ESPN ranks with DK salaries
players=left_join(espnclean, dk, by=c("Name","Position"))

#linear model for salary based on avg rank and position
salarymodel=lm(Salary~Position + Avg-1, data=players)
players$Residuals=salarymodel$residuals

options(tibble.print_min=20)
players %>% select(Name, Avg, Salary, Residuals,ranksd, Position) %>% filter(Position=="QB")
players %>% select(Name, Avg, Salary, Residuals,ranksd, Position) %>% filter(Position=="RB")
players %>% select(Name, Avg, Salary, Residuals,ranksd, Position) %>% filter(Position=="WR")
players %>% select(Name, Avg, Salary, Residuals,ranksd, Position) %>% filter(Position=="TE")
players %>% select(Name, Avg, Salary, Residuals,ranksd, Position) %>% filter(Position=="DST")



sort(salarymodel$residuals, decreasing=TRUE)

plot(salarymodel)


plot(rstandard(salarymodel))

#espnclean=espnclean %>% select(Yates,Karabell, Cockcroft,Bell, Clay) %>% rowwise() %>%  mutate(ranksd=sd(.,na.rm=TRUE))

                          