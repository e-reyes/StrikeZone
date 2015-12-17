## Erick S. Reyes
##StrikeZone Project

#Package
library(class)
library(gmodels)
library(ggvis)

### Data Frame for strikezone test points ###

#Initiate variables
x_pts= seq(-2,2,by=.01)
y_pts= seq(0,4.5,by=.01)
TestPts=data.frame(PlateLocHeight=numeric(),PlateLocSide=numeric())
#Build dataFrame

for(i in 1:length(x_pts)){
  new=data.frame(PlateLocSide=x_pts,PlateLocHeight=y_pts[i])
  TestPts=rbind(TestPts,new)
}

#Import .csv file with pitch location data and pitchcall

PitchCall_db_2015=read.csv("C:/Users/Erick S. Reyes/Documents/R/Strikezone/umpire_query.csv")
PitchCall_db_2014=read.csv("C:/Users/Erick S. Reyes/Documents/R/Strikezone/umpire_query2014.csv")
PitchCall_db_2013=read.csv("C:/Users/Erick S. Reyes/Documents/R/Strikezone/umpire_query2013.csv")

##To combine data from 2015,2014 and 2013###

#PitchCall_db= rbind(PitchCall_db_2015,PitchCall_db_2014,PitchCall_db_2013)


#Focus on area of interest, Eliminate pitches way outside the strikezone

PitchCall_db_2015=subset(PitchCall_db_2015,PitchCall != "Ball In Dirt" & PlateLocHeight>0 & PlateLocHeight<5 & PlateLocSide>-3 & PlateLocSide<3 )

#Build a training data set(2/3 of total data)and a test data set (1/3 of total data)

set.seed(1234)
ind = sample(2, nrow(PitchCall_db_2015), replace=TRUE, prob=c(0.67, 0.33))
ballstrike.training= PitchCall_db_2015[ind==1, 2: 3]
ballstrike.test= PitchCall_db_2015[ind==2, 2:3]

#Vector that stores pitchcall for both test and training data
ballstrike.train_Pitchcall= PitchCall_db_2015[ind==1,1]
ballstrike.test_Pitchcall= PitchCall_db_2015[ind==2,1]

## KNN Algorithm using StrikeZone test points

StrikeZone_prop=knn(ballstrike.training,TestPts,ballstrike.train_Pitchcall,k=467,prob = TRUE)

#Create two new columns in TestPts dataframe, [Pred_Pitchcall] & [Prob]

TestPts[["Pred_Pitchcall"]]= StrikeZone_prop


x= attributes(StrikeZone_prop)[3]
y=unlist(x)
z= matrix(y,nrow=160801)
TestPts[["Prob"]]=z

##Create a csv with Strikze Zone Probabilities

write.csv(TestPts, file = "Strike Zone_2015.csv")


## PLOT Strike Zone ###

#Strike Zone for 2015 
Strike.Zone=read.csv("C:/Users/Erick S. Reyes/Documents/R/Strikezone/Strike Zone_2015.csv")



index=Strike.Zone$Pred_Pitchcall=="Ball"
Strike.Zone$Prob[index]=abs((Strike.Zone$Prob[index]-1))


##Set Color for different Strike Probabilities##

P=c(.167,.333,.5,.667,.834,1)
color_palette= colorRampPalette(c("slateblue1","lightpink","red"))
color=color_palette(6)
index2=Strike.Zone$Prob<=P[1]
Strike.Zone$Color[index2]=color[1]
for(i in 2:6){
  index3=Strike.Zone$Prob >= P[i-1] & Strike.Zone$Prob<=P[i]
  Strike.Zone$Color[index3]=color[i]
}



##build data frame to plot background and "Rule Book" Strike Zone##
rect= data.frame(x1= -1, x2=1, y1=1.5, y2=3.5,color= "white")
background= data.frame(x1=-2,x2=2,y1=1,y2=4,color= "blue")


#Plot
StrikeZone= ggvis(data= background) %>% layer_rects( x = ~background$x1, x2 = ~background$x2, y = ~background$y1, y2= ~ background$y2, fill := ~background$color) %>% layer_points(data=Strike.Zone, x= ~Strike.Zone$PlateLocSide, y= ~Strike.Zone$PlateLocHeight, fill:= ~Strike.Zone$Color,size=300, opacity := ~Strike.Zone$Prob) %>% layer_rects(data=rect, x= ~rect$x1, x2= ~rect$x2, y= ~rect$y1, y2= ~rect$y2, stroke:= "white" ,fillOpacity := 0.1) %>% scale_numeric("x", domain= c(-2, 2),clamp=TRUE) %>% scale_numeric("y", domain =c(1,4),clamp= TRUE)%>% add_axis("x", title = "Plate Location Side") %>% add_axis("y", title = "Plate Locatation Height")
StrikeZone

########## JOE WEST STRIKE ZONE 2013-2015 #################

#Strike Zone for Joe West (Includes 2015,2014,2013 data)

PitchCall_db_2015=read.csv("C:/Users/Erick S. Reyes/Documents/R/Strikezone/umpire_query.csv")
PitchCall_db_2014=read.csv("C:/Users/Erick S. Reyes/Documents/R/Strikezone/umpire_query2014.csv")
PitchCall_db_2013=read.csv("C:/Users/Erick S. Reyes/Documents/R/Strikezone/umpire_query2013.csv")

##To combine data from 2015,2014 and 2013###

PitchCall_db= rbind(PitchCall_db_2015,PitchCall_db_2014,PitchCall_db_2013)

#Focus on area of interest, Eliminate pitches way outside the strikezone

PitchCall_db_JoeWest=subset(PitchCall_db,Umpire=="Joe West" & PitchCall != "Ball In Dirt" & PlateLocHeight>0 & PlateLocHeight<5 & PlateLocSide>-3 & PlateLocSide<3 )

#Build a training data set(2/3 of total data)and a test data set (1/3 of total data)

set.seed(1234)
ind = sample(2, nrow(PitchCall_db_JoeWest), replace=TRUE, prob=c(0.67, 0.33))
ballstrike.training= PitchCall_db_JoeWest[ind==1, 2: 3]
ballstrike.test= PitchCall_db_JoeWest[ind==2, 2:3]

#Vector that stores pitchcall for both test and training data
ballstrike.train_Pitchcall= PitchCall_db_JoeWest[ind==1,1]
ballstrike.test_Pitchcall= PitchCall_db_JoeWest[ind==2,1]

## KNN Algorithm using StrikeZone test points

StrikeZone_prop_JoeWest=knn(ballstrike.training,TestPts,ballstrike.train_Pitchcall,k=97,prob = TRUE)

#Create two new columns in TestPts dataframe, [Pred_Pitchcall] & [Prob]

TestPts[["Pred_Pitchcall"]]= StrikeZone_prop_JoeWest


x= attributes(StrikeZone_prop_JoeWest)[3]
y=unlist(x)
z= matrix(y,nrow=160801)
TestPts[["Prob"]]=z

##Create a csv with Strikze Zone Probabilities

write.csv(TestPts, file = "Strike Zone_JoeWest.csv")





##PLOT JOE WEST Strike Zone ##
Strike.Zone_JoeWest=read.csv("C:/Users/Erick S. Reyes/Documents/R/Strikezone/Strike Zone_JoeWest.csv")
index=Strike.Zone_JoeWest$Pred_Pitchcall=="Ball"
Strike.Zone_JoeWest$Prob[index]=abs((Strike.Zone_JoeWest$Prob[index]-1))

P=c(.167,.333,.5,.667,.834,1)
color_palette= colorRampPalette(c("slateblue1","lightpink","red"))
color=color_palette(6)
index2=Strike.Zone_JoeWest$Prob<=P[1]
Strike.Zone_JoeWest$Color[index2]=color[1]
for(i in 2:6){
  index3=Strike.Zone_JoeWest$Prob >= P[i-1] & Strike.Zone_JoeWest$Prob<=P[i]
  Strike.Zone_JoeWest$Color[index3]=color[i]
}

##build data frame to plot background and "Rule Book" Strike Zone##
rect= data.frame(x1= -1, x2=1, y1=1.5, y2=3.5,color= "white")
background= data.frame(x1=-2,x2=2,y1=1,y2=4,color= "blue")

#Plot Joe West Strike Zone

StrikeZone_JoeWest= ggvis(data= background) %>% layer_rects( x = ~background$x1, x2 = ~background$x2, y = ~background$y1, y2= ~ background$y2, fill := ~background$color) %>% layer_points(data=Strike.Zone_JoeWest, x= ~Strike.Zone_JoeWest$PlateLocSide, y= ~Strike.Zone_JoeWest$PlateLocHeight, fill:= ~Strike.Zone_JoeWest$Color,size=300, opacity := ~Strike.Zone_JoeWest$Prob) %>% layer_rects(data=rect, x= ~rect$x1, x2= ~rect$x2, y= ~rect$y1, y2= ~rect$y2, stroke:= "white" ,fillOpacity := 0.1) %>% scale_numeric("x", domain= c(-2, 2),clamp=TRUE) %>% scale_numeric("y", domain =c(1,4),clamp= TRUE) %>% add_axis("x", title = "Plate Location Side") %>%
  add_axis("y", title = "Plate Locatation Height")

StrikeZone_JoeWest
############################### RUN TO TEST KNN ALGORITHM #########################

##Prepare for KNN algorithm test


## KNN algorithm 

StrikeZone_prop=knn(ballstrike.training,ballstrike.test,ballstrike.train_Pitchcall,k=125,prob = TRUE)
CrossTable(x=ballstrike.test_Pitchcall, y = StrikeZone_prop, prop.chisq=FALSE)

#Remove dataframes

rm(PitchCall_db)
rm(ballstrike.training)
rm(ballstrike.test_Pitchcall)
rm(ballstrike.train_Pitchcall)
rm(ind)
rm(StrikeZone_prop)
rm(x_pts,y_pts,i)
