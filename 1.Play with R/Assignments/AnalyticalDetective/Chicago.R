mvt = read.csv("mvtChicago.csv")
str(mvt)
max(mvt$ID)
min(mvt$Beat)
table(mvt$Arrest)
table(mvt$LocationDescription)
mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Date = DateConvert
mvt$Weekday = weekdays(DateConvert)
table(mvt$Month)
table(mvt$Weekday)
table(mvt$Arrest,mvt$Month)
sort(table(mvt$LocationDescription))
Top5 = subset(mvt, LocationDescription=="STREET" | 
LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | 
LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | 
LocationDescription=="DRIVEWAY - RESIDENTIAL")
str(Top5) #or 
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)

#R will remember the other categories of the LocationDescription variable 
#from the original dataset, so running table(Top5$LocationDescription) will 
#have a lot of unnecessary output. To make our tables a bit nicer to read, 
#we can refresh this factor variable. In your R console, type:
table(Top5$LocationDescription)
  Top5$LocationDescription = factor(Top5$LocationDescription)
  table(Top5$LocationDescription, Top5$Arrest)
  prop.table(table(Top5$LocationDescription, Top5$Arrest),1)
#If you run the str or table function on Top5 now, you should see 
  #that LocationDescription now only has 5 values, as we expect.

#Use the Top5 data frame to answer the remaining questions.

#One of the locations has a much higher arrest rate than the other locations. 
#Which is it

  
  table(Top5$LocationDescription, Top5$Weekday)
