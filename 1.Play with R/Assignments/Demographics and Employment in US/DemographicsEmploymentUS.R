CPS = read.csv("CPSData.csv")
sort(table(CPS$State))
prop.table(table(CPS$Citizenship))
table(CPS$Race, CPS$Hispanic)
is.na(CPS$Married) 
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
table(CPS$State, is.na(CPS$MetroAreaCode))

prop.table(table(CPS$Region, is.na(CPS$MetroAreaCode)))
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
MetroAreaMap=read.csv("MetroAreaCodes.csv")
CountryMap=read.csv("CountryCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))# Ignores 
#missing values so correct proportion

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
