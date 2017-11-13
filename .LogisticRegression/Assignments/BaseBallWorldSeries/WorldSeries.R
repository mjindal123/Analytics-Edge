baseball = read.csv("baseball.csv")
length(table(baseball$Year))
baseball = subset(baseball, Playoffs == 1)
table(baseball$Team)
table(table(baseball$Year))
PlayoffTable = table(baseball$Year)
PlayoffTable
names(PlayoffTable)
baseball$NumCompetitors=PlayoffTable[as.character(baseball$Year)]
summary(baseball)
table(baseball$NumCompetitors)
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
LogModel = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball, family=binomial)