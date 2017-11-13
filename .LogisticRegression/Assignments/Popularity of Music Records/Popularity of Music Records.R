songs = read.csv("songs.csv")
table(songs$year)
str(songs)
MichaelJackson = subset(songs, artistname == "Michael Jackson")
abc = subset(MichaelJackson,Top10==1)
abc
table(songs$timesignature)
which.max(songs$tempo)
songs$songtitle[6206]
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

summary(SongsLog1)

cor(SongsTrain$loudness,SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)


#We just subtracted the variable loudness. We couldn't do this with the variables "songtitle" 
#and "artistname", because they are not numeric variables, and we might get different values 
#in the test set that the training set has never seen. But this approach (subtracting the 
#variable from the model formula) will always work when you want to remove numeric variables.

summary(SongsLog2)


SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

testPredict = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, testPredict >= 0.45)


# Let's check if there's any incremental benefit in using Model 3 
# instead of a baseline model. Given the difficulty of guessing which 
# song is going to be a hit, an easier model would be to pick the most 
# frequent outcome (a song is not a Top 10 hit) for all songs. What would
# the accuracy of the baseline model be on the test set?

table(SongsTest$Top10)
314/373
table(SongsTest$Top10, testPredict >= 0.45)
19/59
309/314
