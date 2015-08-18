# Loading data set

musicpopularity = read.csv("songs.csv")
str(musicpopularity)

# number of songs by year
table(musicpopularity$year)

# michael jackson subset
michaeljackson = subset(musicpopularity, musicpopularity$artistname == "Michael Jackson")
nrow(michaeljackson)

michaeljackson[c("songtitle","Top10")]

table(musicpopularity$timesignature)

# song with max tempo
which.max(musicpopularity$tempo)
musicpopularity$songtitle[6206]

# subset to train and test
songsTrain = subset(musicpopularity, year <= 2009)
songsTest = subset(musicpopularity, year == 2010)

str(songsTrain)

# Removing variables which will not be used in the model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = songsTrain[ , !(names(songsTrain) %in% nonvars) ]
SongsTest = songsTest[ , !(names(songsTest) %in% nonvars) ]

# GLM with all data
mod1 = glm (Top10 ~ . , data = SongsTrain, family = binomial)
summary(mod1)

# Collinearity between energy and loudness
cor(SongsTrain)

# GLM without loudness
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# GLM without energy
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

SongsPredict = predict(SongsLog3, newdata = SongsTest, type = "response")
SongsPredict

table(SongsTest$Top10, SongsPredict >= 0.45)

table(SongsTest$Top10)
314/(314+59)

