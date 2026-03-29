df <- read.csv(file = "C:/Users/adiallo2/Downloads/NBA2014_2015.csv", sep = ",",
               header = TRUE, dec = ".")

nrow(df)     #Nombre de lignes
ncol(df)     #Nombre de colonnes
colnames(df)   #Noms des colonnes
srt(df)
View(df)   #Visionner le dataset
df$Period <- as.factor(df$PERIOD)    #Convertir les données
df$PTSTYPE <- as.factor(df$PTS_TYPE)   #Convertir les données
df$SHOOTER <- as.factor(df$SHOOTER)    #Convertir les données

length(df$PERIOD)   #Longueur de la colonne PERIOD
length(df$PTS_TYPE)    #Longueur de la colonne PTS_TYPE
length(df$SHOOTER)    #Longueur de la colonne SHOOTER
summary(df)
sd(df$SHOT_DIST)
sd(df$SHOT_CLOCK, na.rm = TRUE )
     
     #combien de tirs manqués/réussis
     table(df$SHOT_RESULT)
     #les quartiles
     quantile(df$SHOT_CLOCK, probs = seq(0, 1, 0.25), na.rm= TRUE)
     #les déciles
     quantile(df$CLOSE_DEF_DIST, probs = seq(0, 1, 0.1), na.rm= TRUE)
     #nombre de matches différents
     liste_game <- unique(df$GAME_ID)
length(df$liste_game)
#nombre de joueurs différents
df$SHOOTER <- as.factor(df$SHOOTER)
nlevels(df$SHOOTER)
       #conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
       df$SHOT_DIST_METRE <- df$SHOT_DIST * 0.30
       #nombre de points qu'a rapporté la tentative
       
       df$PTS_MARQUES <- ifelse(df$SHOT_RESULT == "made",df$PTS_TYPE, 0)
       #On supprime la variable GAME_RESULT car elle n'est pas utile
       df$GAME_RESULT <- NULL
       
       #création d'un objet sans la première colonne GAME_ID
       df2 <- df[  , -1 ]

              