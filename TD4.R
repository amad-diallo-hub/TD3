df = read.csv(file = "C:/Users/adiallo2/Downloads/velov.csv",
              header = TRUE,
              sep = ";", 
              dec = "," )
summary(df)
View(df)
class(df$status)
class(df$CodePostal)

df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)
df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO" , "OK")
table(df$bornes)
#en réalité, c'est aussi peut-être car la station est fermée OU que des usagers ont déposé leur vélo pile au moment de l'extraction.

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations")

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 5)

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 6,
     col = "yellow")

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     breaks = 6,
     col = "red",
     xlab = "Capacity")

abline(h = 100, col = "blue", lty = 2)

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity")

lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 4)

hist(x = df$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08))

lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 2)

boxplot(x = df$capacity, 
        main = "Boxplot de \n la capacité des stations")

boxplot(x = df$capacity, 
        main = "Boxplot de \n la capacité des stations",
        horizontal = TRUE)

boxplot(x = df$capacity, 
        main = "Boxplot de \n la capacité des stations",
        horizontal = FALSE,
        outline = FALSE)

moy = mean(df$capacity, na.rm = TRUE)
points(moy, col = "red", pch = 15, cex = 2)

par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes
#7ème
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))
#C'est plus simple d'analyser les deux graphiques si la borne des ordonnées est la même.
# On remarque que la disponibilité des stations est plus homogènes sur le 8ème.

par(mfrow=c(1,1)) #fenêtre sur 1 ligne et 1 colonne
# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")

# Calculer les moyennes de chaque groupe
means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)
