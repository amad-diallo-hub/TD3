df <- read.csv("C:/Users/DIALLO/Downloads/fao.csv", header = TRUE, sep = ";", dec = ",")
nom <- colnames(df)
print(nom)
class(df)
nombre_pays <- nrow(df)
View(df)
resume_de_données <- summary(df)
resume_de_données

rang = order(df$Population, decreasing = FALSE)
pays_moins_peuples = df[ rang  , c("Population", "Nom") ]
pays_moins_peuples = pays_moins_peuples[c(1:5), ]
View(pays_moins_peuples)

rang2 = order(df$Population, decreasing = TRUE)
pays_plus_peuples = df[ rang2  , c("Population", "Nom") ]
pays_plus_peuples = pays_plus_peuples[c(1:5), ]
View(pays_plus_peuples)

rang3 = order(df$Prod_viande, decreasing = TRUE)
pays_plus_viandes = df[ rang3  , c("Prod_viande", "Nom") ]
pays_plus_viandes = pays_plus_viandes[c(1:5), ]
View(pays_plus_viandes)

rang4 = order(df$Import_viande, decreasing = TRUE)
pays_import_plus_viandes = df[ rang4  , c("Import_viande", "Nom") ]
pays_import_plus_viandes = pays_import_plus_viandes[c(1:5), ]
View(pays_import_plus_viandes)

Pays_qui_mange_bien <- subset(df, Dispo_alim >= 2300)
nrow(Pays_qui_mange_bien)
View(Pays_qui_mange_bien)

Pays_qui_mange_bien_et_importe_beaucoup <- subset(df, Dispo_alim >= 3500 & Import_viande>=1000)
nrow(Pays_qui_mange_bien_et_importe_beaucoup)
View(Pays_qui_mange_bien_et_importe_beaucoup)

France_et_Belgique <- subset(df, Nom == "France" | Nom == "Belgique")
nrow(France_et_Belgique)
View(France_et_Belgique)

df$part_export <-round((df$Export_viande/df$Prod_viande)*100, 2)
View(df)

df$dispo_alim_pays <-round(df$Dispo_alim*df$Population, 2)
View(df)
df

write.table(x = df, file = "C:/Users/DIALLO/Downloads/ExportTp2.csv",
            sep = ";",row.names = FALSE)

Somme_dispo_mondiale <- sum(df$dispo_alim_pays, na.rm = TRUE)

Nombre_adulte <- round(Somme_dispo_mondiale/2300, 0)

plot(df$Prod_viande, df$Export_viande, xlab = "Variable X", ylab = "Variable Y", main = "Nuage de points")

Correlation <- cor(df$Prod_viande, df$Export_viande, method = "spearman" )
Correlation2 <- cor(x = df$Prod_viande,
    y = df$Export_viande)

matrice1 = cor(df[ , - 1], use = "complete.obs")
matrice1 = round(matrice1, 2)
View(matrice1)
corrplot(matrice1, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE )


















