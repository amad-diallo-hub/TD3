install.packages("readxl")
library(readxl)
pokemon <- read_excel(path = "C:/Users/DIALLO/Downloads/pokemon.xlsx",sheet = "pokemon")
dim(pokemon)
summary(pokemon)
pokemon$is_legendary <-as.factor(pokemon$is_legendary)
pokemon$generation <-as.factor(pokemon$generation)
pokemon$type <-as.factor(pokemon$type)
summary(pokemon)

pokemon$attack_group <- ifelse(test = pokemon$attack >= 70, yes = "attack+", no = "attack-")
pokemon$attack_group <- as.factor(pokemon$attack_group)
summary(pokemon)

pokemon$water_fire = ifelse(pokemon$type %in% c("water","fire"), "yes","no")
pokemon$water_fire <-as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

requete = subset(pokemon, is.na(weight_kg))
View(requete)

requete1 = subset(pokemon, !is.na(weight_kg))
View(requete1)

pokemon$weight_kgNa <-ifelse(test = pokemon$weight_kg == pokemon$weight_kg , yes = pokemon$weight_kg, no = mean(weight_kg)) 
pokemon$height_mNA <- "Manga"

pokemon$weight_group = cut(pokemon$weight_kg,
                           breaks = 3,
                           labels = c("léger","moyen","lourd"))
View(pokemon)

pokemon$height_m_group = cut(pokemon$height_m,
                           breaks = 4,
                           labels = c("]0,1]","]1,2]","]2,3]","]3,max]"))
View(pokemon)

Minimum <- min(pokemon$defense, na.rm = TRUE)
Q1 <- quantile(pokemon$defense, probs = 0.25, na.rm = TRUE)
Q2 <- quantile(pokemon$defense, probs = 0.5, na.rm = TRUE)
Q3 <- quantile(pokemon$defense, probs = 0.75, na.rm = TRUE)
Maximum <- max(pokemon$defense, na.rm = TRUE)
pokemon$defense_group = cut(pokemon$defense,
                            breaks = c(Minimum,Q1,Q2,Q3,Maximum),
                            labels = c("[min,Q1]", "(Q1,Q2]", "(Q2,Q3]", "(Q3,max]"),
                                  include.lowest = TRUE)
View(pokemon)

Moyenne_attack_type <- aggregate( attack_type= y ~ pokomon$attack + pokomon$type, data = pokomon, FUN = mean(attack_type))

Moyenne_attack_type <- aggregate(x = attack ~ type, 
          data = pokemon,
          FUN = function(x) mean(x))
View(Moyenne_attack_type)

Mediane_attack_generation_type <- aggregate(x = attack ~ generation + type, 
                                 data = pokemon,
                                 FUN = function(x) median(x))
View(Mediane_attack_generation_type)

Effectif_type <- aggregate(x = attack ~ type, 
                                 data = pokemon,
                                 FUN = function(x) mean(x))
View(Moyenne_attack_type)

Moyenne_Mediane_speed_generation_type <- aggregate(x = speed ~ generation + type, 
                                            data = pokemon,
                                            FUN = function(x) c(med = median(x),moy = mean(x), eff = length(x)))
View(Moyenne_Mediane_speed_generation_type)















