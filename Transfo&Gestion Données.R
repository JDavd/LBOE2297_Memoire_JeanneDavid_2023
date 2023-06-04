#### Isoler noms sp plantes et poll ####
SpP <- read.csv("Data_brouillons/spplantes.csv", header = T, sep = ";", dec = ",")
SpP = SpP %>%
  mutate(nom.scientifique = as.factor(nom.scientifique))
SpP
summary(SpP)
sp <- levels(SpP$nom.scientifique)
sp
write.csv2(sp, file = "Data_brouillons/sp.csv")

SpPol <- read.csv("Data_brouillons/POLL_2022-06-20_2022-08-01.csv", header = T, sep = ";", dec = ",")
SpPol = SpPol %>%
  mutate(nom.scientifique = as.factor(nom.scientifique),
         Site = as.factor(Site)) %>% 
  select(nom.scientifique, Site)
SpPol
summary(SpPol)
SpPol <- as.data.frame(SpPol)

de<- apply(SpPol[,1:2],2,unique)  
spP <- levels(c(SpPol$Site, SpPol$Site))
spP
write.csv2(de, file = "Data_brouillons/spPol.csv")


lev <- levels(Interactions$Sp_Pollinisateurs)
write.csv2(lev, file = "Data_brouillons/spPol2.csv")

levels(Inv_Classes_join$Sp_poll)

# interactions zero
zero_Q <- Interactions %>% 
  replace(is.na(.), 0) %>% 
  group_by(Site_gestion_date_Quadrat) %>% 
  summarise(data = sum(N_Interactions)) %>% 
  filter(data == 0)
write.csv2(zero_Q, file = "Output/zero.csv")
zero_site <- Interactions_Gestion %>% 
  replace(is.na(.), 0) %>% 
  group_by(Site_gestion_date) %>% 
  summarise(data = sum(N_Interactions)) %>% 
  filter(data == 0)
write.csv2(zero_site, file = "Output/zero.csv")


### Transfo pour extraire plantes pour interactions

Donnees_pres <- read.csv("Data/inventaires/Donnees_pres.csv", header = T, sep = ";", dec = ",")

Donnees_pres = Donnees_pres %>% 
  mutate(Site_gestion_date_Quadrat = as.factor(Site_gestion_date_Quadrat))

Plantes <- Donnees_pres[,c(1,26:83)]
Plantes$S <- specnumber(Plantes[2:59])
Plantes$Ab <- rowSums(Plantes[2:59])

Plantes

Plantes %>% ggplot(aes (x = S, y = Site_gestion_date_Quadrat)) + 
  geom_point() +
  labs(title = "titre" , x = "x", y = "y") +
  theme_bw()

sum <- colSums(Plantes[2:59]) == 0
sum(colSums(Plantes[2:59]) == 0)
names(sum[sum == TRUE])


Plantes = Plantes %>%
  select(-c(Anacamptis_pyramidalis,Convolvulus_sepium,Erigeron_canadensis,Helianthus_annuus,Helminthotheca_echioides,
            Rubus_spec.,Tripleurospermum_maritimum_._Matricaria_chamomilla,Vicia_cracca))
sum(colSums(Plantes[2:51]) == 0)

Plantes_pl <- Plantes %>%
  pivot_longer(cols = Achillea_millefolium:Graminees,
               names_to = "Espèces",
               values_to = "Qtté")
Plantes_pl <- Plantes %>%
  pivot_longer(cols = Achillea_millefolium:Graminees,
               names_to = "Espèces",
               values_to = "Qtté") %>% 
  filter(Qtté > 0)

write.csv2(Plantes_pl, file = "Data_brouillons/Plantes_pl.csv")



#### Analyses et tri données

##### Interactions vers inventaires poll + vérif données

Interactions <- read.csv("Data/interactions.csv", header = T, sep = ";", dec = ",")

#Interactions <- read.csv("Data_brouillons/interactions_test1.csv", header = T, sep = ";", dec = ",")

N_Interactions <-  Interactions %>% 
  mutate(Site_gestion_date_Quadrat = as.factor(Site_gestion_date_Quadrat)) %>%  
  na.omit() %>% #enlever les plantes sans interactions
  select(Site_gestion_date_Quadrat,individu,Sp_Pollinisateurs,N_Interactions)

nrow(N_Interactions)
sum(N_Interactions$N_Interactions)

Interactions = Interactions %>% 
  mutate(Site_gestion_date_Quadrat = as.factor(Site_gestion_date_Quadrat)) %>% 
  na.omit() %>% #enlever les plantes sans interactions
  select(Site_gestion_date_Quadrat,individu,Sp_Pollinisateurs)

nrow(Interactions)

Interactions = Interactions %>% 
  select(Site_gestion_date_Quadrat,individu,Sp_Pollinisateurs)

Interactions <- Interactions %>%
  distinct(individu, .keep_all = TRUE)

nrow(Interactions)
###vérification
sum(Interactions$individu)
1654*(1654+1)/2
###


### Poll isolés => pivot et n individus ###
Interactions = Interactions %>% 
  select(Site_gestion_date_Quadrat,Sp_Pollinisateurs) %>% 
  group_by(Site_gestion_date_Quadrat,Sp_Pollinisateurs) %>% 
  summarize(n = n())

Interactions_pw <- Interactions %>%
  pivot_wider(names_from = Sp_Pollinisateurs,
              values_from = n,
              names_sort=TRUE) %>% 
  arrange(Site_gestion_date_Quadrat)


write.csv2(Interactions_pw, file = "Data_brouillons/Poll_pw2.csv")


### Fusionner les quadrats
Donnees_pres <- read.csv("Data/Donnees_pres.csv", header = T, sep = ";", dec = ",")
# Renommer les colonnes et mise en facteurs des variables 

colnames(Donnees_pres)[1]<- c("Site_gestion_date_Quadrat")
colnames(Donnees_pres)[12:13]<- c("Gestion_2", "Gestion_3")
colnames(Donnees_pres)[33]<- c("Temperature")

Donnees_pres = Donnees_pres %>% 
  mutate(Site_gestion_date_Quadrat = as.factor(Site_gestion_date_Quadrat),
         Site_gestion_date = as.factor(Site_gestion_date),
         Site = as.factor(Site),
         Gestion = as.factor(Gestion), 
         Gestion_2 = as.factor(Gestion_2),
         Gestion_3 = as.factor(Gestion_3),
         type_gestion = as.factor(type_gestion),
         Area = as.factor(Area),
         Quartier = as.factor(Quartier),
         Jours = as.factor(Jours),
         Gestion_moment = as.factor(Gestion_moment),
         Gestion_moment_5 = as.factor(Gestion_moment_5),
         Activite = as.factor(Activite),
         Periode = as.factor(Periode),
         Temperature = as.factor(Temperature),
         Meteo = as.factor(Meteo))
#colnames(Donnees_pres)

Donnees_pres$Date <- dmy(Donnees_pres$Date)

# Sélection des colonnes d'intérêts
Inventaire <- Donnees_pres %>% 
  select(Site_gestion_date,
         Achillea_millefolium:Vicia_tetrasperma_subsp._tetrasperma,
         Aglais_io:Volucella_zonaria)

# Fusionner les quadrats
Inventaire_agg <- aggregate(.~ Site_gestion_date, data=Inventaire, FUN=sum)
write.csv2(Inventaire_agg, file= "Data_brouillons/Aggrege.csv")


# Aire 
Inventaire %>% 
  group_by(Site) %>% 
  summarize(area = (Area_gis_m_sq)) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarize(sum= sum(area))
area <- Inventaire %>% 
  group_by(Site) %>% 
  summarize(area = (Area_gis_m_sq)) %>% 
  distinct() 
write.csv2(area, file = "Output/area.csv")




summary(Interactions_Gestion)

# interactions zero
zero_Q <- Interactions %>% 
  replace(is.na(.), 0) %>% 
  group_by(Site_gestion_date_Quadrat) %>% 
  summarise(data = sum(N_Interactions)) %>% 
  filter(data == 0)
zero_Q #18 Q

zero_site <- Interactions_Gestion %>% 
  replace(is.na(.), 0) %>% 
  group_by(Site_gestion_date) %>% 
  summarise(data = sum(N_Interactions)) %>% 
  filter(data == 0)
zero_site  # 14 parcelles! 
# 6 qui n'ont pas de plantes du tout

Pl_zero <- Interactions_Gestion %>% 
  replace(is.na(.), 0) %>% 
  group_by(Site_gestion_date, Sp_Plantes) %>% 
  summarise(data = sum(N_Interactions)) %>% 
  filter(data == 0)
Pl_zero #246

Pl_zero <- Interactions_Gestion %>% 
  replace(is.na(.), 0) %>% 
  group_by(Sp_Plantes) %>% 
  summarise(data = sum(N_Interactions)) %>% 
  filter(data == 0)
Pl_zero #16 espèces de plantes non-visitées, dans aucun quadrat!
write.csv2(Pl_zero, file = "Output/Plzero.csv")

Pl_zero <- Interactions_Gestion %>% 
  replace(is.na(.), 0)
Pl_zero

# Plantes et poll remarquables: 

somme_spplantes <- Interact_esp %>% 
  group_by(Sp_Plantes) %>% 
  summarise(sum_bis = sum(sum)) %>% 
  arrange(desc(sum_bis))
View(somme_spplantes)


somme_spplantetpoll <- Interact_esp %>%
  filter(Sp_Pollinisateurs != "") %>% 
  group_by(Sp_Plantes, Sp_Pollinisateurs) %>% 
  summarise(N_interactions = sum(sum)) %>% 
  arrange(desc(N_interactions))
View(somme_spplantetpoll)

nombre_int_sppolletplantes <- Interact_esp %>%
  filter(Sp_Pollinisateurs != "") %>% 
  group_by(Sp_Plantes, Sp_Pollinisateurs) %>% 
  summarize(nombredefoiscetypedinteraction=n())
View(nombre_int_sppolletplantes)

Nombrespgrpolldiff <-  Interactions_Classes %>%
  filter(Sp_Pollinisateurs != "") %>% 
  group_by(Sp_Plantes) %>% 
  summarise("Nombre d'espèces/groupes de pollinisateurs différents" = n_distinct(Sp_Pollinisateurs),
            "Nombre de catégories de pollinisateurs différentes" = n_distinct(Classe_Poll))
View(Nombrespgrpolldiff)
write.csv2(Nombrespgrpolldiff, file = "Output/Nombrespgrpolldiff.csv")


Nombrespgrplantesadiff <-  Interact_esp %>%
  filter(Sp_Pollinisateurs != "") %>% 
  group_by(Sp_Pollinisateurs) %>% 
  summarise("Nombre d'espèces/groupes de plantes différents" = n_distinct(Sp_Plantes))
View(Nombrespgrplantesadiff)
write.csv2(Nombrespgrplantesadiff, file = "Output/Nombrespgrplantesadiff.csv")

Nombrespgrplantesadiff_classe <-  Interactions_Classes %>%
  filter(Sp_Pollinisateurs != "") %>% 
  group_by(Classe_Poll) %>% 
  summarise("Nombre d'espèces/groupes de plantes différents" = n_distinct(Sp_Plantes))
View(Nombrespgrplantesadiff_classe)
write.csv2(Nombrespgrplantesadiff_classe, file = "Output/Nombrespgrplantesadiff_classe.csv")
