#>   method      from
#>   print.bytes Rcpp
#> 3.46 MB
pryr::object_size(diamonds2)
#> 3.89 MB
pryr::object_size(diamonds, diamonds2)
library(magrittr)
diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>%
  dplyr::mutate(price_per_carat = price / carat)
install.packages("plotKML")
install.packages("devtools")
devtools::install_github("ehrscape/R-project/AnalyzeGPS")
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("rgdal")
install.packages("sf")
install.packages("raster")
install.packages("sp")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("rlang")
install.packages("rlang")
install.packages("htmltools")
install.packages("leafsync")
install.packages("rgeos")
library(readxl)
paddle_data <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/paddle_data.xlsx",
                          sheet = "Daten")
View(paddle_data)
#Vergleich von überlebenden larven zwischen Treatments
View(paddle_data)
#Vergleich von überlebenden larven zwischen Treatments
lmer
library(lme4)
install.packages("lme4")
#Vergleich von überlebenden larven zwischen Treatments
library(lm4)
#Vergleich von überlebenden larven zwischen Treatments
library('lm4')
library(lm4)
library(lme4)
library(Matrix)
library(lme4)
#Vergleich von überlebenden larven zwischen Treatments
lmer(larvae_alive_total ~ paddle_light + (1|Aquarium))
#Anaylse der Daten aus dem Paddelschlag-Projekt
View(paddle_data)
library(Matrix)
library(lme4)
#Vergleich von Schätzwerten für Anzahl lebende Larven und gezählte Werte
tab_werte <- table(paddle_data)
lmer(larvae_alive_total ~ paddle_light + (1|Aquarium))
library(readxl)
Auskopplung_Eichung <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/Auskopplung_Eichung.xlsx")
View(Auskopplung_Eichung)
View(Auskopplung_Eichung)
View(paddle_data)
View(Auskopplung_Eichung)
library(readxl)
Auskopplung_Eichung <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/Auskopplung_Eichung.xlsx")
View(Auskopplung_Eichung)
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Abweichungen,mu=0) # testet ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Abweichungen1,mu=0) # testet ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(data=Auskopplung_Eichung,x=Abweichungen1,mu=0) # testet ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(data=Auskopplung_Eichung,x=Abweichung1,mu=0) # testet ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(data="Auskopplung_Eichung",x=Abweichung1,mu=0) # testet ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(data="Auskopplung_Eichung",x="Abweichung1",mu=0) # testet ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
#t.test(data="Auskopplung_Eichung",x="Abweichung1",mu=0) # testet ob die Abweichungen im Mittel Null sind
t.test(x=Differenz_ZW-SW, mu=0)
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
#t.test(data="Auskopplung_Eichung",x="Abweichung1",mu=0) # testet ob die Abweichungen im Mittel Null sind
t.test(x="Differenz_ZW-SW", mu=0)
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
#t.test(data="Auskopplung_Eichung",x="Abweichung1",mu=0) # testet ob die Abweichungen im Mittel Null sind
t.test(data="Auskopplung_Eichung", x="Differenz_ZW-SW", mu=0)
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(data="Auskopplung_Eichung",x=Abweichung1,mu=0) # testet ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(data=Auskopplung_Eichung,x=Abweichung1,mu=0) # testet ob die Abweichungen im Mittel Null sind
t.test(x=Abweichung1,mu=0)
t.test(x=Schaetzwert,y=Zahlwert,paired=TRUE) # tested ob beide
t.test(x=Schaetzwert,y=Zahlwert,paired=TRUE) # tested ob beide
library(readxl)
paddle_data <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/paddle_data.xlsx",
                          sheet = "Daten")
View(paddle_data)
t.test(x~y, var.equal, alternative)
t.test(Schaetzwert_5.1~Zahlwert_6.1, var.equal, alternative)
t.test(data="Auskopplung_Eichung", x="Abweichung1", mu=0) #Alles in Anführungzeichen. Andere Fehlermerdung aber funktioniert immer noch nicht
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Abweichung1,mu=0) #Anweisung von Manuel. Funktioniert nicht. Soll testen,ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Auskopplung_Eichung.Abweichung1,mu=0) #Anweisung von Manuel. Funktioniert nicht. Soll testen,ob die Abweichungen im Mittel Null sind
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Auskopplung_Eichung[ ,"Abweichung1"],mu=0) #Anweisung von Manuel. Funktioniert nicht. Soll testen,ob die Abweichungen im Mittel Null sind
t.test(data="Auskopplung_Eichung", x="Abweichung2", mu=0) #Alles in Anführungzeichen. Andere Fehlermerdung aber funktioniert immer noch nicht
t.test(data=Auskopplung_Eichung, x="Abweichung2", mu=0) #Alles in Anführungzeichen. Andere Fehlermerdung aber funktioniert immer noch nicht
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Auskopplung_Eichung[ ,"Abweichung2"],mu=0) #Anweisung von Manuel. Funktioniert nicht. Soll testen,ob die Abweichungen im Mittel Null sind
t.test(x=Auskopplung_Eichung[ , "Schaetzwert_5.1."], y=Auskopplung_Eichung[ , "Zahlwert_6.1."], paired=TRUE) # tested ob beide
t.test(x=Auskopplung_Eichung[ ,"Schaetzwert_5.1."],
       y=Auskopplung_Eichung[ ,"Zahlwert_6.1."], paired=TRUE) # tested ob beide
Schaetzwert<-Auskopplung_Eichung[ ,"Schaetzwert_5.1."]
t.test(x=Schaetzwert,
       y=Auskopplung_Eichung[ ,"Zahlwert_6.1."], paired=TRUE) # tested ob beide
Zahlwert<-Auskopplung_Eichung[ ,"Zahlwert_6.1."]
t.test(x=Schaetzwert, y=Zahlwert, paired=TRUE) # tested ob beide
t.test(Schaetzwert~Zahlwert, paired=TRUE) # tested ob beide
t.test(x=Schaetzwert, y=Zahlwert, paired=TRUE) # tested ob beide
library(readxl)
Auskopplung_Eichung <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/Auskopplung_Eichung.xlsx")
View(Auskopplung_Eichung)
Schaetzwert<-Auskopplung_Eichung[ ,"Schaetzwert"]
Zahlwert<-Auskopplung_Eichung[ ,"Zahlwert"]
t.test(x=Schaetzwert, y=Zahlwert, paired=TRUE) # tested ob beide
view(Schaetzwert)
View(Schaetzwert)
library(readxl)
Auskopplung_Eichung <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/Auskopplung_Eichung.xlsx")
View(Auskopplung_Eichung)
Schaetz_Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y" ,"Schaetzwert"]
Zahl_Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y" ,"Zahlwert"]
t.test(x=Schaett_Lebend, y=Zahl_Lebend, paired=TRUE) # tested ob beide
t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) # tested ob beide
Schaetz_Lebend<-as.data.frame(Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y" ,"Schaetzwert"])
Zahl_Lebend<-as.data.frame(Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y" ,"Zahlwert"])
t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) # tested ob beide
View(Schaetz_Lebend)
View/Zahl_Lebend)
View(Zahl_Lebend)
Schaetz_Lebend<-as.data.frame(Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y"]$Schaetzwert)
View(Schaetz_Lebend)
Schaetz_Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y"]$Schaetzwert
Blubb<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y"]
library(readxl)
Auskopplung_Eichung <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/Auskopplung_Eichung.xlsx")
View(Auskopplung_Eichung)
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Auskopplung_Eichung[ ,"Abweichung2"],mu=0) #Testet ob die Abweichung2 im Mittel Null ist
Blubb<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y"]
View(Auskopplung_Eichung)
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Auskopplung_Eichung[ ,"Abweichung2"],mu=0) #Testet ob die Abweichung2 im Mittel Null ist
Blubb<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y"]
Blubb<-Auskopplung_Eichung[Auskopplung_Eichung$"Totalausfall" !="Y"]
Zahl_Lebend<-as.data.frame(Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y" ,"Zahlwert"])
t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) # tested ob beide
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Auskopplung_Eichung[ ,"Abweichung2"],mu=0) #Testet ob die Abweichung2 im Mittel Null ist
Blubb<-Auskopplung_Eichung[Auskopplung_Eichung$"Totalausfall" !="Y"]
Blubb<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y"]
Blubb<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y",]
Blubb<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y", ]
Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall !="Y", ]
Schaetz_Lebend<-Lebend$Schaetzwert
Zahl_Lebend<-Lebend$Zahlwert
t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) # tested ob beide
View(Schaetz_Lebend)
View(Lebend)
Auskopplung_Eichung[3, ]
Auskopplung_Eichung[3,"Schaetzwert"]
Auskopplung_Eichung$Totalausfall !="Y"
library(readxl)
Auskopplung_Eichung <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/Auskopplung_Eichung.xlsx")
View(Auskopplung_Eichung)
Auskopplung_Eichung$Totalausfall =="N",
Auskopplung_Eichung$Totalausfall =="N"
Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall =="N", ]
Schaetz_Lebend<-Lebend$Schaetzwert
Zahl_Lebend<-Lebend$Zahlwert
t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) # tested ob beide
library(readxl)
paddle_data <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/paddle_data.xlsx",
                          sheet = "Daten")
View(paddle_data)
###Notizen für Auswertung 2
# Wie groß ist der Fehler der Schätzung
t.test(x=Auskopplung_Eichung[ ,"Abweichung2"],mu=0) #Testet ob die Abweichung2 im Mittel Null ist
t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) # tested ob beide
boxplot(Auskopplung_Eichung[ ,"Abweichung2"])
# Gibt es systematische Unterschiede zwischen den Treatments
av1<-aov(Schaetz_Lebend~Lebend$Treatment)
summary(av1)
TukeyHSD(av1,which="Lebend$Treatment")
# Darstellung
boxplot(Schaetz_Lebend~Lebend$Treatment)
median(Auskopplung_Eichung[ ,"Abweichung2"])
median(Auskopplung_Eichung[ ,"Abweichung2"], )
median(Auskopplung_Eichung[ ,"Abweichung2"], na.rm=FALSE)
Auskopplung_Eichung[ ,"Abweichung2"
                     Auskopplung_Eichung[ ,"Abweichung2"]
                     Auskopplung_Eichung[ ,"Abweichung2"]
                     median(Auskopplung_Eichung[ ,"Abweichung2"], na.rm=TRUE)
                     median(Lebend$Abweichung2, na.rm=TRUE)
                     boxplot(Lebend$Abweichung2) #Im Mittel sind Schaetzwerte ca 15% zu hoch
                     ###Notizen für Auswertung 2
                     # Wie groß ist der Fehler der Schätzung
                     t.test(x=Lebend$Abweichung2,mu=0) #Testet ob die Abweichung2 im Mittel Null ist. Schaetz unterscheidet sich sig vom Zahlwert
                     ###Notizen für Auswertung 2
                     # Wie groß ist der Fehler der Schätzung
                     Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall =="N", ]
                     t.test(x=Lebend$Abweichung2,mu=0) #Testet ob die Abweichung2 im Mittel Null ist. Schaetz unterscheidet sich sig vom Zahlwert
                     Schaetz_Lebend<-Lebend$Schaetzwert
                     Zahl_Lebend<-Lebend$Zahlwert
                     t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) #Tested ob Schaetz und Zahl im Mittel Null sind. Sie sind es nicht. Meine Schaetzwerte sind doof.
                     boxplot(Lebend$Abweichung2) #Im Mittel sind Schaetzwerte ca 15% zu hoch
                     median(Lebend$Abweichung2, na.rm=TRUE)
                     #Anaylse der Daten aus dem Paddelschlag-Projekt
                     View(paddle_data)
                     library(Matrix)
                     library(lme4)
                     ###Notizen für Auswertung 2
                     # Wie groß ist der Fehler der Schätzung
                     Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall =="N", ]
                     t.test(x=Lebend$Abweichung2,mu=0) #Testet ob die Abweichung2 im Mittel Null ist. Schaetz unterscheidet sich sig vom Zahlwert
                     Schaetz_Lebend<-Lebend$Schaetzwert
                     Zahl_Lebend<-Lebend$Zahlwert
                     t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) #Tested ob Schaetz und Zahl im Mittel Null sind. Sie sind es nicht. Meine Schaetzwerte sind doof.
                     boxplot(Lebend$Abweichung2) #Im Mittel sind Schaetzwerte ca 15% zu hoch
                     median(Lebend$Abweichung2, na.rm=TRUE) #Abweichung von +16%
                     mean(Lebend$Abweichung2, na.rm=TRUE)
                     t.test(x=Lebend$Abweichung2,mu=0) #Testet ob die Abweichung2 im Mittel Null ist. Schaetz unterscheidet sich sig vom Zahlwert
                     # Gibt es systematische Unterschiede zwischen den Treatments
                     av1<-aov(Schaetz_Lebend~Lebend$Treatment)
                     summary(av1) #p=ns, d.h. Schaetzwerte unterscheiden sich nicht zwischen Treatments
                     TukeyHSD(av1,which="Lebend$Treatment") #Auch hier kein Unterschied
                     # Darstellung
                     boxplot(Schaetz_Lebend~Lebend$Treatment) #Kein Unterschied
                     # Darstellung
                     boxplot(Schaetz_Lebend~Lebend$Treatment) #Kein Unterschied
                     head(Lebend)
                     av2<-aov(ZahlLebend$Treatment)
                     av2<-aov(Zahl_Lebend$Treatment)
                     av2<-aov(Zahl_Lebend~Lebend$Treatment)
                     summary(av2)
                     head(Lebend)
                     av1<-lm(Schaetz_Lebend~Lebend$Treatment)
                     summary(av1)
                     head(Lebend)
                     av1<-lm(Schaetzwert~Treatment, data=Lebend)
                     summary(av1)
                     ###Notizen für Auswertung 2
                     # Wie groß ist der Fehler der Schätzung
                     Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall =="N", ]
                     t.test(x=Lebend$Abweichung2,mu=0) #Testet ob die Abweichung2 im Mittel Null ist. Schaetz unterscheidet sich sig vom Zahlwert
                     Schaetz_Lebend<-Lebend$Schaetzwert
                     Zahl_Lebend<-Lebend$Zahlwert
                     t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) #Tested ob Schaetz und Zahl im Mittel Null sind. Sie sind es nicht. Meine Schaetzwerte sind doof.
                     boxplot(Lebend$Abweichung2) #Im Mittel sind Schaetzwerte ca 15% zu hoch
                     median(Lebend$Abweichung2, na.rm=TRUE) #Abweichung von +16%
                     mean(Lebend$Abweichung2, na.rm=TRUE)
                     # Gibt es systematische Unterschiede zwischen den Treatments
                     av1<-aov(Schaetz_Lebend~Lebend$Treatment)
                     av2<-aov(Zahl_Lebend~Lebend$Treatment)
                     summary(av1) #p=ns, d.h. Schaetzwerte unterscheiden sich nicht zwischen Treatments
                     summary(av2)
                     TukeyHSD(av1,which="Lebend$Treatment") #Auch hier kein Unterschied
                     # Darstellung
                     boxplot(Schaetz_Lebend~Lebend$Treatment) #Kein Unterschied
                     head(Lebend)
                     lm1<-lm(Schaetzwert~Treatment, data=Lebend)#statt treatment Sediment + paddel. wenn beides ns dann das weniger s rauswerfern und noch mal testen
                     summary(lm1)
                     # Außerdem:
                     # Darstellung des zeitlichen Verlaufs
                     #mittelwert pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
                     lm2<-lm(Schaetzwert~Sediment+Paddel+Zeit, data=Lebend)#Zeit wird einen effekt haben, dewegen Interaktionsterm (Manuel)
                     glm2<-glm(Schaetzwert~Sediment+Paddel+Zeit, data=Lebend, family=poisson)
                     library(readxl)
                     Auskopplung_Eichung <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/Auskopplung_Eichung.xlsx")
                     View(Auskopplung_Eichung)
                     library(readxl)
                     paddle_data <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/paddle_data.xlsx",
                                               sheet = "Daten")
                     View(paddle_data)
                     # Außerdem:
                     # Darstellung des zeitlichen Verlaufs
                     #mittelwert pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
                     lm2<-lm(Schaetzwert~T_Sediment+T_Paddel+day_experiment, data=Lebend)#Zeit wird einen effekt haben, dewegen Interaktionsterm (Manuel)
                     ###Notizen für Auswertung 2
                     # Wie groß ist der Fehler der Schätzung
                     Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall =="N", ]
                     t.test(x=Lebend$Abweichung2,mu=0) #Testet ob die Abweichung2 im Mittel Null ist. Schaetz unterscheidet sich sig vom Zahlwert
                     Schaetz_Lebend<-Lebend$Schaetzwert
                     Zahl_Lebend<-Lebend$Zahlwert
                     t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) #Tested ob Schaetz und Zahl im Mittel Null sind. Sie sind es nicht. Meine Schaetzwerte sind doof.
                     boxplot(Lebend$Abweichung2) #Im Mittel sind Schaetzwerte ca 15% zu hoch
                     median(Lebend$Abweichung2, na.rm=TRUE) #Abweichung von +16%
                     mean(Lebend$Abweichung2, na.rm=TRUE) #Abweichung +13%
                     # Gibt es systematische Unterschiede zwischen den Treatments
                     av1<-aov(Schaetz_Lebend~Lebend$Treatment)
                     av2<-aov(Zahl_Lebend~Lebend$Treatment)
                     summary(av1) #p=ns, d.h. Schaetzwerte unterscheiden sich nicht zwischen Treatments
                     summary(av2) #p=ns, d.h. Schaetzwerte unterscheiden sich nicht zwischen Treatments
                     TukeyHSD(av1,which="Lebend$Treatment") #Auch hier kein Unterschied
                     # Darstellung
                     boxplot(Schaetz_Lebend~Lebend$Treatment) #Kein Unterschied
                     head(Lebend)
                     lm1<-lm(Schaetzwert~Treatment, data=Lebend) #statt treatment Sediment + paddel. wenn beides ns dann das weniger s rauswerfern und noch mal testen
                     summary(lm1)
                     lm2<-lm(Schaetzwert~T_Sediment+T_Sediment, data=Lebend)
                     Summary(lm2)
                     summary(lm2)
                     lm1<-lm(Schaetzwert~Treatment, data=Lebend) #statt treatment Sediment + paddel. wenn beides ns dann das weniger s rauswerfern und noch mal testen
                     summary(lm1)
                     lm2<-lm(Schaetzwert~T_Sediment+T_Sediment, data=Lebend) #
                     summary(lm2)
                     lm2<-lm(Schaetzwert~T_Sediment+T_Paddel, data=Lebend) #
                     summary(lm2)
                     lm1<-lm(Schaetzwert~Treatment, data=Lebend) #ns
                     summary(lm1)
                     lm1a<-lm(Schaetzwert~T_Sediment+T_Paddel, data=Lebend) #beides ns. Faktor mit weniger srauswerfen und noch mal versuchen
                     summary(lm1a)
                     lm1b<-lm(Schaetzwert~T_Sediment, data=Lebend)
                     summary(lm1b)
                     # Außerdem:
                     # Darstellung des zeitlichen Verlaufs
                     #mittelwert pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
                     lm2<-lm(Schaetzwert~T_Sediment+T_Paddel+day_experiment, data=Lebend)#Zeit wird einen effekt haben, dewegen Interaktionsterm (Manuel)
                     # Außerdem:
                     # Darstellung des zeitlichen Verlaufs
                     #mittelwert pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
                     lm2<-lm(Schaetzwert~T_Sediment+T_Paddel+day_experiment, data=paddle_data)#Zeit wird einen effekt haben, dewegen Interaktionsterm (Manuel)
                     library(readxl)
                     Auskopplung_Data <- read_excel("C:/Users/bt306529/Doktorarbeit/Projekte/Projekt_Kanusport-Projekte/Projekt_Paddelschlag/Data/Auskopplung_Data.xlsx")
                     View(Auskopplung_Data)
                     # Darstellung des zeitlichen Verlaufs
                     #mittelwert pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
                     lm2<-lm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data)#Zeit wird einen effekt haben, dewegen Interaktionsterm (Manuel)
                     summary(lm2)
                     glm2<-glm(ecc_larvae~Sediment+Paddel+Zeit, data=Auskopplung_Data, family=poisson)
                     glm2<-glm(egg_larvae~Sediment+Paddel+Zeit, data=Auskopplung_Data, family=poisson)
                     glm2<-glm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data, family=poisson)
                     summary(glm2)
                     boxplot(Schaetzwert~Auskopplung_Data$day_experiment)
                     boxplot(egg_larvae~Auskopplung_Data$day_experiment)
                     boxplot(Auskopplung_Data~egg_larvae~Auskopplung_Data$day_experiment)
                     boxplot(Auskopplung_Data$egg_larvae~Auskopplung_Data$day_experiment)
                     #Darstellung zu 2.
                     boxplot(Schaetz_Lebend~Lebend$Treatment) #Kein Unterschied
                     boxplot(Auskopplung_Data$egg_larvae~Auskopplung_Data$Treat,emt)
                     boxplot(Auskopplung_Data$egg_larvae~Auskopplung_Data$Treatment)
                     boxplot(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data)
                     boxplot(Auskopplung_Data$egg_larvae~Auskopplung_Data$T_Sediment+T_Paddel+day_experiment)
                     boxplot(Auskopplung_Data$egg_larvae~Auskopplung_Data$T_Sediment+Auskopplung_Data$T_Paddel+Auskopplung_Data$day_experiment)
                     ###Auswertung
                     #1. Wie groß ist der Fehler der Schätzung (data=Auskopplung_Eichung)
                     Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall =="N", ]
                     t.test(x=Lebend$Abweichung2,mu=0) #Testet ob die Abweichung2 im Mittel Null ist. Schaetz unterscheidet sich sig vom Zahlwert
                     boxplot(Lebend$Abweichung2) #Im Mittel sind Schaetzwerte ca 15% zu hoch
                     median(Lebend$Abweichung2, na.rm=TRUE) #Abweichung von +16%
                     mean(Lebend$Abweichung2, na.rm=TRUE) #Abweichung +13%
                     #2. Gibt es systematische Unterschiede zwischen den Treatments (data= Auskopplung_Eichung -> wiederholen mit kompletten Datensatz?)
                     av1<-aov(Schaetz_Lebend~Lebend$Treatment)
                     Schaetz_Lebend<-Lebend$Schaetzwert
                     Zahl_Lebend<-Lebend$Zahlwert
                     t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) #Tested ob Schaetz und Zahl im Mittel Null sind. Sie sind es nicht. Meine Schaetzwerte sind doof.
                     #2. Gibt es systematische Unterschiede zwischen den Treatments (data= Auskopplung_Eichung -> wiederholen mit kompletten Datensatz?)
                     av1<-aov(Schaetz_Lebend~Lebend$Treatment)
                     summary(av1) #p=ns, d.h. Schaetzwerte unterscheiden sich nicht zwischen Treatments
                     TukeyHSD(av1,which="Lebend$Treatment") #Auch hier kein Unterschied
                     av2<-aov(Zahl_Lebend~Lebend$Treatment)
                     summary(av2) #p=ns, d.h. Schaetzwerte unterscheiden sich nicht zwischen Treatments
                     head(Lebend)
                     lm1<-lm(Schaetzwert~Treatment, data=Lebend) #ns
                     summary(lm1)
                     lm1a<-lm(Schaetzwert~T_Sediment+T_Paddel, data=Lebend) #beides ns. Faktor mit weniger s rauswerfen und noch mal versuchen
                     summary(lm1a)
                     lm1b<-lm(Schaetzwert~T_Sediment, data=Lebend) #immernoch ns
                     summary(lm1b)
                     #Darstellung zu 2.
                     boxplot(Schaetz_Lebend~Lebend$Treatment) #Kein Unterschied
                     #3. Darstellung des zeitlichen Verlaufs
                     lm2<-lm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data) #Zeit wird einen effekt haben, deswegen Interaktionsterm (Manuel)
                     summary(lm2)
                     glm2<-glm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data, family=poisson)
                     summary(glm2)
                     #Mittelwert pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
                     boxplot(Auskopplung_Data$egg_larvae~Auskopplung_Data$T_Sediment+Auskopplung_Data$T_Paddel+Auskopplung_Data$day_experiment)
                     library(ggplot2)
                     library(ggplot2)
                     Sediment<-Auskopplung_Data[Auskopplung_Data$T_Sediment=="Y", ]
                     Sediment
                     boxplot(Sediment$egg_larvae~Sediment$day_experiment)
                     ggplot(data=Sediment, aes(x=day_experiment, y=egg_larvae))
                     ggplot(data=Sediment, aes(x=day_experiment, y=egg_larvae))
                     Sediment_means<-aggregate(Sediment$egg_larvae, list(Sediment$day_experiment), FUN=mean)
                     Sediment_means
                     Sediment
                     view(Sediment)
                     View(Sediment)
                     Sediment_means<-aggregate(Sediment$egg_larvae, list(Sediment$day_experiment), FUN=function(x) mean(x, na.rm=TRUE), na.action=na.pass) #Mitterlwerte pro Tag für alle Sediemnt Treatments
                     Sediment_means<-aggregate(Sediment$egg_larvae, list(Sediment$day_experiment), FUN=(function(x) mean(x, na.rm=TRUE)), na.action=na.pass) #Mitterlwerte pro Tag für alle Sediemnt Treatments
                     Sediment_means<-aggregate(Sediment$egg_larvae, list(Sediment$day_experiment), FUN=mean, na.action=na.omit) #Mitterlwerte pro Tag für alle Sediemnt Treatments
                     Sediment_means
                     Sediment_means<-aggregate(Sediment$egg_larvae, list(Sediment$day_experiment), FUN=mean, na.action=na.rm) #Mitterlwerte pro Tag für alle Sediemnt Treatments
                     Sediment_means
                     Sediment_means<-aggregate(Sediment$egg_larvae, list(Sediment$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Sediemnt Treatments
                     Sediment_means
                     View(Sediment_means)
                     ggplot(data=Sediment_means, aes(x=day_experiment, y=egg_larvae))+geom_point(x=Group.1, y=x)
                     ggplot(data=Sediment_means, aes(x=day_experiment, y=egg_larvae))+geom_point(data=Sediment_means, x=Group.1, y=x)
                     ggplot(data=Sediment_means, aes(x=day_experiment, y=egg_larvae))+geom_point(x=Sediment_means$Group.1, y=Sediment_means$x)
                     ggplot(data=Sediment_means, aes(x=day_experiment, y=egg_larvae))+geom_qq(x=Sediment_means$Group.1, y=Sediment_means$x)
                     ggplot(data=Sediment_means, aes(x=day_experiment, y=egg_larvae))+geom_line(x=Sediment_means$Group.1, y=Sediment_means$x)
                     ggplot(data=Sediment_means, aes(x=Group.1, y=x))+geom_point()
                     ggplot(data=Sediment_means, aes(x=Group.1, y=x))+geom_point()+scale_x_continuous(name="Day of Experiment")+scale_y_continuous(name="mean number larvae (sediment tretament)")
                     ggplot(data=Sediment_means, aes(x=Group.1, y=x))+geom_point()+scale_x_continuous(name="day of experiment")+scale_y_continuous(name="mean number larvae (sediment tretament)")
                     +scale_y_continuous(name="mean number larvae (sediment tretament)")
                     ggplot(data=Sediment_means, aes(x=Group.1, y=x)) +
                       geom_point()+scale_x_continuous(name="day of experiment") +
                       scale_y_continuous(name="mean number larvae (sediment tretament)")
                     ggplot(data=Sediment_means, aes(x=Group.1, y=x)) +
                       geom_point()+scale_x_continuous(name="day of experiment") +
                       scale_y_continuous(name="mean number larvae (sediment tretament)") +
                       geom_line()
                     Sediment_plot<-ggplot(data=Sediment_means, aes(x=Group.1, y=x)) +
                       geom_point()+scale_x_continuous(name="day of experiment") +
                       scale_y_continuous(name="mean number larvae (sediment tretament)") +
                       geom_line()
                     Data_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, shape=Treatment)) +
                       geom_point()
                     Data_plot
                     Data_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Treatment)) +
                       geom_point()
                     Data_plot
                     Data_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Treatment)) +
                       geom_point(position="jitter")
                     Data_plot
                     Data_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Treatment)) +
                       geom_point(position="jitter") +
                       geom_line()
                     Data_plot
                     Sediment_plot<-ggplot(data=Sediment_means, aes(x=Group.1, y=x)) +
                       geom_point() +
                       scale_x_continuous(name="day of experiment") +
                       scale_y_continuous(name="mean number larvae (sediment tretament)") +
                       geom_line()
                     Sediment_plot
                     lm1a<-lm(Schaetzwert~T_Sediment+T_Paddel, data=Lebend) #beides ns. Faktor mit weniger s rauswerfen und noch mal versuchen
                     summary(lm1a)
                     lm1b<-lm(Schaetzwert~T_Sediment, data=Lebend) #immernoch ns
                     summary(lm1b)
                     #Darstellung zu 2.
                     boxplot(Schaetz_Lebend~Lebend$Treatment) #Kein Unterschied
                     #3. Darstellung des zeitlichen Verlaufs
                     lm2<-lm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data) #Zeit wird einen effekt haben, deswegen Interaktionsterm (Manuel)
                     summary(lm2)
                     glm2<-glm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data, family=poisson)
                     summary(glm2)
                     #Mittelwert pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
                     boxplot(Auskopplung_Data$egg_larvae~Auskopplung_Data$T_Sediment+Auskopplung_Data$T_Paddel+Auskopplung_Data$day_experiment)
                     boxplot(Sediment$egg_larvae~Sediment$day_experiment) #Testlauf mit Boxplot
                     Sediment_plot<-ggplot(data=Sediment_means, aes(x=Group.1, y=x)) +
                       geom_point() +
                       scale_x_continuous(name="day of experiment") +
                       scale_y_continuous(name="mean number larvae (sediment tretament)") +
                       geom_line()
                     Sediment_plot
                     #Mittelwerte!
                     Data_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Treatment)) +
                       geom_point(position="jitter") +
                       geom_line()
                     #Mittelwerte!
                     Data_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Treatment)) +
                       geom_point(position="jitter") +
                       #geom_line()
                       Data_plot
                     #Mittelwerte!
                     Data_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Treatment)) +
                       geom_point(position="jitter") +
                       geom_line()
                     Data_plot
                     library(visreg)
                     install.packages("visreg")
                     library(visreg)
                     visreg(lm2, xvar=T_Sediment)
                     #3. Darstellung des zeitlichen Verlaufs
                     lm2<-lm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data) #Zeit wird einen effekt haben, deswegen Interaktionsterm (Manuel)
                     visreg(lm2, xvar=T_Sediment)
                     visreg(lm2, xvar="T_Sediment")
                     visreg(lm2, xvar="T_Sediment", by="day_experiment")
                     visreg(lm2, xvar="T_Sediment", by="T_Paddel")
                     visreg(glm2, xvar="T_Sediment", by="day_experiment", scale="response")
                     ?visreg