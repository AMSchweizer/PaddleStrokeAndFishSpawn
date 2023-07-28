#Anaylse der Daten aus dem Paddelschlag-Projekt
View(paddle_data)
library(Matrix)
library(lme4)

###Notizen für Auswertung
#1.Vergleich von Schätzwerten für Anzahl lebende Larven und gezählte Werte.
#->"_filled" -Tabellen wurden erstellt
#2.Vergleich von überlebenden Larven zwischen Treatments am letzten Tag
#->Es gibt keinen s Unterschied zwischen den Treatments für die Anzahl der Larven
#->soll Varianz der Treatments angeschaut werden?
#->soll lieber mit lmer gearbeitet werden? Wie genau?
#3.Vergleich von überlebenden Larven zwischen Treatments über gesamten Versuch
#->lineare Modelle nicht anwendbar, deswegen keine mathematischen Ergebnisse
#->plot vom zeitlichen Verlauf der larvenanzahl erstellt
#Vergleich von toten Eiern zwischen Treatments
#Verleich von Pilzbefall zwischen Treatments

###Auswertung
###Larvenanzahl
#1. Wie groß ist der Fehler der Schätzung (data=Auskopplung_Eichung)
Lebend<-Auskopplung_Eichung[Auskopplung_Eichung$Totalausfall =="N", ]
t.test(x=Lebend$Abweichung2,mu=0) #Testet ob die Abweichung2 im Mittel Null ist. Schaetz unterscheidet sich sig vom Zahlwert

Schaetz_Lebend<-Lebend$Schaetzwert
Zahl_Lebend<-Lebend$Zahlwert
t.test(x=Schaetz_Lebend, y=Zahl_Lebend, paired=TRUE) #Tested ob Schaetz und Zahl im Mittel Null sind. Sie sind es nicht.

boxplot(Lebend$Abweichung2) #Im Mittel sind Schaetzwerte ca 15% zu hoch
median(Lebend$Abweichung2, na.rm=TRUE) #Abweichung von +16%
mean(Lebend$Abweichung2, na.rm=TRUE) #Abweichung +13%

#2.1 Gibt es systematische Unterschiede zwischen den Treatments am letzten Versuchstag (data= Auskopplung_Eichung)
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

boxplot(Schaetz_Lebend~Lebend$Treatment) #Kein Unterschied

#2.2 Gibt es systematische Unterschiede zwischen den Treatments am letzten Versuchstag (T16)(data= Auskopplung_Eichung_filled)
av2_2.2<-aov(Auskopplung_Eichung_filled$Zahlwert_auf~Auskopplung_Eichung_filled$Treatment)
summary(av2_2.2) #p=ns, d.h. aufgefüllte Zahlwerte unterscheiden sich nicht zwischen Treatments
TukeyHSD(av2_2.2,which="Auskopplung_Eichung_filled$Treatment") #kein Unterschied

lm1_2.2<-lm(Zahlwert_auf~Treatment, data=Auskopplung_Eichung_filled) #ns 
summary(lm1_2.2)
lm1a_2.2<-lm(Zahlwert_auf~T_Sediment+T_Paddel, data=Auskopplung_Eichung_filled) #beides ns. Faktor mit weniger s rauswerfen und noch mal versuchen
summary(lm1a_2.2)
lm1b1_2.2<-lm(Zahlwert_auf~T_Sediment, data=Auskopplung_Eichung_filled) #immernoch ns
summary(lm1b1_2.2)
lm1b2_2.2<-lm(Zahlwert_auf~T_Paddel, data=Auskopplung_Eichung_filled) #immernoch ns
summary(lm1b2_2.2)

boxplot(Auskopplung_Eichung_filled$Zahlwert_auf~Auskopplung_Eichung_filled$Treatment, range=0) #sieht vielversprechend aus aber statistisch ns

#2.3. Boxplot zu Tag 11
Auskopplung_Data_11 <- Auskopplung_Data_filled[Auskopplung_Data_filled$day_experiment==11, ]
avT11<-aov(Auskopplung_Data_11$egg_larvae~Auskopplung_Data_11$Treatment)
summary(avT11) #p=0.0426, d.h. Messwerte unterscheiden sich  zwischen Treatments
TukeyHSD(avT11,which="Auskopplung_Data_11$Treatment") #Unterschied: sediment-paddle_strong_sediment mit p=0.0358943
larvae_11<-Auskopplung_Data[Auskopplung_Data$day_experiment==11,]
aggregate(larvae_11$egg_larvae, list(larvae_11$Treatment), FUN=mean)
aggregate(larvae_11$egg_larvae, list(larvae_11$Treatment), FUN=sd)

boxplot(Auskopplung_Data_11$egg_larvae~Auskopplung_Data_11$Treatment, range=0)

#2.3. Boxplot zu Tag 9
Auskopplung_Data_9 <- Auskopplung_Data_filled[Auskopplung_Data_filled$day_experiment==9, ]
avT9<-aov(Auskopplung_Data_9$egg_larvae~Auskopplung_Data_9$Treatment)
summary(avT9) #p=0.000292, d.h. Messwerte unterscheiden sich  zwischen Treatments
TukeyHSD(avT9,which="Auskopplung_Data_9$Treatment") #Unterschied: sediment-paddle_strong_sediment mit p=0.0358943

boxplot(Auskopplung_Data_9$egg_larvae~Auskopplung_Data_9$Treatment)

#3.0 Gibt es systematische Unterschiede zwischen den Treatments über den gesamten Versuch
#->Auskommentiert weil: Die daten sind im zeitlichen Verlauf nicht linear.
#->Deswegen können lm und glm nicht angewendet werden. Sie liefern hier zudem gegensätzliche Ergebnisse.
##3.0.1 Darstellung des zeitlichen Verlaufs (Auskopplung_data)
#lm2<-lm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data) #Zeit wird einen effekt haben, deswegen Interaktionsterm (Manuel)
#summary(lm2) #day_experiment ist s
#glm2<-glm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data, family=poisson)
#summary(glm2) #alles s. Manuel schaut nach
#install.packages("visreg")
#library(visreg)
#visreg(lm2, xvar="T_Sediment", by="day_experiment") #prediction line immer gerade aber verschiedene Werte
#visreg(lm2, xvar="T_Sediment", by="T_Paddel") #prediction line immer gerade und immer gleicher wert
#visreg(glm2, xvar="T_Sediment", by="day_experiment", scale="response")
#?visreg
#Median pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
#boxplot(Auskopplung_Data$egg_larvae~Auskopplung_Data$T_Sediment+Auskopplung_Data$T_Paddel+Auskopplung_Data$day_experiment)
#library(ggplot2)
#install.packages("ggthemes")
#library(ggthemes)
library(ggplot2)
ggplot(data=Auskopplung_Data)+
  geom_boxplot(aes(x=factor(day_experiment), y=egg_larvae, fill=factor(Treatment)))

##3.0.2 Darstellung des zeitlichen Verlaufs (Auskopplung_Data_filled)
#lm2.2<-lm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data_filled) #Zeit wird einen effekt haben, deswegen Interaktionsterm (Manuel)
#summary(lm2.2) #day_experiment ist s
#plot(lm2.2)
#glm2.2<-glm(egg_larvae~T_Sediment+T_Paddel+day_experiment, data=Auskopplung_Data_filled, family=poisson)
#summary(glm2.2) #alles s. Manuel schaut nach
#Median pro treatment und zeitpunkt jeweils plotten gegen Zeit. Mit Fehlerbalken
#boxplot(Auskopplung_Data_filled$egg_larvae~Auskopplung_Data_filled$T_Sediment+Auskopplung_Data_filled$T_Paddel+Auskopplung_Data_filled$day_experiment)
ggplot(data=Auskopplung_Data_filled)+
  geom_boxplot(aes(x=factor(day_experiment), y=egg_larvae, fill=factor(Treatment))) #sieht besser aus als plot davor

###Plots wurden alle mit den aufgefüllten Daten erstellt###
library(ggplot2)
#3.1 Control
Control<-Auskopplung_Data_filled[Auskopplung_Data_filled$Treatment=="control", ] #Alle Zeilen mit control-Treatment
#View(Control)
Control_means<-aggregate(Control$egg_larvae, list(Control$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle control-Treatment
#View(Control_means)
boxplot(Control$egg_larvae~Control$day_experiment) #Testlauf mit Boxplot
Control_plot<-ggplot(data=Control_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (no tretament)") +
  geom_line()
Control_plot

Control_singles_plot<-ggplot(data=Control, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (no tretament)")
Control_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag haben alle Totalausfall

#3.2 Sediment
Sediment<-Auskopplung_Data_filled[Auskopplung_Data_filled$T_Sediment=="Y", ] #Alle Zeilen mit Sediment-Treatment
#View(Sediment)
Sediment_means<-aggregate(Sediment$egg_larvae, list(Sediment$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Sediment Treatments
#View(Sediment_means)
boxplot(Sediment$egg_larvae~Sediment$day_experiment) #Testlauf mit Boxplot
Sediment_plot<-ggplot(data=Sediment_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (sediment tretament)") +
  geom_line()
Sediment_plot

Sediment_singles_plot<-ggplot(data=Sediment, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (all sediment treatments)")
Sediment_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag ein paar Totalausfälle

#3.2.1 Sediment ohne Paddel
Sediment_only<-Auskopplung_Data_filled[Auskopplung_Data_filled$Treatment=="sediment", ] #Alle Zeilen mit Sediment-Treatment und ohne paddel
Sediment_only_means<-aggregate(Sediment_only$egg_larvae, list(Sediment_only$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Sediemnt Treatments
boxplot(Sediment_only$egg_larvae~Sediment_only$day_experiment) #Testlauf mit Boxplot
Sediment_only_plot<-ggplot(data=Sediment_only_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (sediment only tretament)") +
  geom_line()
Sediment_only_plot

Sediment_only_singles_plot<-ggplot(data=Sediment_only, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (sediment only treatments)")
Sediment_only_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag ein paar Totalausfälle

#3.3 Paddel light
Paddel_L<-Auskopplung_Data_filled[Auskopplung_Data_filled$T_Paddel=="L", ] #Alle Zeilen mit Paddel-"l"-Treatment
#View(Paddel_L)
Paddel_L_means<-aggregate(Paddel_L$egg_larvae, list(Paddel_L$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"l"-Treatment
#View(Paddel_L_means)
boxplot(Paddel_L$egg_larvae~Paddel_L$day_experiment) #Testlauf mit Boxplot
Paddel_L_plot<-ggplot(data=Paddel_L_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (light paddel tretament)") +
  geom_line()
Paddel_L_plot

Paddel_L_singles_plot<-ggplot(data=Paddel_L, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (all light paddel treatments)")
Paddel_L_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag ein Totalausfall

#3.3.1 Paddel light ohne Sediment
Paddel_L_only<-Auskopplung_Data_filled[Auskopplung_Data_filled$Treatment=="paddle_light", ] 
Paddel_L_only_means<-aggregate(Paddel_L_only$egg_larvae, list(Paddel_L_only$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"l"-Treatment
boxplot(Paddel_L_only$egg_larvae~Paddel_L_only$day_experiment) #Testlauf mit Boxplot
Paddel_L_only_plot<-ggplot(data=Paddel_L_only_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (light paddel only tretament)") +
  geom_line()
Paddel_L_only_plot

Paddel_L_only_singles_plot<-ggplot(data=Paddel_L_only, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (light paddel only treatments)")
Paddel_L_only_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag ein Totalausfall

#3.4 Paddel strong
Paddel_S<-Auskopplung_Data_filled[AAAuskopplung_Data_filled$T_Paddel=="S", ] #Alle Zeilen mit Paddel-"s"-Treatment
#View(Paddel_S)
Paddel_S_means<-aggregate(Paddel_S$egg_larvae, list(Paddel_S$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"s"-Treatment
#View(Paddel_S_means)
boxplot(Paddel_S$egg_larvae~Paddel_S$day_experiment) #Testlauf mit Boxplot
Paddel_S_plot<-ggplot(data=Paddel_S_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (strong paddel tretament)") +
  geom_line()
Paddel_S_plot

Paddel_S_singles_plot<-ggplot(data=Paddel_S, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (all strong paddel treatments)")
Paddel_L_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag ein Totalausfall

#3.4.1 Paddel strong ohne Sediment
Paddel_S_only<-Auskopplung_Data_filled[Auskopplung_Data_filled$Treatment=="paddle_strong", ]
Paddel_S_only_means<-aggregate(Paddel_S_only$egg_larvae, list(Paddel_S_only$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"s"-Treatment
boxplot(Paddel_S_only$egg_larvae~Paddel_S_only$day_experiment) #Testlauf mit Boxplot
Paddel_S_only_plot<-ggplot(data=Paddel_S_only_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (strong paddel only treatment)") +
  geom_line()
Paddel_S_only_plot

Paddel_S_only_singles_plot<-ggplot(data=Paddel_S_only, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (strong paddel only treatments)")
Paddel_S_only_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag ein Totalausfall

#3.5.1 Überkreuzung 1 - Paddel light mit Sediment
Paddel_L_Sediment<-Auskopplung_Data_filled[Auskopplung_Data_filled$Treatment=="paddle_light_sediment", ] 
Paddel_L_Sediment_means<-aggregate(Paddel_L_Sediment$egg_larvae, list(Paddel_L_Sediment$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"l"-Treatment
boxplot(Paddel_L_Sediment$egg_larvae~Paddel_L_Sediment$day_experiment) #Testlauf mit Boxplot
Paddel_L_Sediment_plot<-ggplot(data=Paddel_L_Sediment_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (light paddel sediment tretament)") +
  geom_line()
Paddel_L_Sediment_plot

Paddel_L_Sediment_singles_plot<-ggplot(data=Paddel_L_Sediment, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (light paddel sediment treatments)")
Paddel_L_Sediment_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag ein Totalausfall

#3.5.2 Überkreuzung 2 - Paddel strong mit Sediment
Paddel_S_Sediment<-Auskopplung_Data_filled[Auskopplung_Data_filled$Treatment=="paddle_strong_sediment", ]
Paddel_S_Sediment_means<-aggregate(Paddel_S_Sediment$egg_larvae, list(Paddel_S_Sediment$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"s"-Treatment
boxplot(Paddel_S_Sediment$egg_larvae~Paddel_S_Sediment$day_experiment) #Testlauf mit Boxplot
Paddel_S_Sediment_plot<-ggplot(data=Paddel_S_Sediment_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae (strong paddel sediment treatment)") +
  geom_line()
Paddel_S_Sediment_plot

Paddel_S_Sediment_singles_plot<-ggplot(data=Paddel_S_Sediment, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
  geom_line() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="number larvae (strong paddel sediment treatments)")
Paddel_S_Sediment_singles_plot #alle Auqarien haben einen ähnlichen Verlauf. Am letzten Tag ein Totalausfall

#3.6.1 Kombinationen der Graphen
#Kombination der means-Treatment-Graphen ohne Überkreuzungen
All_Treatment_only_plot<-ggplot(data=Control_means, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="mean number larvae") +
  geom_line()
All_Treatment_only_plot<-All_Treatment_only_plot +
  geom_point(data=Sediment_only_means, color="dark green") +
  geom_line(data=Sediment_only_means, color="dark green")
All_Treatment_only_plot<-All_Treatment_only_plot +
  geom_point(data=Paddel_L_only_means, color="yellow") +
  geom_line(data=Paddel_L_only_means, color="yellow")
All_Treatment_only_plot<-All_Treatment_only_plot +
  geom_point(data=Paddel_S_only_means, color="blue") +
  geom_line(data=Paddel_S_only_means, color="blue")
All_Treatment_only_plot<-All_Treatment_only_plot +
  geom_point(data=Paddel_L_Sediment_means, color="orange") +
  geom_line(data=Paddel_L_Sediment_means, color="orange")
All_Treatment_only_plot<-All_Treatment_only_plot +
  geom_point(data=Paddel_S_Sediment_means, color="dark turquoise") +
  geom_line(data=Paddel_S_Sediment_means, color="dark turquoise")
All_Treatment_only_plot #eventuell noch Fehlerbalken dran?. Dann noch "zoom" zu passenden boxplot (Zeile 64)
###ENDE lebende Larven###

#Kombination der single-Treatment-Graphen nach Aquarien -> Auskommentiert weil hat bisher nicht geklappt wie wir wollen
#All_Treatment_single_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Aquarium)) +
#  geom_line(na.rm=TRUE) +
#  scale_x_continuous(name="day of experiment") +
#  scale_y_continuous(name="number larvae") +
#  geom_line()
#All_Treatment_single_plot

#Kombination der single-Treatment-Graphen nach Treatment
#All_Treatment_single2_plot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Treatment)) +
#  geom_line(na.rm=TRUE) +
#  scale_x_continuous(name="day of experiment") +
#  scale_y_continuous(name="number larvae") +
#  geom_line()
#All_Treatment_single2_plot

#All_Treatment_single2_Jitterplot<-ggplot(data=Auskopplung_Data, aes(x=day_experiment, y=egg_larvae, color=Treatment)) +
#  geom_point(position="jitter") +
#  geom_line()
#All_Treatment_single2_Jitterplot

###Alles aus 2. und 3. muss noch für Verpilzung und Eier-Überleben gemacht werden

###4. Verpilzung###
Auskopplung_Data$egg_fungi_cum<-ave(Auskopplung_Data$egg_fungi, Auskopplung_Data$Aquarium, FUN=cumsum)#kumulative Verpilzungsrate für jedes einzelne Aquarium
#View(Auskopplung_Data)
#av_fungi_4<-aov(egg_fungi_cum~Treatment, data=Auskopplung_Data[Auskopplung_Data$day_experiment==4,])
#summary(av_fungi_4)#Unterschied zwischen den Treatments
#TukeyHSD(av_fungi_4,which="Treatment")#Unterschiede bei: paddle_strong vs control & paddle_strong vs paddle_light & sediment vs paddle_strong

#av_fungi_8<-aov(egg_fungi_cum~Treatment, data=Auskopplung_Data[Auskopplung_Data$day_experiment==8,])
#summary(av_fungi_8)#kein Unterschied
#TukeyHSD(av_fungi_8,which="Treatment")

#av_fungi_12<-aov(egg_fungi_cum~Treatment, data=Auskopplung_Data[Auskopplung_Data$day_experiment==12,])
#summary(av_fungi_12)#Unterschied zwischen den Treatments
#TukeyHSD(av_fungi_12,which="Treatment")#Unterschiede bei: sediment vs control

av_fungi_11<-aov(egg_fungi_cum~Treatment, data=Auskopplung_Data[Auskopplung_Data$day_experiment==11,])
summary(av_fungi_11)#Unterschied zwischen den Treatments
TukeyHSD(av_fungi_11,which="Treatment")#Unterschiede bei: sediment vs control UND sedimnt vs paddle_light_sediment
boxplot(egg_fungi_cum~Treatment, data=Auskopplung_Data[Auskopplung_Data$day_experiment==11,], range=0)
fungi_11<-Auskopplung_Data[Auskopplung_Data$day_experiment==11,]
aggregate(fungi_11$egg_fungi_cum, list(fungi_11$Treatment), FUN=mean)
aggregate(fungi_11$egg_fungi_cum, list(fungi_11$Treatment), FUN=sd)

av_fungi_15<-aov(egg_fungi_cum~Treatment, data=Auskopplung_Data[Auskopplung_Data$day_experiment==15,])
summary(av_fungi_15)#Unterschied zwischen den Treatments
TukeyHSD(av_fungi_15,which="Treatment")#Unterschiede bei: paddle_strong_sediment vs paddle_light UND sediment vs paddle_strong_sediment
boxplot(egg_fungi_cum~Treatment, data=Auskopplung_Data[Auskopplung_Data$day_experiment==15,], range=0)
fungi_15<-Auskopplung_Data[Auskopplung_Data$day_experiment==15,]
aggregate(fungi_15$egg_fungi_cum, list(fungi_15$Treatment), FUN=mean)
aggregate(fungi_15$egg_fungi_cum, list(fungi_15$Treatment), FUN=sd)

#4.1 Control
Control<-Auskopplung_Data[Auskopplung_Data$Treatment=="control", ] #Alle Zeilen mit control-Treatment
Control_means_f<-aggregate(Control$egg_fungi_cum, list(Control$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle control-Treatment

#4.2 Sediment
Sediment<-Auskopplung_Data[Auskopplung_Data$T_Sediment=="Y", ] #Alle Zeilen mit Sediment-Treatment
Sediment_means_f<-aggregate(Sediment$egg_fungi_cum, list(Sediment$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Sediment Treatments

#4.2.1 Sediment
Sediment_only<-Auskopplung_Data[Auskopplung_Data$Treatment=="sediment", ] 
Sediment_only_means_f<-aggregate(Sediment_only$egg_fungi_cum, list(Sediment_only$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle reinen Sediment-Treatment

#4.3 Paddel light
Paddel_L<-Auskopplung_Data[Auskopplung_Data$T_Paddel=="L", ] #Alle Zeilen mit Paddel-"l"-Treatment
Paddel_L_means_f<-aggregate(Paddel_L$egg_fungi_cum, list(Paddel_L$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"l"-Treatment

#4.3.1 Paddel light ohne Sediment
Paddel_L_only<-Auskopplung_Data[Auskopplung_Data$Treatment=="paddle_light", ] 
Paddel_L_only_means_f<-aggregate(Paddel_L_only$egg_fungi_cum, list(Paddel_L_only$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"l"-Treatment

#4.4 Paddel strong
Paddel_S<-Auskopplung_Data[Auskopplung_Data$T_Paddel=="S", ] #Alle Zeilen mit Paddel-"s"-Treatment
Paddel_S_means_f<-aggregate(Paddel_S$egg_fungi_cum, list(Paddel_S$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"s"-Treatment

#4.4.1 Paddel strong ohne Sediment
Paddel_S_only<-Auskopplung_Data[Auskopplung_Data$Treatment=="paddle_strong", ]
Paddel_S_only_means_f<-aggregate(Paddel_S_only$egg_fungi_cum, list(Paddel_S_only$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"s"-Treatment

#4.5.1 Überkreuzung 1 - Paddel light mit Sediment
Paddel_L_Sediment<-Auskopplung_Data[Auskopplung_Data$Treatment=="paddle_light_sediment", ] 
Paddel_L_Sediment_means_f<-aggregate(Paddel_L_Sediment$egg_fungi_cum, list(Paddel_L_Sediment$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"l"-Treatment

#4.5.2 Überkreuzung 2 - Paddel strong mit Sediment
Paddel_S_Sediment<-Auskopplung_Data[Auskopplung_Data$Treatment=="paddle_strong_sediment", ]
Paddel_S_Sediment_means_f<-aggregate(Paddel_S_Sediment$egg_fungi_cum, list(Paddel_S_Sediment$day_experiment), FUN=mean, na.rm=TRUE) #Mitterlwerte pro Tag für alle Paddel-"s"-Treatment

#4.6.1 Kombinationen der Graphen
#Kombination der means-Treatment-Graphen ohne Überkreuzungen
All_Treatment_only_plot_f<-ggplot(data=Control_means_f, aes(x=Group.1, y=x)) +
  geom_point() +
  scale_x_continuous(name="day of experiment") +
  scale_y_continuous(name="accumulative number fungi") +
  geom_line()
All_Treatment_only_plot_f<-All_Treatment_only_plot_f +
  geom_point(data=Sediment_only_means_f, color="dark green") +
  geom_line(data=Sediment_only_means_f, color="dark green")
All_Treatment_only_plot_f<-All_Treatment_only_plot_f +
  geom_point(data=Paddel_L_only_means_f, color="yellow") +
  geom_line(data=Paddel_L_only_means_f, color="yellow")
All_Treatment_only_plot_f<-All_Treatment_only_plot_f +
  geom_point(data=Paddel_S_only_means_f, color="blue") +
  geom_line(data=Paddel_S_only_means_f, color="blue")
All_Treatment_only_plot_f<-All_Treatment_only_plot_f +
  geom_point(data=Paddel_L_Sediment_means_f, color="orange") +
  geom_line(data=Paddel_L_Sediment_means_f, color="orange")
All_Treatment_only_plot_f<-All_Treatment_only_plot_f +
  geom_point(data=Paddel_S_Sediment_means_f, color="dark turquoise") +
  geom_line(data=Paddel_S_Sediment_means_f, color="dark turquoise")
All_Treatment_only_plot_f #eventuell noch Fehlerbalken dran?. Dann noch "zoom" zu passenden boxplot (Zeile 64)
###ENDE Fungi###

library(ggplot2)

###weiterer code von Manuel
#wichtig: kombi-Plot plus 2 boxplots.
#wie viele Tiere kommen durch? kein sig U zwischen Treat ments.
#nach Tag 12 kann man Ergebnissen nicht merh trauen (Accidents, aber methoden um zu rektifizieren).
#Experiment wiederholen? Wir sehen keinen Effekt, weder an Tag 11 noch an Tag 14.

#Anova oder lm  für Larvenüberleben. 11 weil letzter Tag vor Acciednets und 15/16 weil wir das schon haben.) 
#Verpilzun Grafik wie für Lebenede Larven
#Analyse Anzahl Eier nicht machen
#-> 4 anovas mit tukay, 2 Grafiken mit Farbgebung und Anzeige was gleich ist

#Boxplot: ...,range=0)

