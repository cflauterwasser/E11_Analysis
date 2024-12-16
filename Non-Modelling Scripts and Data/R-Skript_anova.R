

####Vorbereitung####

  setwd("C:/Users/Lulu/Documents/Studium/Master/Sommersemester 2024/angewandte Vegetationsökologie Frauenschuh-Orchidee/statistische Auswertung")
  
  library(openxlsx)
  library(FSA)
  data_anova <- read.xlsx("data.anova.2024.xlsx", sheet = "Tabelle1", sep= ";")
  summary(data_anova)
  data_anova$Location <- as.factor(data_anova$Location)
  summary(data_anova)
  
  
  
  ####Anova zum Vergleichen der Bodenfeuchten####
  
  M1_Bodenfeuchte <-aov(soilwater_mean~Location, data = data_anova)
  summary(M1_Bodenfeuchte) # p < 0.01, das bedeuted die verschiedenen sites unterscheiden sich signifikant in ihrer Bodenfeuchte
  
  qqnorm(M1_Bodenfeuchte$residuals)
  qqline(M1_Bodenfeuchte$residuals) #Daten sind annähernd normalverteilt
  
  #post hoc test
  TukeyHSD(M1_Bodenfeuchte)
  #Buche und Kiefer nicht entbuscht(a) ; Fichte und Kiefer entbuscht (b)
  
  boxplot_1 <-boxplot(soilwater_mean~Location, data =data_anova, col = c("firebrick", "darkblue", "gold", "forestgreen"), xlab = "Fläche", ylab = "mittlere Bodenfeuchte [%]", ylim=c(10,50), cex.lab = 1.5, cex.axis = 1.4)
  
  text(x = 1, y = 45, labels = "a", cex = 1.5)
  text(x = 2, y = 45, labels = "b", cex = 1.5)
  text(x = 3, y = 45, labels = "b", cex = 1.5)
  text(x = 4, y = 45, labels = "a", cex = 1.5)
  
  
  
  
  ####Anova zum Vergleichen der Bodentiefe####
  
  M2_Bodentiefe <-aov(soildepth_mean~Location, data = data_anova)
  summary(M2_Bodentiefe) # p < 0.01, das bedeuted die verschiedenen sites unterscheiden sich signifikant in ihrer Bodentiefe, (p = 0.00521)
  
  
  qqnorm(M2_Bodentiefe$residuals)
  qqline(M2_Bodentiefe$residuals) #Daten sind annähernd normalverteilt
  
  TukeyHSD(M2_Bodentiefe)
  #nur Kiefer nicht entbuscht und Buche unterscheiden sich signifikant voneinander
  
  boxplot_2 <- boxplot(soildepth_mean~Location, data =data_anova,col = c("firebrick", "darkblue", "gold", "forestgreen"), ylab = "mittlere Bodentiefe [cm]", ylim=c(5,45),cex.lab = 1.5, cex.axis = 1.4)
  
  text(x = 1, y = 42, labels = "a", cex = 1.5)
  text(x = 2, y = 42, labels = "ab", cex = 1.5)
  text(x = 3, y = 42, labels = "ab", cex = 1.5)
  text(x = 4, y = 42, labels = "b", cex = 1.5)
  
  
  ####Anova zum vergleichen der Lichtverfügbarkeit (ratio.PAR)####
  
  M3_Lichtverfügbarkeit <-aov(ratio.PAR~Location, data = data_anova)
  summary(M3_Lichtverfügbarkeit) # p < 0.01, das bedeuted die verschiedenen sites unterscheiden sich signifikant in ihrer Lichtverfügbarkeit
  
  qqnorm(M3_Lichtverfügbarkeit$residuals)
  qqline(M3_Lichtverfügbarkeit$residuals) 
  #Residuen sind nicht normalverteilt
  
  M3_Lichtverfügbarkeit <- kruskal.test(ratio.PAR ~ Location, data = data_anova)
  M3_Lichtverfügbarkeit #p<0.05
  
  PostHoc <- dunnTest(ratio.PAR ~ Location, data = data_anova)
  #dunn.test als post hoc test, weil die Residuen nicht normalverteilt sind
  
  print(PostHoc, dunn.test.results = TRUE)
  
  boxplot_3 <- boxplot(ratio.PAR~Location, data =data_anova, col = c("firebrick", "darkblue", "gold", "forestgreen"), ylab = "PAR [%]", ylim=c(0,100), cex.lab = 1.5, cex.axis = 1.4)
  
  text(x = 1, y = 95, labels = "a", cex = 1.5)
  text(x = 2, y = 95, labels = "b", cex = 1.5)
  text(x = 3, y = 95, labels = "a", cex = 1.5)
  text(x = 4, y = 95, labels = "c", cex = 1.5)
  
  
  ####Anova zum Vergleichen der Exposition####
  
  M4_Exposition <-aov(Exposition~Location, data = data_anova)
  summary(M4_Exposition) 
  # p < 0.01, das bedeuted die verschiedenen sites unterscheiden sich signifikant in ihrer Exposition
  
  qqnorm(M4_Exposition$residuals)
  qqline(M4_Exposition$residuals) 
  #normalverteilt
  
  TukeyHSD(M4_Exposition)
  #die Buchenfläche unterscheidet sich in ihrer Exposition signifikant von den anderen Flächen
 
  boxplot_4 <- boxplot(Exposition~Location, data =data_anova,col = c("firebrick", "darkblue", "gold", "forestgreen"), ylab = "Exposition [°]", ylim=c(120,350), cex.lab = 1.5, cex.axis = 1.4)
  
  
  text(x = 1, y = 350, labels = "a", cex = 1.5)
  text(x = 2, y = 350, labels = "b", cex = 1.5)
  text(x = 3, y = 350, labels = "b", cex = 1.5)
  text(x = 4, y = 350, labels = "b", cex = 1.5)
  
  ####Anova zum Vergleichen der Hangneigung (Slope)####
  #in %
  #Variabilität (großer boxplot) vermutlich durch Unebenheiten im Gelände
  
  M5_Hangneigung <-aov(Slope~Location, data = data_anova)
  summary(M5_Hangneigung) 
  # p < 0.01, das bedeuted die verschiedenen sites unterscheiden sich signifikant in ihrer Exposition
  
  qqnorm(M5_Hangneigung$residuals)
  qqline(M5_Hangneigung$residuals) 
  #Residuen sind normalverteilt
  
  TukeyHSD(M5_Hangneigung)
  #alle Flächen unterscheiden sich signifikant voneinander
  
  boxplot_5 <- boxplot(Slope~Location, data =data_anova, col = c("firebrick", "darkblue", "gold", "forestgreen"), ylab = "Hangneigung [%]", ylim=c(10,80), cex.lab = 1.5, cex.axis = 1.4)
  
  text(x = 1, y = 80, labels = "a", cex = 1.5)
  text(x = 2, y = 80, labels = "b", cex = 1.5)
  text(x = 3, y = 80, labels = "c", cex = 1.5)
  text(x = 4, y = 80, labels = "d", cex = 1.5)
  
