

####Vorbereitung####

setwd("C:/Users/Lulu/Documents/Studium/Master/Sommersemester 2024/angewandte Vegetationsökologie Frauenschuh-Orchidee/statistische Auswertung")
library(openxlsx)
library(permute)
library(lattice)
library(vegan)

####Vegetationsanalyse####

veg_data <- read.xlsx("Vegetationsdaten.xlsx", sheet = "gesamt", sep= ";", rowNames =TRUE)
summary(veg_data)

environmental_data <- read.xlsx("data.anova.2024.xlsx", sheet = "env_data", sep = ";", rowNames = TRUE)
summary(environmental_data)

dca_result <- decorana(veg_data)
plot(dca_result)
summary(dca_result)
#axis lengths between 3.5 and 4.7 -> DCA possible

env_fit <- envfit(dca_result, environmental_data, perm = 999)
print(env_fit)


#####Plot####

DCA_scores <- scores(dca_result, display = "sites")
DCA1 <- DCA_scores[, 1] 
DCA2 <- DCA_scores[, 2]  


col <- c(rep("forestgreen", 20), rep("gold", 20), rep("firebrick", 20), rep("darkblue", 20))


plot(DCA1, DCA2, col = col, pch = 16, xlim = c(-2.5, 5.0), ylim = c(-3.0, 3.0), xlab = "DCA Achse 1", ylab = "DCA Achse 2")

plot(env_fit, p.max = 0.05, col = "black")


draw_transparent_hull <- function(x, y, color) {
  hull_indices <- chull(x, y) 
  hull_indices <- c(hull_indices, hull_indices[1]) 
  polygon(x[hull_indices], y[hull_indices], col = color, border = NA) 
}

# Gruppen mit transparenten Farben hinterlegen
draw_transparent_hull(DCA1[1:20], DCA2[1:20], rgb(34/255, 139/255, 34/255, alpha = 0.3)) 
draw_transparent_hull(DCA1[21:40], DCA2[21:40], rgb(255/255, 215/255, 0/255, alpha = 0.3)) 
draw_transparent_hull(DCA1[41:60], DCA2[41:60], rgb(178/255, 34/255, 34/255, alpha = 0.3)) 
draw_transparent_hull(DCA1[61:80], DCA2[61:80], rgb(0/255, 0/255, 139/255, alpha = 0.3))  

# Legende hinzufügen
legend("bottomright", legend = c("Buche", "Fichte", "Kiefer (entb.)", "Kiefer (n. entb.)"),
       col = c("firebrick", "darkblue", "gold", "forestgreen"), pch = 16, cex = 1.0)
