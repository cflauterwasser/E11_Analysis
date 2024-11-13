
#___________________________________________________________________________________
#### Loading Packages ####

library(openxlsx)
library(MASS)
library(plotrix)
library(agricolae)
library(patchwork)
library(tidyverse)
library(magrittr)
library(performance)



#___________________________________________________________________________________
#### Creating Own Functions ####

# own function
invlogit <- function(x) return(round(exp(x) / (1 + exp(x)), 3))
# x <- TukeyHSD.letters
#x$groups[1] <- "b"
#AI: "This function computes the inverse logit transformation, which is often used in logistic regression to convert log-odds to probabilities"

rename.letters <- function(x) {
  x$groups <- as.character(x$groups)
  if (grepl("a", x$groups[1])) {
    new.letters <- x$groups
  } else {
    which.letters <- unique(strsplit(paste(x$groups, collapse = ""), "")[[1]]) 
    which.letters <- which.letters[order(which.letters)]
    tab.translate <- data.frame(orig = unique(strsplit(paste(x$groups, collapse = ""), "")[[1]]), new = NA) 
    tab.translate$orig <- as.character(tab.translate$orig)
    tab.translate$new[1] <- "a"
    which.letters <- which.letters[-(which.letters %in% tab.translate$new[1])] 
    for (j in 2:dim(tab.translate)[1]) { # j=3
      if (!(tab.translate[j, "orig"] %in% which.letters)) {
        tab.translate[j, "new"] <- which.letters[order(which.letters)][1]
      } else {
        tab.translate[j, "new"] <- tab.translate[j, "orig"]
      }
      which.letters <- which.letters[-(which.letters %in% tab.translate$new[j])]
    }
    new.letters <- rep(NA, dim(x)[1])
    for (k in 1:dim(x)[1]) { # k = 1
      new.letters[which(x$groups %in% tab.translate$orig[k])] <- tab.translate$new[k]
    }
    if (any(is.na(new.letters))) {
      for (l in which(is.na(new.letters))) { # l=3
        multi.letter <- unique(strsplit(paste(x$groups[l], collapse = ""), "")[[1]]) 
        for (m in 1:length(multi.letter)) { # m = 1
          multi.letter[m] <- tab.translate$new[which(tab.translate$orig %in% multi.letter[m])]
        }
        multi.letter <- multi.letter[order(multi.letter)]
        new.letters[l] <- paste0(multi.letter, collapse = "")  
      }
    }
  }
  x$groups <- new.letters
  return(x)
}
# rename.letters(TukeyHSD.letters)
#AI: "This function renames groups of letters (likely from statistical test results) to ensure they are in a specific order. It checks if the first group contains "a" and renames accordingly, creating a mapping of original to new letters"



#model_final1 <- model_final
#
write_model_table <- function(model.result = NULL, file.name = NULL) {
  
  if (file.name %in% list.files()) {
    wb.model.tables <- loadWorkbook(xlsxFile = file.name)
  } else {
    wb.model.tables <- createWorkbook() 
  }
  
  dep_var <- names(attributes(summary(model.result)$term)$dataClasses[1])
  if (grepl(pattern = "(", dep_var, fixed = TRUE)) {
    start.p <- regexpr(pattern = "(", text = dep_var, fixed = TRUE)[1] + 1
    if (grepl(pattern = ",", dep_var, fixed = TRUE)) {
      end.p <- regexpr(pattern = ",", text = dep_var, fixed = TRUE)[1] - 1
    } else {
      end.p <- regexpr(pattern = ")", text = dep_var, fixed = TRUE)[1] - 1
    }
    dep_var <- str_sub(dep_var, start = start.p, end = end.p)
  }
  coef_table <- as.data.frame(summary(model.result)$coefficients)
  coef_table[, 1] <- round(coef_table[, 1], 3)
  coef_table[, 2] <- round(coef_table[, 2], 3)
  coef_table[, 3] <- round(coef_table[, 3], 3)
  coef_table[, 4] <- round(coef_table[, 4], 4)
  
  style_border <- createStyle(border = "TopBottomLeftRight ", borderStyle = "thin", borderColour = "black")
  style_header <- createStyle(border = "TopBottomLeftRight", fgFill = "lightgrey")
  
  if (length(names(wb.model.tables) != 0)) {
    if (dep_var %in% names(wb.model.tables))
      removeWorksheet(wb.model.tables, sheet = dep_var)
  }
  addWorksheet(wb.model.tables, sheetName = dep_var)
  
  writeData(wb.model.tables, sheet = dep_var, headerStyle = style_header, rowNames = TRUE, x = coef_table)
  addStyle(wb.model.tables, sheet = dep_var, style = style_border, rows = 1:(dim(coef_table)[1] + 1), cols = 1:(dim(coef_table)[2] + 1), stack = TRUE, gridExpand = TRUE)
  
  # save table with model results
  saveWorkbook(wb.model.tables, file = file.name, overwrite = TRUE)
}
#AI: "This function writes the coefficients of a statistical model to an Excel file. It:
#Checks if the specified file exists and loads it or creates a new workbook.
#Extracts the dependent variable name from the model summary.
#Creates a coefficient table and formats it.
#Writes the table to a new sheet in the Excel workbook and saves the workbook."






#___________________________________________________________________________________
#### Importing Data####

# make some boxplots
# read in the data
ind_data <- read.xlsx("datalike2019.xlsx", sheet = 1)
str(ind_data)



#___________________________________________________________________________________
#### Data Preparation ####

ind_data <- as.tibble(ind_data)

ind_data <- ind_data |> 
  mutate(leaf.area = l.leaves*w.leaves)


ind_data$management2 <- factor(ind_data$management2, levels = c("Wald", "Hang", "Buche", "Fichte"), labels = c("nicht entbuscht", "entbuscht", "Buche", "Fichte"))
ind_data

# differences between environmental variables
names(ind_data)
# area.bunch from cm² to m²
ind_data$area.bunch <- ind_data$area.bunch / 10000




#___________________________________________________________________________________
#### _________________________ ####
#### RESULT TABLE, BOXPLOTS ####




#___________________________________________________________________________________
#### Calculating Means and Standard Errors ####

means <- aggregate(. ~ management2, data = ind_data[, c(6:27)], mean)
se <- aggregate(. ~ management2, data = ind_data[, c(6:27)], std.error)



#___________________________________________________________________________________
#### Creating Results Table ####

result.tab <- data.frame(matrix(NA, nrow = 21, ncol = 13))
rownames(result.tab) <- colnames(ind_data[, c(6:13, 15:27)])
colnames(result.tab) <- c("parameter", levels(ind_data$management2), "F.val", "df", "P.val", "Signif", paste0(levels(ind_data$management2), ".letter"))


#___________________________________________________________________________________
#### ANOVA and Tukey's HSD Test ####

for (i in 1:dim(result.tab)[1]) { # i = 13
  temp <- rownames(result.tab)[i]
  mean.se <- paste0(format(round(means[, temp], 2), nsmall = 2), "±", format(round(se[, temp], 3), nsmall = 3))
  mean.se <- gsub(" ", "", mean.se)
  aov1 <- aov(as.formula(paste0(temp, " ~ management2")), data = ind_data) 
  anova1 <- summary(aov1)
  TukeyHSD.letters <- HSD.test(aov1, "management2", group = TRUE)
  TukeyHSD.letters <- TukeyHSD.letters$groups
  TukeyHSD.letters <- TukeyHSD.letters[order(match(rownames(TukeyHSD.letters), levels(ind_data$management2))),]
  
  
  TukeyHSD.letters <- rename.letters(TukeyHSD.letters)
  TukeyHSD.letters <- as.character(TukeyHSD.letters[, "groups"])
  degfr <- paste(anova1[[1]]$Df, collapse = " and ")
  fval <- format(round(anova1[[1]]$"F value"[1], 2), nsmall = 2)
  pval <- round(anova1[[1]]$"Pr(>F)"[1], digits = 4)
  symp <- symnum(pval, corr = FALSE,
                 cutpoints = c(0,  .001,.01,.05, 1),
                 symbols = c("***","**","*","n.s."))
  pval <- format.pval(pval, eps = .001, digits = 2)
  res_temp <- c(temp, mean.se, fval, degfr, pval, symp, TukeyHSD.letters)
  result.tab[i, ] <- res_temp
}



#___________________________________________________________________________________
#### > Naming Environmental Variables ####

result.tab$env.names <- c("Sprossanzahl pro Horst", "Sprossanzahl pro m² Horstgröße", "Horstgröße [m²]", "Anzahl blühender Sprosse", "Anteil blühender Sprosse [%]", "Sprosshöhe [cm]", "Blattlänge [cm]", "Blattbreite [cm]", "Exposition [°]", "Neigung [%]", "Bodentiefe [cm]", "Bodenfeuchte [%]", "Photosynthetisch aktive Strahlung [%]", "Deckung Krautschicht [%]", "Deckung Strauchschicht [%]", "Deckung Baumschicht [%]", "Deckung offener Boden [%]", "Deckung Moosschicht [%]", "Maximale Vegetationshöhe [cm]", "Höhe 90% der Vegetation [cm]","Blattfläche [cm²]")
result.tab



#___________________________________________________________________________________
#### > Writing Results to Excel Sheet ####

write.xlsx(result.tab, "Result Tables/results.diff.2024.xlsx")



#___________________________________________________________________________________
#### > Boxplots Env Vars ~ Management ####

#
for (i in 1:dim(result.tab)[1]) { # i = 11
  temp.x <- rownames(result.tab)[i]
  temp.y <- "management2"
  range.y <- range(ind_data[, temp.x], na.rm = TRUE)
  plus.temp <- diff(range.y) * 0.1
  range.y[1] <- range.y[1] * 0.9
  range.y[2] <- range.y[2] * 1.1
  max.y <- aggregate(as.formula(paste0(temp.x, " ~ management2")), data = ind_data, FUN = function(X) max(X, na.rm = TRUE))
  temp.env.name <- result.tab[i, "env.names"] 
  
  p <- ggplot(ind_data, aes_string(x = temp.y, y =  temp.x)) + 
    geom_boxplot(fill = c("firebrick", "gold", "darkblue", "forestgreen")) +
    labs(x = "
         ", y = temp.env.name) +
    ylim(range.y) +
    geom_text(x = 1, y = max.y[1, 2] + plus.temp, label = result.tab[i, 10]) +
    geom_text(x = 2, y = max.y[2, 2] + plus.temp, label = result.tab[i, 11]) +
    geom_text(x = 3, y = max.y[3, 2] + plus.temp, label = result.tab[i, 12]) +
    geom_text(x = 4, y = max.y[4, 2] + plus.temp, label = result.tab[i, 13]) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.title.y = element_text(),
          plot.margin = unit(c(0, 0.0, 0.2, 0), "cm")) 
  file_name <- paste0("Plots/boxplot_", temp.x, "_2024.png")
  ggsave(filename = file_name, plot = p, width = 7.4, height = 7.4, units = "cm")
}




#___________________________________________________________________________________
#### ___________ ####
#### MODELLING ####




#___________________________________________________________________________________
#### Available Dependent Variables ####

result.tab

#nb.stem       #done
#stem.per.sqm  #done
#area.bunch    #done
#nb.flower     #contained in prop.flower
#prop.flower   #done
#stem.height   #done
#leaf.area     #done


#___________________________________________________________________________________
#### Model - Stem Count Per Patch (nb.stem) ####

# nb.stem = Sprossanzahl pro Horst
lm1 <- glm(nb.stem ~ management2 + exposition + slope + soil_depth + soil_water + PAR + HL_cover + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data, family = poisson)
summary(lm1)

# besser glm.nb (edit 2024: hier auch!)
glm1 <- glm.nb(nb.stem ~ management2 + exposition + slope + soil_depth + soil_water + PAR + HL_cover + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data)
summary(glm1)

glm2 <- stepAIC(glm1)
summary(glm2)

glm3 <- update(glm2, .~. -soil_water)
anova(glm2, glm3)
summary(glm3)


model_final <- glm3


write_model_table(model.result = model_final, file.name = "Model.result.tables_2024.xlsx")


#check preconditions
par(mfrow = c(2, 2))
plot(model_final)
par(mfrow = c(1, 1))

check_model(model_final)



#___________________________________________________________________________________
#### > Plotting - nb.stem ~ exposition ####

plot <- ggplot(ind_data, aes(x = exposition, y = nb.stem)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "glm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Exposition [°]",
       y = "Sprossanzahl pro Horst") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_nb.stem~exposition_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### > Plotting - nb.stem ~ HL_cover ####

plot <- ggplot(ind_data, aes(x = HL_cover, y = nb.stem)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "glm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung Krautschicht [%]",
       y = "Sprossanzahl pro Horst") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_nb.stem~HL_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### > Plotting - nb.stem ~ TL_cover ####

plot <- ggplot(ind_data, aes(x = TL_cover, y = nb.stem)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "glm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung Baumschicht [%]",
       y = "Sprossanzahl pro Horst") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_nb.stem~TL_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### Model - Patch Size  (area.bunch) ####

# area.bunch = Horstgröße [m²]

lm1 <- lm(log(area.bunch) ~ management2 + exposition + slope + soil_depth + soil_water + PAR + HL_cover + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data)
#plot(lm2)
summary(lm1)

lm2 <- stepAIC(lm1)
summary(lm2)

lm3 <- update(lm2, .~. -TL_cover)
anova(lm2, lm3)
summary(lm3)

lm4 <- update(lm3, .~. -moss_cover)
anova(lm3, lm4)
summary(lm4)


model_final <- lm4


write_model_table(model.result = model_final, file.name = "Model.result.tables_2024.xlsx")


#check preconditions
par(mfrow = c(2, 2))
plot(model_final)
par(mfrow = c(1, 1))

check_model(model_final)



#___________________________________________________________________________________
#### > Plotting - log(area.bunch) ~ soil_depth ####

plot <- ggplot(ind_data, aes(x = soil_depth, y = log(area.bunch))) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Bodentiefe [cm]",
       y = "Horstgröße [m²]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_log(area.bunch)~soil_depth_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### > Plotting - log(area.bunch) ~ HL_cover ####

plot <- ggplot(ind_data, aes(x = HL_cover, y = log(area.bunch))) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung Krautschicht [%]",
       y = "Horstgröße [m²]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_log(area.bunch)~HL_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### Model - Proportion Flowering (prop.flower) ####

# prop.flower = Anteil blühender Sprosse [%]
ind_data$prop.flower <- ind_data$prop.flower / 100
lm1 <- lm(car::logit(prop.flower, adjust = 0.0001) ~ management2 + exposition + slope + soil_depth + soil_water + PAR + HL_cover + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data)

#plot(lm1)
summary(lm1)

lm2 <- stepAIC(lm1)
summary(lm2)

model_final <- lm2

write_model_table(model.result = model_final, file.name = "Model.result.tables_2024.xlsx")


#check preconditions
par(mfrow = c(2, 2))
plot(model_final) # difficult
par(mfrow = c(1, 1))

check_model(model_final) # difficult



#___________________________________________________________________________________
#### Model - Proportion Flowering - Markus' Alternative (cbind(nb.flower, nb.stem)) ####

# edit 2024: Alternative von Markus
glm1 <- glm(cbind(nb.flower, nb.stem) ~ management2 + exposition + slope + soil_depth + soil_water + PAR + HL_cover + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data, family = binomial)

summary(glm1)

glm2 <- stepAIC(glm1)
summary(glm2)

glm3 <- update(glm2, .~. -soil_depth)
anova(glm2, glm3)
summary(glm3)


model_final <- glm3

write_model_table(model.result = model_final, file.name = "Model.result.tables_2024.xlsx")


#check preconditions
par(mfrow = c(2, 2))
plot(model_final) # difficult
par(mfrow = c(1, 1))

check_model(model_final) # difficult


#___________________________________________________________________________________
#### > Plotting - prop.flower ~ management2 ####

plot <- ggplot(ind_data, aes(x = management2, y = prop.flower,
                             fill = management2,
                             col = management2)) +
  geom_violin(scale ="width",
              alpha =0.1) + 
  geom_jitter(aes(group = management2),
              width = 0.3,
              height = 0,
              alpha = 0.3) + 
  stat_summary(fun="median",
               geom="crossbar",
               mapping = aes(ymin=after_stat(y), ymax=after_stat(y)),
               width=1,
               position = position_dodge(),
               show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Management / Standort",
       y = "Anteil blühender Sprosse") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none") +
  scale_fill_manual(values = c("firebrick", "gold", "darkblue", "forestgreen")) +
  scale_color_manual(values = c("firebrick", "gold", "darkblue", "forestgreen"))
    



# Perform pairwise Wilcoxon tests
wilcox_results <- pairwise.wilcox.test(ind_data$prop.flower, ind_data$management2, p.adjust.method = "holm")

max_y <- max(ind_data$prop.flower)

letters_df <- data.frame(
  management2 = c("nicht entbuscht", "entbuscht", "Buche", "Fichte"),
  y_position = rep(max_y + 0.05, 4),
  label = c("a", "b", "ab", "b")
)


# Create the violin plot
plot <- ggplot(ind_data, aes(x = management2, y = prop.flower,
                             fill = management2,
                             col = management2)) +
  geom_violin(scale ="width",
              alpha =0.1) + 
  geom_jitter(aes(group = management2),
              width = 0.3,
              height = 0,
              alpha = 0.3) + 
  stat_summary(fun="median",
               geom="crossbar",
               mapping = aes(ymin=after_stat(y), ymax=after_stat(y)),
               width=1,
               position = position_dodge(),
               show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Management / Standort",
       y = "Anteil blühender Sprosse") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none") +
  scale_fill_manual(values = c("firebrick", "gold", "darkblue", "forestgreen")) +
  scale_color_manual(values = c("firebrick", "gold", "darkblue", "forestgreen")) +
  geom_text(data = letters_df, aes(x = management2, y = y_position, label = label), 
            position = position_dodge(0.9), size = 5, color = "black")  # Set color to black

# Print the plot
print(plot)




plot

# save file
ggsave(filename = "Plots/violin_prop.flower~management_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### > Plotting - prop.flower ~ soil_water ####

plot <- ggplot(ind_data, aes(x = soil_water, y = prop.flower)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Bodenfeuchte [%]",
       y = "Anteil blühender Sprosse [%]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_prop.flower~soil_water_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### > Plotting - prop.flower ~ HL_cover ####

plot <- ggplot(ind_data, aes(x = HL_cover, y = prop.flower)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung Krautschicht [%]",
       y = "Anteil blühender Sprosse [%]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_prop.flower~HL_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### Model - Stem Height (stem.height)  ####

# stem.height = Sprosshöhe [cm]

lm1 <- lm(log(stem.height) ~ management2 + exposition + slope + soil_depth + soil_water + PAR + HL_cover + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data)

#plot(lm1)
summary(lm1)

#lm2 <- stepAIC(lm1)
#summary(lm2)
#edit 2024: left out stepAIC to manually test if there is really nothing significant to model

lm2 <- update(lm1, .~. -soil_water)
anova(lm1, lm2)
summary(lm2)

lm3 <- update(lm2, .~. -soil_depth)
anova(lm2, lm3)
summary(lm3)

lm4 <- update(lm3, .~. -management2)
anova(lm4, lm3)
summary(lm4)

lm5 <- update(lm4, .~. -slope)
anova(lm4, lm5)
summary(lm5)

lm6 <- update(lm5, .~. -PAR)
anova(lm5, lm6)
summary(lm6)

lm7 <- update(lm6, .~. -exposition)
anova(lm6, lm7)
summary(lm7)

lm8 <- update(lm7, .~. -HL_cover)
anova(lm7, lm8)
summary(lm8)

lm9 <- update(lm8, .~. -vh.max)
anova(lm8, lm9)
summary(lm9)

lm10 <- update(lm9, .~. -vh.90)
anova(lm9, lm10)
summary(lm10)

lm11 <- update(lm10, .~. -SL_cover)
anova(lm10, lm11)
summary(lm11)

lm12 <- update(lm11, .~. -moss_cover)
anova(lm11, lm12)
summary(lm12)

lm13 <- update(lm12, .~. -TL_cover)
anova(lm12, lm13)
summary(lm13)

model_final <- lm13
write_model_table(model.result = model_final, file.name = "Model.result.tables_2024.xlsx")


#check preconditions
par(mfrow = c(2, 2))
plot(model_final) # strong patterns
par(mfrow = c(1, 1))

check_model(model_final)



#___________________________________________________________________________________
#### Not significant, not usable > Plotting - stem.height ~ soil_cover  ####

plot <- ggplot(ind_data, aes(x = soil_cover, y = stem.height)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung offener Boden [%]",
       y = "Sprosshöhe [cm]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_stem.height~soil_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### Model - Stem Density (stem.per.sqm)  ####

# new 2024

lm1 <- lm(stem.per.sqm ~ management2 + exposition + slope + soil_depth + soil_water + PAR + HL_cover + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data)
summary(lm1)

lm2 <- stepAIC(lm1)
summary(lm2)

lm3 <- update(lm2, .~. -PAR)
anova(lm2, lm3)
summary(lm3)

lm4 <- update(lm3, .~. -HL_cover)
anova(lm3, lm4)
summary(lm4)


model_final <- lm4
write_model_table(model.result = model_final, file.name = "Model.result.tables_2024.xlsx")


#check preconditions
par(mfrow = c(2, 2))
plot(model_final) # difficult
par(mfrow = c(1, 1))

check_model(model_final)



#___________________________________________________________________________________
#### > Plotting - stem.per.sqm ~ vh.90 ####

plot <- ggplot(ind_data, aes(x = vh.90, y = stem.per.sqm)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Höhe 90% der Vegetation [cm]",
       y = "Sprossanzahl pro m² Horstgröße") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file
ggsave(filename = "Plots/regline_stem.per.sqm~vh.90_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### Model - Leaf Area (leaf.area)  ####

# new 2024

lm1 <- lm(leaf.area ~ management2 + exposition + slope + soil_depth + soil_water + PAR + HL_cover + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data)
summary(lm1)

lm2 <- stepAIC(lm1)
summary(lm2)

lm3 <- update(lm2, .~. -exposition)
anova(lm2, lm3)
summary(lm3)


model_final <- lm3
write_model_table(model.result = model_final, file.name = "Model.result.tables_2024.xlsx")


#check preconditions
par(mfrow = c(2, 2))
plot(model_final) # patterns
par(mfrow = c(1, 1))

check_model(model_final)



#___________________________________________________________________________________
#### > Plotting - leaf.area ~ soil_cover ####

plot <- ggplot(ind_data, aes(x = soil_cover, y = leaf.area)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung offener Boden [%]",
       y = "Blattfläche [cm²]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file 
ggsave(filename = "Plots/regline_leaf.area~soil_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")




#___________________________________________________________________________________
#### Model - Herb Layer Cover (HL_cover)  ####

# herb layer correlated with lot of Cypripedium fitness traits, here deeper investigation of correlation of HL with other env variables

lm1 <- lm(HL_cover ~ management2 + exposition + slope + soil_depth + soil_water + PAR + SL_cover + TL_cover + soil_cover + moss_cover + vh.max + vh.90, data = ind_data)
summary(lm1)

lm2 <- stepAIC(lm1)
summary(lm2)

lm3 <- update(lm2, .~. -vh.max)
anova(lm2, lm3)
summary(lm3)


model_final <- lm3
write_model_table(model.result = model_final, file.name = "Model.result.tables_2024.xlsx")


#check preconditions
par(mfrow = c(2, 2))
plot(model_final) # qqline not good
par(mfrow = c(1, 1))

check_model(model_final)



#___________________________________________________________________________________
#### > Plotting - HL_cover ~ SL_cover ####

plot <- ggplot(ind_data, aes(x = SL_cover, y = HL_cover)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung Strauchschicht [%]",
       y = "Deckung Krautschicht [%]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file 
ggsave(filename = "Plots/regline_HL_cover~SL_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm") 



#___________________________________________________________________________________
#### > Plotting - HL_cover ~ soil_cover ####

plot <- ggplot(ind_data, aes(x = soil_cover, y = HL_cover)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung offener Boden [%]",
       y = "Deckung Krautschicht [%]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file 
ggsave(filename = "Plots/regline_HL_cover~soil_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________
#### > Plotting - HL_cover ~ moss_cover ####

plot <- ggplot(ind_data, aes(x = moss_cover, y = HL_cover)) +
  geom_point(colour = "black",
             alpha = 0.25,
             shape = 16,
             size = 0.6) +
  geom_smooth(method = "lm", formula= y~x, aes(group=1), color = "darkgreen", fill = "green", linetype = 1) +
  labs(x = "Deckung Moosschicht [%]",
       y = "Deckung Krautschicht [%]") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none")

plot

# save file 
ggsave(filename = "Plots/regline_HL_cover~moss_cover_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")



#moving the model result table, since directly saving it in the folder created problems
file.rename("Model.result.tables_2024.xlsx", "Result Tables/Model.result.tables_2024.xlsx")