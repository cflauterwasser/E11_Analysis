#___________________________________________________________________________________________________
#### Loading Packages ####

library(openxlsx)
library(MASS)
library(plotrix)
library(agricolae)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(magrittr)



#___________________________________________________________________________________________________
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

# plot
#model_final1 <- model_final

#model_final
plot_reg.lines <- function(model_final) {
  expl.vars <- data.frame(orig.name = colnames(model_final$model), stringsAsFactors = FALSE)
  expl.vars$new.name <- result.tab$env.names[match(expl.vars$orig.name, result.tab$parameter)]
  
  if (any(sapply(model_final$model, class) == "factor")) {
    factor.vars <- colnames(model_final$model)[which(sapply(model_final$model, class) == "factor")]
    expl.vars <- expl.vars[(expl.vars$orig.name != factor.vars), ]
  }
  plots_reg.lines <- list()
  for (i in 2:dim(expl.vars)[1]) { # i=2
    plots_reg.lines[[i-1]] <- ggplot(data = ind_data, aes_string(y = expl.vars$orig.name[1], x = expl.vars$orig.name[i])) + 
      geom_point(colour = "black", alpha = 0.25, shape = 20) +
      geom_smooth(method = MASS::glm.nb, formula = y ~ x, color = "darkgreen", se = TRUE, fill = "green", alpha = 0.35) + 
      labs(x = expl.vars$new.name[i], y = expl.vars$new.name[1]) +
      theme(panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA),
            plot.margin = unit(c(0, 0.05, 0.2, 0), "cm"),
            legend.position = "none")
  }
  #### ploting
  if (length(plots_reg.lines) == 1) design <- "1#"
  if (length(plots_reg.lines) == 2) design <- "12"
  if (length(plots_reg.lines) == 3) design <- "12
                                               3#"
  if (length(plots_reg.lines) == 4) design <- "12
                                               34"
  if (length(plots_reg.lines) == 4) design <- "12
                                               34
                                               5#"
  if (length(plots_reg.lines) == 4) design <- "12
                                               34
                                               56"
  plot_reg.lines <- wrap_plots(plots_reg.lines, design = design)
  file_name <- paste0("Plots/regline_", expl.vars$orig.name[1], ".png")
  if (length(plots_reg.lines) == 1 | length(plots_reg.lines) == 2)
    ggsave(filename = file_name, plot = plot_reg.lines, width = 14.8, height = 7.4, units = "cm")
  if (length(plots_reg.lines) == 3 | length(plots_reg.lines) == 4)
    ggsave(filename = file_name, plot = plot_reg.lines, width = 14.8, height = 14.8, units = "cm")
  if (length(plots_reg.lines) == 5 | length(plots_reg.lines) == 6)
    ggsave(filename = file_name, plot = plot_reg.lines, width = 14.8, height = 22.2, units = "cm")
}
#AI: "This function generates scatter plots with regression lines for each explanatory variable in a model. It uses ggplot2 to create the plots and saves them as PNG files. The function:
#Extracts variable names from the model.
#Creates scatter plots for each variable against a dependent variable.
#Fits a negative binomial regression line to the data.
#Saves the combined plots in a specified format based on the number of plots."


#model_final single plots
pplot_reg.lines.single.plots <- function(model_final) {
  expl.vars <- data.frame(orig.name = colnames(model_final$model), stringsAsFactors = FALSE)
  expl.vars$new.name <- result.tab$env.names[match(expl.vars$orig.name, result.tab$parameter)]
  
  if (any(sapply(model_final$model, class) == "factor")) {
    factor.vars <- colnames(model_final$model)[which(sapply(model_final$model, class) == "factor")]
    expl.vars <- expl.vars[(expl.vars$orig.name != factor.vars), ]
  }
  for (i in 2:dim(expl.vars)[1]) { # i=2
    plots_reg.lines.single <- ggplot(data = ind_data, aes_string(y = expl.vars$orig.name[1], x = expl.vars$orig.name[i])) + 
      geom_point(colour = "black", alpha = 0.25, shape = 16, size = 0.6) +
      geom_smooth(method = MASS::glm.nb, formula = y ~ x, color = "darkgreen", se = TRUE, fill = "green", alpha = 0.35) + 
      labs(x = expl.vars$new.name[i], y = expl.vars$new.name[1]) +
      theme(panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA),
            plot.margin = unit(c(0, 0.05, 0.2, 0), "cm"),
            legend.position = "none")
    #### ploting
    file_name <- paste0("Plots/regline_", expl.vars$orig.name[1], "_", expl.vars$orig.name[i], ".png")
    ggsave(filename = file_name, plot = plots_reg.lines.single, width = 7.4, height = 7.4, units = "cm")
  }
}
#AI: "Similar to plot_reg.lines, this function creates individual scatter plots with regression lines for each explanatory variable against the dependent variable and saves them as separate PNG files." 


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



#___________________________________________________________________________________________________
#### Importing Data####

# make some boxplots
# read in the data
ind_data <- read.xlsx("data_2019.xlsx", sheet = 1)
str(ind_data)



#___________________________________________________________________________________________________
#### Data Preparation ####

ind_data$management2 <- factor(ind_data$management2, levels = c("CC", "5yr", "beech_forest", "spruce_forest"), labels = c("Kiefer (n. entb.)", "Kiefer (entb.)", "Buche", "Fichte"))

# differences between environmental variables
names(ind_data)
# area.bunch from cm2 to m2
ind_data$area.bunch <- ind_data$area.bunch / 10000




#___________________________________________________________________________________
#### _________________________ ####
#### RESULT TABLE, BOXPLOTS ####




#___________________________________________________________________________________________________
#### Calculating Means and Standard Errors ####

means <- aggregate(. ~ management2, data = ind_data[, c(6:15, 17:27)], mean)
se <- aggregate(. ~ management2, data = ind_data[, c(6:15, 17:27)], std.error)



#___________________________________________________________________________________________________
#### Creating Results Table ####

result.tab <- data.frame(matrix(NA, nrow = 20, ncol = 13))
rownames(result.tab) <- colnames(ind_data[, c(6:15, 18:27)])
colnames(result.tab) <- c("parameter", levels(ind_data$management2), "F.val", "df", "P.val", "Signif", paste0(levels(ind_data$management2), ".letter"))



#___________________________________________________________________________________________________
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



#___________________________________________________________________________________________________
#### > Naming Environmental Variables ####

result.tab$env.names <- c("Sprossanzahl pro Horst", "Sprossanzahl pro m² Horstgröße", "Horstgröße [m²]", "Anzahl blühender Sprosse", "Anteil blühender Sprosse [%]", "Sprosshöhe [cm]", "FvFm", "PI", "SLA", "LDMC", "Exposition [°]", "Neigung [%]", "Bodentiefe [cm]", "Bodenfeuchte nach Regen [%]", "Bodenfeuchte nach Trockenheit [%]", "PAR [%]", "Deckung Krautschicht [%]", "Deckung Strauchschicht [%]", "Deckung offener Boden [%]", "Deckung Moosschicht [%]")
result.tab



#___________________________________________________________________________________________________
#### > Writing Results to Excel Sheet ####

write.xlsx(result.tab, "Result Tables/results.diff.2019.xlsx")



#___________________________________________________________________________________________________
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
    geom_boxplot(fill = c("forestgreen", "gold", "firebrick", "darkblue")) +
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
  file_name <- paste0("Plots/boxplot_", temp.x, "_2019.png")
  ggsave(filename = file_name, plot = p, width = 7.4, height = 7.4, units = "cm")
}



#___________________________________________________________________________________________________
#### Model - Stem Count Per Patch (nb.stem) ####


### Welche Umweltparamter erklären die Populationseigenschaften?

# nb.stem = Sprossanzahl pro Horst
lm2 <- glm(nb.stem ~ management2 + slope + soil_depth + soil_water2 + PAR + HL_cover + SL_cover + soil_cover + moss_cover, data = ind_data, family = poisson)
summary(lm2)

# besser glm.nb
lm3 <- glm.nb(nb.stem ~ management2 + slope + soil_depth + soil_water2 + PAR + HL_cover + SL_cover + soil_cover + moss_cover, data = ind_data)
summary(lm3)

lm4 <- stepAIC(lm3)
summary(lm4)

lm5 <- update(lm4, .~. - soil_cover)
anova(lm4, lm5)
summary(lm5)
#
lm6 <- update(lm5, .~. + management2)
anova(lm5, lm6)
summary(lm6)
#
model_final <- lm5
#plot_reg.lines(model_final)
pplot_reg.lines.single.plots(model_final)
#
#
write_model_table(model.result = model_final, file.name = "Model.result.tables_2019.xlsx")
#



#___________________________________________________________________________________________________
#### Model - Patch Size  (area.bunch) ####

# area.bunch = Horstgröße [m2]
result.tab
lm2 <- lm(log(area.bunch) ~ management2 + slope + soil_depth + soil_water1 + soil_water2 + PAR + HL_cover + SL_cover + soil_cover + moss_cover, data = ind_data)
#plot(lm2)
summary(lm2)

lm3 <- stepAIC(lm2)
summary(lm3)

lm4 <- update(lm3, .~. -moss_cover)
anova(lm4, lm3)
summary(lm4)
#
lm5 <- update(lm4, .~. -HL_cover)
anova(lm4, lm5)
summary(lm5)
model_final <- lm5
#
write_model_table(model.result = model_final, file.name = "Model.result.tables_2019.xlsx")
#
#



#___________________________________________________________________________________________________
#### Model - Proportion Flowering (prop.flower) ####

# prop.flower = Anteil blühender Sprosse [%]
ind_data$prop.flower <- ind_data$prop.flower / 100
lm2 <- lm(car::logit(prop.flower, adjust = 0.0001) ~ management2 + slope + soil_depth + soil_water1 + soil_water2 + PAR + HL_cover + SL_cover + moss_cover, data = ind_data)
#plot(lm2)
summary(lm2)

lm3 <- stepAIC(lm2)
summary(lm3)

lm4 <- update(lm3, .~. -soil_water1)
anova(lm4, lm3)
summary(lm4)
#
model_final <- lm4
write_model_table(model.result = model_final, file.name = "Model.result.tables_2019.xlsx")

# plot 
expl.vars <- data.frame(orig.name = colnames(model_final$model), stringsAsFactors = FALSE)
expl.vars$new.name <- result.tab$env.names[match(expl.vars$orig.name, result.tab$parameter)]
expl.vars$orig.name1 <- expl.vars$orig.name
expl.vars$orig.name[1] <- "prop.flower"

if (any(sapply(model_final$model, class) == "factor")) {
  factor.vars <- colnames(model_final$model)[which(sapply(model_final$model, class) == "factor")]
  expl.vars <- expl.vars[(expl.vars$orig.name != factor.vars), ]
}



#___________________________________________________________________________________________________
#### > Plotting - prop.flower ~ PAR ####

# plot PAR
lm.PAR <- lm(car::logit(prop.flower, adjust = 0.0001) ~ PAR, data = ind_data)
range.x <- range(ind_data$PAR)
# df with predictions, lower and upper limits of CIs: 
new.x <- data.frame(PAR = seq(from = range.x[1], to = range.x[2], 0.1))
new.y <- predict(lm.PAR,
                 newdata = new.x,
                   se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(PAR = seq(from = range.x[1], to = range.x[2], 0.1), 
         # model object mod1 has a component called linkinv that 
         # is a function that inverts the link function of the GLM:
         lower = invlogit(fit - 1.96*se.fit), 
         point.estimate = invlogit(fit), 
         upper = invlogit(fit + 1.96*se.fit)) 


# plotting with ggplot: 
plots_reg.lines.single <- ggplot(data = ind_data, aes_string(y = "prop.flower", x = "PAR")) +
  geom_point(colour = "black", alpha = 0.25, shape = 16, size = 0.6) +
  geom_line(aes(x = PAR, y = point.estimate), data = new.y, colour = "darkgreen") + 
  geom_ribbon(aes(x = PAR, ymin = lower, ymax = upper), data = new.y, color = NA, fill = "green", alpha = 0.35, inherit.aes = FALSE) + 
  labs(x = "PAR [%]", y = "Anteil blühender Sprosse [%]") +
  scale_y_continuous(labels = seq(0, 100, 25)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0, 0.05, 0.2, 0), "cm"),
        legend.position = "none")
#### ploting
file_name <- paste0("Plots/regline_", expl.vars$orig.name[1], "_PAR_2019.png")
ggsave(filename = file_name, plot = plots_reg.lines.single, width = 7.4, height = 7.4, units = "cm")
#
#
#



#___________________________________________________________________________________________________
#### Model - Proportion Flowering -> Pine Plots ####

# prop.flower = Anteil blühender Sprosse [%]
# hier jetzt nur für die beiden Kiefernflächen!

ind_data$prop.flower <- ind_data$prop.flower / 100
ind_data.kiefer <- subset(ind_data, management2 == "Kiefer (entb.)" | management2 == "Kiefer (n. entb.)")

lm2 <- lm(car::logit(prop.flower, adjust = 0.0001) ~ management2 + slope + soil_depth + soil_water1 + soil_water2 + PAR + HL_cover + SL_cover + moss_cover, data = ind_data.kiefer)
#plot(lm2)
summary(lm2)

lm3 <- stepAIC(lm2)
summary(lm3)

lm4 <- update(lm3, .~. - PAR)
anova(lm4, lm3)
summary(lm4)
#
model_final <- lm4
#write_model_table(model.result = model_final, file.name = "Model.result.tables_2019.xlsx")

# plot 
expl.vars <- data.frame(orig.name = colnames(model_final$model), stringsAsFactors = FALSE)
expl.vars$new.name <- result.tab$env.names[match(expl.vars$orig.name, result.tab$parameter)]
expl.vars$orig.name1 <- expl.vars$orig.name
expl.vars$orig.name[1] <- "prop.flower"

if (any(sapply(model_final$model, class) == "factor")) {
  factor.vars <- colnames(model_final$model)[which(sapply(model_final$model, class) == "factor")]
  expl.vars <- expl.vars[(expl.vars$orig.name != factor.vars), ]
}



#___________________________________________________________________________________________________
#### > Plotting - prop.flower ~ SL_cover ####

# plot SL_cover
lm.SL_cover <- lm(car::logit(prop.flower, adjust = 0.0001) ~ SL_cover, data = ind_data.kiefer)
range.x <- range(ind_data$SL_cover)
# df with predictions, lower and upper limits of CIs: 
new.x <- data.frame(SL_cover = seq(from = range.x[1], to = range.x[2], 0.1))
new.y <- predict(lm.SL_cover,
                 newdata = new.x,
                 se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(SL_cover = seq(from = range.x[1], to = range.x[2], 0.1), 
         # model object mod1 has a component called linkinv that 
         # is a function that inverts the link function of the GLM:
         lower = invlogit(fit - 1.96*se.fit), 
         point.estimate = invlogit(fit), 
         upper = invlogit(fit + 1.96*se.fit)) 


# plotting with ggplot: 
plots_reg.lines.single <- ggplot(data = ind_data.kiefer, aes_string(y = "prop.flower", x = "SL_cover")) +
  geom_point(colour = "black", alpha = 0.25, shape = 16, size = 0.6) +
  geom_line(aes(x = SL_cover, y = point.estimate), data = new.y, colour = "darkgreen") + 
  geom_ribbon(aes(x = SL_cover, ymin = lower, ymax = upper), data = new.y, color = NA, fill = "green", alpha = 0.35, inherit.aes = FALSE) + 
  labs(x = "SL_cover [%]", y = "Anteil blühender Sprosse [%]") +
  scale_y_continuous(labels = seq(0, 100, 25)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0, 0.05, 0.2, 0), "cm"),
        legend.position = "none")
#### ploting
file_name <- paste0("Plots/regline_", expl.vars$orig.name[1], "_Ki.SL_cover_2019.png")
ggsave(filename = file_name, plot = plots_reg.lines.single, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________________________
#### Correlation Test - PAR ~ SL_cover ####

################### cor PAR SL cover
cor.test(ind_data.kiefer$PAR, ind_data.kiefer$SL_cover)



#___________________________________________________________________________________________________
#### > Plotting - PAR ~ SL_cover ####

# plot SL_cover
lm.SL_cover_PAR <- lm(PAR ~ SL_cover, data = ind_data.kiefer)
range.x <- range(ind_data.kiefer$SL_cover)
# df with predictions, lower and upper limits of CIs: 
new.x <- data.frame(SL_cover = seq(from = range.x[1], to = range.x[2], 0.1))
new.y <- predict(lm.SL_cover_PAR,
                 newdata = new.x,
                 se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(SL_cover = seq(from = range.x[1], to = range.x[2], 0.1), 
         # model object mod1 has a component called linkinv that 
         # is a function that inverts the link function of the GLM:
         lower = (fit - 1.96*se.fit), 
         point.estimate = fit, 
         upper = (fit + 1.96*se.fit)) 
# plotting with ggplot: 
plots_reg.lines.single <- ggplot(data = ind_data.kiefer, aes_string(y = "PAR", x = "SL_cover")) +
  geom_point(colour = "black", alpha = 0.25, shape = 16, size = 0.6) +
  geom_line(aes(x = SL_cover, y = point.estimate), data = new.y, colour = "darkgreen") + 
  geom_ribbon(aes(x = SL_cover, ymin = lower, ymax = upper), data = new.y, color = NA, fill = "green", alpha = 0.35, inherit.aes = FALSE) + 
  labs(x = "Deckung Strauchschicht [%]", y = "PAR [%]") +
  scale_y_continuous(labels = seq(10, 40, 10)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0, 0.05, 0.2, 0), "cm"),
        legend.position = "none")
#### ploting
file_name <- paste0("Plots/regline_", expl.vars$orig.name[1], "_Ki.SL_cover_PAR_2019.png")
ggsave(filename = file_name, plot = plots_reg.lines.single, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________________________
#### Model - Stem Height (stem.height)  ####

# stem.height = Sprosshöhe [cm]
result.tab
lm2 <- lm(log(stem.height) ~ management2 + slope + soil_depth + soil_water2 + PAR + HL_cover + SL_cover + moss_cover, data = ind_data)
#plot(lm2)
summary(lm2)

lm3 <- stepAIC(lm2)
summary(lm3)

lm4 <- update(lm3, .~. -slope)
anova(lm4, lm3)
summary(lm4)

lm5 <- update(lm4, .~. -PAR)
anova(lm4, lm5)
summary(lm5)
#
model_final <- lm5
write_model_table(model.result = model_final, file.name = "Model.result.tables_2019.xlsx")
#plot(model_final) stem.height = Sprosshöhe [cm]
# plot 
expl.vars <- data.frame(orig.name = colnames(model_final$model), stringsAsFactors = FALSE)
expl.vars$new.name <- result.tab$env.names[match(expl.vars$orig.name, result.tab$parameter)]
expl.vars$orig.name1 <- expl.vars$orig.name
expl.vars$orig.name[1] <- "stem.height"

if (any(sapply(model_final$model, class) == "factor")) {
  factor.vars <- colnames(model_final$model)[which(sapply(model_final$model, class) == "factor")]
  expl.vars <- expl.vars[(expl.vars$orig.name != factor.vars), ]
}



#___________________________________________________________________________________________________
#### > Plotting - stem.height ~ soil_water  ####

# plot PAR
lm.soil_water1 <- lm(log(stem.height) ~ soil_water1, data = ind_data)
range.x <- range(ind_data$soil_water1)
# df with predictions, lower and upper limits of CIs: 
new.x <- data.frame(soil_water1 = seq(from = range.x[1], to = range.x[2], 0.1))
new.y <- predict(lm.soil_water1,
                 newdata = new.x,
                 se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(soil_water1 = seq(from = range.x[1], to = range.x[2], 0.1), 
         # model object mod1 has a component called linkinv that 
         # is a function that inverts the link function of the GLM:
         lower = exp(fit - 1.96*se.fit), 
         point.estimate = exp(fit), 
         upper = exp(fit + 1.96*se.fit)) 

# plotting with ggplot: 
plots_reg.lines.single <- ggplot(data = ind_data, aes_string(y = "stem.height", x = "soil_water1")) +
  geom_point(colour = "black", alpha = 0.25, shape = 16, size = 0.6) +
  geom_line(aes(x = soil_water1, y = point.estimate), data = new.y, colour = "darkgreen") + 
  geom_ribbon(aes(x = soil_water1, ymin = lower, ymax = upper), data = new.y, color = NA, fill = "green", alpha = 0.35, inherit.aes = FALSE) + 
  labs(x = "Bodenfeuchte nach Regen [%]", y = "Sprosshöhe [cm]") +
#  scale_y_continuous(labels = seq(0, 100, 25)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0, 0.05, 0.2, 0), "cm"),
        legend.position = "none")
#### ploting
file_name <- paste0("Plots/regline_", expl.vars$orig.name[1], "_soil_water1_2019.png")
ggsave(filename = file_name, plot = plots_reg.lines.single, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________________________
#### Model - Specific Leaf Area (SLA) ####

# SLA = SLA
result.tab
lm2 <- lm(SLA ~ management2 + slope + soil_depth + soil_water2 + PAR + HL_cover + SL_cover + moss_cover, data = ind_data)
#plot(lm2)
summary(lm2)

lm3 <- stepAIC(lm2)
summary(lm3)
#
lm4 <- update(lm3, .~. -moss_cover)
anova(lm4, lm3)
summary(lm4)
#
model_final <- lm4
write_model_table(model.result = model_final, file.name = "Model.result.tables_2019.xlsx")
#plot(model_final)
#plot(model_final) SLA = SLA
# plot 
expl.vars <- data.frame(orig.name = colnames(model_final$model), stringsAsFactors = FALSE)
expl.vars$new.name <- result.tab$env.names[match(expl.vars$orig.name, result.tab$parameter)]
expl.vars$orig.name1 <- expl.vars$orig.name
expl.vars$orig.name[1] <- "SLA"

if (any(sapply(model_final$model, class) == "factor")) {
  factor.vars <- colnames(model_final$model)[which(sapply(model_final$model, class) == "factor")]
  expl.vars <- expl.vars[(expl.vars$orig.name != factor.vars), ]
}



#___________________________________________________________________________________________________
#### > Plotting - SLA ~ PAR ####

# plot PAR
lm.PAR <- lm((SLA) ~ PAR, data = ind_data)
range.x <- range(ind_data$PAR)
# df with predictions, lower and upper limits of CIs: 
new.x <- data.frame(PAR = seq(from = range.x[1], to = range.x[2], 0.1))
new.y <- predict(lm.PAR,
                 newdata = new.x,
                 se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(PAR = seq(from = range.x[1], to = range.x[2], 0.1), 
         # model object mod1 has a component called linkinv that 
         # is a function that inverts the link function of the GLM:
         lower = (fit - 1.96*se.fit), 
         point.estimate = (fit), 
         upper = (fit + 1.96*se.fit)) 

# plotting with ggplot: 
plots_reg.lines.single <- ggplot(data = ind_data, aes_string(y = "SLA", x = "PAR")) +
  geom_point(colour = "black", alpha = 0.25, shape = 16, size = 0.6) +
  geom_line(aes(x = PAR, y = point.estimate), data = new.y, colour = "darkgreen") + 
  geom_ribbon(aes(x = PAR, ymin = lower, ymax = upper), data = new.y, color = NA, fill = "green", alpha = 0.35, inherit.aes = FALSE) + 
  labs(x = "PAR [%]", y = "SLA") +
  #  scale_y_continuous(labels = seq(0, 100, 25)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0, 0.05, 0.2, 0), "cm"),
        legend.position = "none")
#### ploting
file_name <- paste0("Plots/regline_", expl.vars$orig.name[1], "_PAR_2019.png")
ggsave(filename = file_name, plot = plots_reg.lines.single, width = 7.4, height = 7.4, units = "cm")



#___________________________________________________________________________________________________
#### Model - Leaf Dry Matter Content (LDMC) ####

# LDMC = LDMC
result.tab
lm2 <- lm(LDMC ~ management2 + slope + soil_depth + soil_water1 + soil_water2 + PAR + HL_cover + SL_cover + moss_cover, data = ind_data)
#plot(lm2)
summary(lm2)

lm3 <- stepAIC(lm2)
summary(lm3)
#
lm4 <- update(lm3, .~. -soil_water2)
anova(lm4, lm3)
summary(lm4)
#
model_final <- lm4
write_model_table(model.result = model_final, file.name = "Model.result.tables_2019.xlsx")
#plot(model_final)
#
#plot(model_final) LDMC = LDMC
# plot 
expl.vars <- data.frame(orig.name = colnames(model_final$model), stringsAsFactors = FALSE)
expl.vars$new.name <- result.tab$env.names[match(expl.vars$orig.name, result.tab$parameter)]
expl.vars$orig.name1 <- expl.vars$orig.name
expl.vars$orig.name[1] <- "LDMC"

if (any(sapply(model_final$model, class) == "factor")) {
  factor.vars <- colnames(model_final$model)[which(sapply(model_final$model, class) == "factor")]
  expl.vars <- expl.vars[(expl.vars$orig.name != factor.vars), ]
}



#___________________________________________________________________________________________________
#### > Plotting - LDMC ~ PAR ####

# plot soil_water1
lm.PAR <- lm((LDMC) ~ PAR, data = ind_data)
range.x <- range(ind_data$PAR)
# df with predictions, lower and upper limits of CIs: 
new.x <- data.frame(PAR = seq(from = range.x[1], to = range.x[2], 0.1))
new.y <- predict(lm.PAR,
                 newdata = new.x,
                 se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(PAR = seq(from = range.x[1], to = range.x[2], 0.1), 
         # model object mod1 has a component called linkinv that 
         # is a function that inverts the link function of the GLM:
         lower = (fit - 1.96*se.fit), 
         point.estimate = (fit), 
         upper = (fit + 1.96*se.fit)) 

# plotting with ggplot: 
plots_reg.lines.single <- ggplot(data = ind_data, aes_string(y = "LDMC", x = "PAR")) +
  geom_point(colour = "black", alpha = 0.25, shape = 16, size = 0.6) +
  geom_line(aes(x = PAR, y = point.estimate), data = new.y, colour = "darkgreen") + 
  geom_ribbon(aes(x = PAR, ymin = lower, ymax = upper), data = new.y, color = NA, fill = "green", alpha = 0.35, inherit.aes = FALSE) + 
  labs(x = "PAR [%]", y = "LDMC") +
  #  scale_y_continuous(labels = seq(0, 100, 25)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.margin = unit(c(0, 0.05, 0.2, 0), "cm"),
        legend.position = "none")
#### ploting
file_name <- paste0("Plots/regline_", expl.vars$orig.name[1], "_PAR_2019.png")
ggsave(filename = file_name, plot = plots_reg.lines.single, width = 7.4, height = 7.4, units = "cm")
#




#moving the model result table, since directly saving it in the folder created problems
file.rename("Model.result.tables_2019.xlsx", "Result Tables/Model.result.tables_2019.xlsx")

