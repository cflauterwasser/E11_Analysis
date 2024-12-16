#___________________________________________________________________________________
#### Importing Data####
library(openxlsx)
library(MASS)
library(plotrix)
library(agricolae)
library(patchwork)
library(tidyverse)
library(magrittr)
library(performance)
# make some boxplots
# read in the data
ind_data <- read.xlsx("datalike2019_.xlsx", sheet = 1)
str(ind_data)



#___________________________________________________________________________________
#### Data Preparation ####

ind_data <- as.tibble(ind_data)

ind_data <- ind_data |> 
  mutate(leaf.area = l.leaves*w.leaves)


ind_data$management2 <- factor(ind_data$management2, levels = c("Wald", "Hang", "Buche", "Fichte"), labels = c("Ki.n.entb.", "Ki. entb.", "Buche", "Fichte"))
ind_data

# differences between environmental variables
names(ind_data)
# area.bunch from cm² to m²
ind_data$area.bunch <- ind_data$area.bunch / 10000



#___________________________________________________________________________________
#### Violin Plot ####



# Perform pairwise Wilcoxon tests
wilcox_results <- pairwise.wilcox.test(ind_data$prop.flower, ind_data$management2, p.adjust.method = "holm")

# Calculate the maximum value of prop.flower
max_y <- max(ind_data$prop.flower)


# HIER EINFACH NUR UNTER "LABEL" DIE BUCHSTABEN EINTRAGEN, DIE EURE TESTS ERGEBEN!
letters_df <- data.frame(
  management2 = c("Ki.n.entb.", "Ki. entb.", "Buche", "Fichte"),
  y_position = rep(max_y + 0.05, 4),  # Adjust y_position based on your data
  label = c("a", "b", "a", "ab")  # Adjust labels based on significant differences
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
  labs(x = "Fläche",
       y = "Anteil blühender Sprosse",
       title = "Blüherfolg") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("forestgreen","gold", "firebrick", "darkblue" )) +
  scale_color_manual(values = c("forestgreen","gold", "firebrick", "darkblue")) +
  geom_text(data = letters_df, aes(x = management2, y = y_position, label = label), 
            position = position_dodge(0.9), size = 3, color = "black")  # Set color to black

# Print the plot
print(plot)




plot

# save file
ggsave(filename = "violin_prop.flower~management_2024.png", plot = plot, width = 7.4, height = 7.4, units = "cm")




