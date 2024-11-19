library(corrplot)
library(openxlsx)

ind_data <- read.xlsx("datalike2019.xlsx", sheet = 1)
str(ind_data)
summary(ind_data)
ind_data$management2 <- factor(ind_data$management2)

subset <- subset(ind_data, select=c(nb.stem, stem.per.sqm, prop.flower, area.bunch, nb.flower, stem.height, exposition, slope, soil_depth, soil_water, PAR, HL_cover, SL_cover, soil_cover, moss_cover, TL_cover, vh.max, vh.90, leaf.area))
cormatrix <- cor(subset)
corpvalues <- cor.mtest(subset)

corrplot(cormatrix, p.mat=corpvalues$p, type="upper", method="number")
corrplot(cormatrix, p.mat=corpvalues$p, type="upper", method="circle")

