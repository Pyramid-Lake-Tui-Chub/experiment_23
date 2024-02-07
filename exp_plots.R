#### INSTALL PACKAGES ----
library(tidyverse)
library(plotrix)
library(RColorBrewer)
library(cowplot)
library(viridis)
library(ggh4x)
library(hrbrthemes)
library(ggplot2)

#### READ IN DATA ----
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\experiment_summary")

surv <- read.csv("exp_survival.csv")
grow <- read.csv("exp_growth.csv")

################################################################################
## DAILY SURVIVAL ##
surv$tx <- factor(surv$tx, levels=c("truckee", "pyramid", "t1", "t2", "t3"))
tx_labels <- as_labeller(c('truckee' = "Low TDS: 130 mg/L",'pyramid' = "Control: 5,888 mg/L", 't1' = "T1: 9,044 mg/L", 't2' = "T2: 12,222 mg/L",'t3' = "T3: 15,400 mg/L"))
rep_labels <- as_labeller(c('1' = "Replicate 1", '2' = "Replicate 2", '3' = "Replicate 3"))

my_colors <- RColorBrewer::brewer.pal(5, "Greys")[5:10]

raw_surv <- ggplot(surv, aes(x= day, y=con_alive, group=rep, fill=tx)) +
  geom_area(alpha = 0.8)+
  scale_fill_manual(values=my_colors) +
  theme(legend.position = "none") +
  labs(x="Day", y="Number Larvae Observed") +
  theme_bw() +
  theme(legend.position="none") +
  facet_grid(rows = vars(tx), 
             cols= vars(rep), 
             labeller = labeller(tx = tx_labels, 
                                 rep = rep_labels))
raw_surv

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "exp_surv_observed.png", units = "in", width = 8, height = 6, res=600)
raw_surv
dev.off()

## DAILY SURVIVAL W/AMMONIA ##
coef <- 13

raw_surv_nh3 <- ggplot(surv, aes(x= day, group=rep, fill=tx)) +
  geom_area(aes(y=con_alive))+
  geom_line(aes(y=nh3*coef), color = "brown3", size = 1)+
  scale_fill_manual(values=my_colors) +
  scale_y_continuous(name = "Number Larvae Observed", sec.axis = sec_axis(~./coef, name = "Total Ammonium (ppm)")) +
  theme(legend.position = "none") +
  labs(x="Day") +
  theme_bw() +
  theme(legend.position="none") +
  facet_grid(rows = vars(tx), 
             cols= vars(rep), 
             labeller = labeller(tx = tx_labels, 
                                 rep = rep_labels))
raw_surv_nh3

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "surv_nh3_color.png", units = "in", width = 8, height = 7, res=600)
raw_surv_nh3
dev.off()

################################################################################
## HATCH OUT PERCENT ##
surv$tx <- as.factor(surv$tx)
surv$con_alive <- as.integer(surv$con_alive)
surv <- surv %>% group_by(tx) %>% mutate(mean_hatch = mean(con_alive))

surv <- surv %>% mutate(tx_labels = case_when(tx == "truckee" ~ "Low TDS: 130 mg/L", 
                                              tx == "pyramid" ~ "Control: 5,888 mg/L", 
                                              tx == "t1" ~ "T1: 9,044 mg/L", 
                                              tx == "t2" ~ "T2: 12,222 mg/L",
                                              tx == "t3" ~ "T3: 15,400 mg/L"))

surv$tx_labels <-factor(surv$tx_labels, levels = c("Low TDS: 130 mg/L","Control: 5,888 mg/L", "T1: 9,044 mg/L", "T2: 12,222 mg/L", "T3: 15,400 mg/L"))

hatch_out <- ggplot(subset(surv, con_alive != 0), aes(x=tx_labels, y=con_alive, fill =tx)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.7) +
  geom_jitter(color = "grey29", size = 0.9, alpha = 0.8)+
  theme(legend.position = "none") +
  labs(x=" ", y="Number Larvae Observed")
hatch_out

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "hatch_out.png", units = "in", width = 8, height = 6, res=600)
hatch_out
dev.off()

################################################################################
## GROWTH ##
grow <- grow %>% mutate(tx_labels = case_when(tx == "truckee" ~ "Low TDS: 130 mg/L", 
                                              tx == "pyramid" ~ "Control: 5,888 mg/L"))

growth <- ggplot(grow, aes(x=tx_labels, y=tl, fill =tx)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.7, option = "E") +
  geom_jitter(color = "grey29", size = 0.9, alpha = 0.8)+
  theme(legend.position = "none") +
  labs(x=" ", y="Total Length (mm)")
growth

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "growth.png", units = "in", width = 8, height = 6, res=600)
growth
dev.off()
















