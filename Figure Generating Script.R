## 466 Report, Figure Generation
## Alexander Baumgaertner

library(tidyverse)
library(openxlsx)

dir <- "D:\\School Documents\\BIOL 466\\Data"
setwd(dir)

## Figure 1

qpcr <- read.xlsx("qpcr.xlsx", rowNames = T)
qpcr$avg <- rowMeans(qpcr)

bcd <- qpcr[1,4]
bca <- qpcr[2,4]
hld <- qpcr[3,4]
hla <- qpcr[4,4]

dct_bc <- bcd - bca
dct_hl <- hld - hla

ddct <- dct_bc - dct_hl

fc <- 2^-ddct

df <- data.frame(
  Sample = factor(c("Breast Cancer", "HeLa"), levels = c("HeLa", "Breast Cancer")),
  Expression = c(fc, 1) # relative expression
)


cols <- c("grey70", "black")

ggplot(df, aes(x=Sample, y = Expression, fill = Sample)) + 
  geom_bar(stat = "identity") +
  ylab("Relative Expression") +
  xlab("") +
  scale_fill_manual(values = cols) + 
  scale_y_continuous(limits = c(0, 50), breaks = c(seq(0, 50, 10))) +
  theme_classic() + 
  theme(legend.position = "none")

## Figure 3

bgal <- read.xlsx("bgal.xlsx", rowNames = T)

df_bgal <- data.frame(
  Interaction = factor(c(
    "DHFR (wt) + MDM2 (wt)",
    "DHFR (P149L) + MDM2 (wt)",
    "DHFR (wt) + MDM2 (RING)",
    "DHFR (wt) + Empty BD"),
    levels = c("DHFR (wt) + MDM2 (wt)",
               "DHFR (P149L) + MDM2 (wt)",
               "DHFR (wt) + MDM2 (RING)",
               "DHFR (wt) + Empty BD")),
  Activity <- bgal$units
)

cols <- c("grey35", "black", "grey70", "grey90")

ggplot(df_bgal, aes(x = Interaction, y = Activity, fill = Interaction)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = cols, labels = c("Positive Control", "Experiment", "Negative Control", "Autoactivation Control"),
                    name = "Function") + 
  xlab("") +
  ylab("Activity (Miller units)") +
  theme_classic() + 
  theme(legend.position = "bottom",
        legend.box.margin = margin(l = -30),
        axis.text.x = element_text(angle = 25, hjust = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

## Figure 4

dhfr <- read.xlsx("dhfr_aa.xlsx") 

dhfr_long <- dhfr %>%
  pivot_longer(
    cols = c("wt", "mtx", "cu466", "L22R.mut"),  
    names_to = "Treatment",                     
    values_to = "Absorbance"                      
  )

dhfr_long$Treatment <- factor(dhfr_long$Treatment,
                            levels = c("wt", "mtx", "cu466", "L22R.mut"),
                            labels = c("Wild Type", "MTX", "CU466", "L22R Mutant"))

cols <- c("grey65", "grey40", "black", "grey80")

ggplot(dhfr_long, aes(x = time, y = Absorbance, color = Treatment,
                    linetype = Treatment, group = Treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = cols) +
  ylab("Absorbance (340nm)") +
  xlab("Time (seconds)") + 
  scale_linetype_manual(values = c("longdash", "dotted", "solid", "dotdash")) +
  theme_classic() +
  theme(
    legend.position = "bottom",   
    legend.title = element_blank()
  ) 

