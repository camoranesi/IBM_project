#IBM_weight####
setwd("C:/C.F.Data/IBM")
library(ggplot2)
library(ggpubr)
library(dplyr)
library(data.table)
library(agricolae)

IBM_nature_dryweight <- read.csv("C:/C.F.Data/IBM/IBM_nature_dryweight.csv") %>% as.data.frame()

IBM_nature_dryweight_noamf <- filter(IBM_nature_dryweight, AMF == "no_amf")

IBM_nature_dryweight_amf <- filter(IBM_nature_dryweight, AMF =="amf") 

HSD_shoot_noamf <- HSD.test(aov(Shoot_dry_weight ~ Treatment,data=IBM_nature_dryweight_noamf), "Treatment", group=T)

HSD_shoot_amf <- HSD.test(aov(Shoot_dry_weight ~ Treatment,data=IBM_nature_dryweight_amf), "Treatment", group=T)

HSD_shoot_noamf_df <- as.data.frame(HSD_shoot_noamf$groups) %>% 
  setDT(keep.rownames = TRUE) %>% 
  setnames(1, "SampleID") %>%
  mutate(AMF="no_amf")

HSD_shoot_amf_df <- as.data.frame(HSD_shoot_amf$groups) %>% 
  setDT(keep.rownames = TRUE) %>% 
  setnames(1, "SampleID") %>%
  mutate(AMF="amf")

HSD_shoot <- rbind(HSD_shoot_noamf_df, HSD_shoot_amf_df)

#Shoot
IBM_barplot <- ggplot(IBM_nature_dryweight, aes(Treatment, Shoot_dry_weight)) +
  geom_boxplot(aes(fill = Treatment)) +
  geom_jitter(aes(color = Treatment), position=position_jitter(width=.1, height=0), 
              size=1.5, alpha=0.3, stroke = 1.3) +
  geom_text(data=HSD_shoot, aes(x=SampleID, y=0.7, label= HSD_shoot$groups)) +
  facet_grid(.~ AMF) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,size = 10, face = "bold"), axis.ticks.x=element_blank(), 
        strip.text.x = element_text(face = "bold", size = 10), 
        strip.background.x = element_blank(),
        strip.text.y = element_text(face = "bold", size = 16),
        legend.text=element_text(size=12)) + 
  ylab("Shoot dry weight (g)") +
  xlab("")  +
  stat_compare_means(method = "anova", label.y = 0.8)       # Add global annova p-value
#  stat_compare_means(label = "p.signif", method = "t.test",
#                     ref.group = ".all.")               

IBM_barplot

ggsave(paste( "IBM_barplot_shoot_weight.pdf", sep=""), IBM_barplot, width = 15, height = 10)

ggsave(paste( "IBM_barplot_shoot_weight.jpg", sep=""), IBM_barplot, width = 15, height = 10)

ggsave(paste( "IBM_barplot_shoot_weight_HSD.pdf", sep=""), IBM_barplot, width = 15, height = 10)

ggsave(paste( "IBM_barplot_shoot_weight_HSD.jpg", sep=""), IBM_barplot, width = 15, height = 10)

#Root
HSD_root_noamf <- HSD.test(aov(Root_dry_weight ~ Treatment,data=IBM_nature_dryweight_noamf), "Treatment", group=T)

HSD_root_amf <- HSD.test(aov(Root_dry_weight ~ Treatment,data=IBM_nature_dryweight_amf), "Treatment", group=T)

HSD_root_noamf_df <- as.data.frame(HSD_root_noamf$groups) %>% 
  setDT(keep.rownames = TRUE) %>% 
  setnames(1, "SampleID") %>%
  mutate(AMF="no_amf")

HSD_root_amf_df <- as.data.frame(HSD_root_amf$groups) %>% 
  setDT(keep.rownames = TRUE) %>% 
  setnames(1, "SampleID") %>%
  mutate(AMF="amf")

HSD_root <- rbind(HSD_root_noamf_df, HSD_root_amf_df)

IBM_barplot <- ggplot(IBM_nature_dryweight, aes(Treatment, Root_dry_weight)) +
  geom_boxplot(aes(fill = Treatment)) +
  geom_jitter(aes(color = Treatment), position=position_jitter(width=.1, height=0), 
              size=1.5, alpha=0.3, stroke = 1.3) +
  geom_text(data=HSD_root, aes(x= SampleID, y=0.6,label= HSD_root$groups)) +
  facet_grid(.~ AMF) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,size = 10, face = "bold"), axis.ticks.x=element_blank(), 
        strip.text.x = element_text(face = "bold", size = 10), 
        strip.background.x = element_blank(),
        strip.text.y = element_text(face = "bold", size = 16),
        legend.text=element_text(size=12)) + 
  ylab("Root dry weight (g)") +
  xlab("")  +
  stat_compare_means(method = "anova", label.y = 0.8)
# Add global annova p-value
#  stat_compare_means(label = "p.signif", method = "t.test",
#                     ref.group = ".all.")               

IBM_barplot

ggsave(paste( "IBM_barplot_root_weight.pdf", sep=""), IBM_barplot, width = 15, height = 10)

ggsave(paste( "IBM_barplot_root_weight.jpg", sep=""), IBM_barplot, width = 15, height = 10)

ggsave(paste( "IBM_barplot_root_weight_HSD.pdf", sep=""), IBM_barplot, width = 15, height = 10)

ggsave(paste( "IBM_barplot_root_weight_HSD.jpg", sep=""), IBM_barplot, width = 15, height = 10)

