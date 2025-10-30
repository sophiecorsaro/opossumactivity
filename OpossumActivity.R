# import CSV
data <- images_2006181

data <- read_csv("C:/Users/sophi/Downloads/images_2006181.csv")

# load packages
library(tidyverse)
library(stringr)

# Data wrangling----------------------------------------------------------------
# split deployment_id into camera, treatment, and site
data$cam.pos<-sapply(str_split(data$deployment_id, "_", n =3), `[`, 3)
data$treatment<-sapply(str_split(data$deployment_id, "_", n =3), `[`, 2)
data$site<-sapply(str_split(data$deployment_id, "_", n =3), `[`, 1)

# subset to remove blanks, "No CV", and humans, subset to mammals and upper cameras----
mammals <- filter(data, data$common_name != "Blank", data$common_name != "No CV Result",
                  data$common_name != "Human", class == "Mammalia", cam.pos == "upper")

# this will only be used if you want to look at the urbanization gradient in addition to the removal and control treatment
Urban <- c("Arboretum", "Pheasant Branch", "FEC", "Havenwoods", "Greenfield Park", "Potters Forest")
Rural <- c("Cherokee Marsh", "New Glarus", "Duffin", "Mukwonago", "Ulrickson", "La Grange")
mammals$habitat <- ifelse(mammals$site %in% Urban, "Urban",
                          ifelse(mammals$site %in% Rural, "Rural",
                                 "Agricultural"))

# change site names from wildlife insights to official site names
# first, create lookup table of site names
site.lookup<-data.frame(photo.site=c("Arboretum","Capital Springs", "Cherokee Marsh", "Duffin", "Potters Forest", "FEC", "Renak-Polak", "Sauk Prairie",
                                     "Ulrickson", "Pinewoods", "Pheasant Branch", "New Glarus", "Mukwonago", "Greenfield Park", "Havenwoods", "La Grange"),
                        site.names=c("UW-Arboretum", "Capital Springs State Park", "Cherokee Marsh Conservation Area", "KMSU-Duffin Rd", "Potters Forest" ,
                                     "Forest Exploration Center", "Renak-Polak State Natural Area", "Sauk Prairie State Recreation Area", "KMSU-Ulrickson Rd",
                                     "KMSU-Pinewoods", "Pheasant Branch Conservancy", "New Glarus Woods", "KM-Mukwonago River Unit", "Greenfield Park",
                                     "Havenwoods State Forest", "KMSU-La Grange"))

# second, conditionally change site names using match()
inds <- match(mammals$site, site.lookup$photo.site)
mammals$site[!is.na(inds)] <- site.lookup$site.names[na.omit(inds)]

# make a df with counts of each species for each plot
mam.count.plot <- mammals %>% count (site, common_name, habitat, treatment, sort = TRUE)

# Convert to df of coyote/meso observations for each camera (or change for site/plot)
mammal.wide <- pivot_wider(mam.count.plot, names_from = common_name,
                           values_from = n, values_fill = 0)
# meso.wide <- pivot_wider(meso.count, names_from = common_name, values_from = n, values_fill = 0)
opossum <- subset(mammal.wide, select = c("site", "habitat", "treatment", "Virginia Opossum"))

mod <- lm(`Virginia Opossum`~treatment,data=opossum)
summary(mod)

tmp.removal <- filter(opossum, treatment == "removal")
mean.removal <- mean(tmp.removal$`Virginia Opossum`)
mean.removal

tmp.control <- filter(opossum, treatment == "control")
mean.control <- mean(tmp.control$`Virginia Opossum`)
mean.control

t.test(`Virginia Opossum`~treatment, data = opossum, paired = TRUE)

ggplot(data = opossum) +
  geom_boxplot(mapping = aes(x = treatment, y = `Virginia Opossum`, fill = treatment)) + labs(x = "Invasive Shrub Manipulation", y = "Virginia Opossum Detections")

library(car)
library(glmmTMB)

opossum.context <- glmmTMB(`Virginia Opossum`~habitat + treatment + (1|site), data = opossum, family = nbinom2())
summary(opossum.context)
Anova(opossum.context, type = 3)

# figure 1
ggplot(data = opossum) +   geom_boxplot(mapping = aes(x = treatment, y = `Virginia Opossum`, fill = treatment)) + labs(x = "Invasive shrub manipulation", y = "Opossum Detections") + guides(fill=guide_legend(title="Plot"))
#figure 2
ggplot(data = opossum) +   geom_boxplot(mapping = aes(x = habitat, y = `Virginia Opossum`, fill = treatment))+ labs(x = "Invasive shrub manipulation", y = "Opossum Detections") + guides(fill=guide_legend(title="Plot"))

