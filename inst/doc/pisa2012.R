## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", warning = FALSE, message = FALSE,
                      fig.height = 5, fig.width = 10)

## ------------------------------------------------------------------------
library(factorMerger)
library(ggplot2)
library(dplyr)
library(reshape2)

## ------------------------------------------------------------------------
data("pisa2012")

## ------------------------------------------------------------------------
pisa2012 %>% ggplot(aes(x = CNT)) + geom_bar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ------------------------------------------------------------------------
meltedPisa <- pisa2012 %>% melt(na.rm = TRUE)
pisaResultsBySubject <-  meltedPisa %>% 
    ggplot(aes(x = reorder(CNT, value, FUN = median), y = value)) + geom_boxplot() + 
    facet_wrap(~variable) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab("Country") 

pisaResultsBySubject

## ------------------------------------------------------------------------
pisaResultsBySubject + 
    geom_hline(data = meltedPisa %>% group_by(variable) %>% summarise(mean = mean(value)), 
               aes(yintercept = mean, group = variable), col = "red")

## ------------------------------------------------------------------------
manova(cbind(PV1MATH, PV1READ, PV1SCIE) ~ CNT, pisa2012) %>% summary()

## ------------------------------------------------------------------------
pisaIdxSubset <- sample(1:nrow(pisa2012), size = 500)
pisaFM <- mergeFactors(pisa2012[pisaIdxSubset, 1:3],
                       factor(pisa2012$CNT[pisaIdxSubset]))

pisaFM
plot(pisaFM, responsePanel = "profile")

## ---- eval = FALSE-------------------------------------------------------
#  pisaFMHClustMath <- mergeFactors(pisa2012[, 1:3],
#                         factor(pisa2012$CNT),
#                         method = "hclust",
#                         successive = TRUE)
#  
#  plot(pisaFMHClustMath)
#  
#  pisaFMHClust <- mergeFactors(pisa2012[, 1:3],
#                         factor(pisa2012$CNT),
#                         method = "hclust",
#                         successive = FALSE)
#  
#  plot(pisaFMHClust)

## ---- eval = FALSE-------------------------------------------------------
#  
#  pisaEuropean <- filter(pisa2012, CNT %in% c("Austria", "Belgium", "Bulgaria",
#                                              "Czech Republic", "Germany", "Denmark",
#                                              "Spain", "Estonia", "Finland",
#                                              "France", "Hungary", "Ireland",
#                                              "Italy", "Netherlands", "Norway",
#                                              "Poland", "Portugal",
#                                              "Russian Federation", "Slovak Republic",
#                                              "Slovenia"))
#  
#  
#  pisaFMHClustEurope <- mergeFactors(pisaEuropean[,1:3],
#                         factor(pisaEuropean$CNT),
#                         method = "hclust",
#                         successive = TRUE)
#  
#  plot(pisaFMHClustEurope)
#  

