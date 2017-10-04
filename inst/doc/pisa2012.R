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
pisa2012 %>% ggplot(aes(x = country)) + geom_bar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ------------------------------------------------------------------------
meltedPisa <- pisa2012 %>% melt(na.rm = TRUE)
pisaResultsBySubject <-  meltedPisa %>% 
    ggplot(aes(x = reorder(country, value, FUN = median), y = value)) + geom_boxplot() + 
    facet_wrap(~variable) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    xlab("Country") 


## ------------------------------------------------------------------------
pisaResultsBySubject + 
    geom_hline(data = meltedPisa %>% group_by(variable) %>% summarise(mean = mean(value)), 
               aes(yintercept = mean, group = variable), col = "red")

## ------------------------------------------------------------------------
manova(cbind(math, reading, science) ~ country, pisa2012) %>% summary()

## ------------------------------------------------------------------------
pisaIdxSubset <- sample(1:nrow(pisa2012), size = 500)
pisaFM <- mergeFactors(pisa2012[pisaIdxSubset, 1:3],
                       factor(pisa2012$country[pisaIdxSubset]))

plot(pisaFM, responsePanel = "profile")

## ---- eval = FALSE-------------------------------------------------------
#  pisaIdxSubset <- which(pisa2012$country %in% c("Belgium",
#                                             "Netherlands",
#                                             "Poland",
#                                             "Germany",
#                                             "Finland",
#                                             "Estonia"))
#  pisaFMHClust <- mergeFactors(pisa2012[pisaIdxSubset, 1:3],
#                         factor(pisa2012$country[pisaIdxSubset]),
#                         method = "fast-fixed")
#  
#  plot(pisaFMHClust, responsePanel = "profile",
#       penalty = log(NROW(pisaFMHClustMath$factor)),
#       panel = "response", nodesSpacing = "effects",
#       panelGrid = F, palette = "Dark2",
#       title = "PISA 2012 - students' performance")
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  pisaEuropean <- filter(pisa2012, country %in% c("Austria", "Belgium", "Bulgaria",
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
#                         factor(pisaEuropean$country),
#                         method = "fast-fixed")
#  
#  plot(pisaFMHClustEurope)
#  

