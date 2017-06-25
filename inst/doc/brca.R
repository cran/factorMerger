## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", warning = FALSE, message = FALSE,
                      fig.height = 5, fig.width = 10)

## ------------------------------------------------------------------------
library(factorMerger)
library(forcats)
library(survival)

## ------------------------------------------------------------------------
data("BRCA")

## ------------------------------------------------------------------------
brcaSurv <- Surv(time = BRCA$times, event = BRCA$patient.vital_status)

## ------------------------------------------------------------------------
drugName <- BRCA$patient.drugs.drug.drug_name # drug name
drugName <- fct_lump(as.factor(drugName), prop = 0.05) 


## ------------------------------------------------------------------------
drugNameFM <- mergeFactors(brcaSurv[!is.na(drugName)], 
                           drugName[!is.na(drugName)], 
                           family = "survival")

plot(drugNameFM, nodesSpacing = "effects", gicPanelColor = "grey2")

## ------------------------------------------------------------------------
anova(coxph(brcaSurv[!is.na(drugName)]~drugName[!is.na(drugName)]))
anova(coxph(brcaSurv[!is.na(drugName)]~cutTree(drugNameFM)))

## ------------------------------------------------------------------------
subtype <- BRCA$patient.clinical_cqcf.histological_type
subtype <- fct_lump(as.factor(subtype), prop = 0.05) 

subtypeFM <- mergeFactors(brcaSurv[!is.na(subtype)], 
                          subtype[!is.na(subtype)],
                           family = "survival")

plot(subtypeFM) 

## ------------------------------------------------------------------------
patCat <- BRCA$patient.stage_event.tnm_categories.pathologic_categories.pathologic_t %>% substr(1, 2)

patCatFM <- mergeFactors(brcaSurv[!is.na(patCat)],
                         patCat[!is.na(patCat)],
                         family = "survival")

plot(patCatFM, responsePanel = "frequency", gicPanelColor = "red")

## ------------------------------------------------------------------------
anova(coxph(brcaSurv[!is.na(patCat)]~patCat[!is.na(patCat)]))
anova(coxph(brcaSurv[!is.na(subtype)]~cutTree(subtypeFM)))

