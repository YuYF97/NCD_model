library(data.table)
library(bnlearn)
library(rlist)
library(base)
library(rlist)
library(MASS)
library(pROC)
library(MASS)
library(gtools)
library(dplyr)
library(plyr)

rm(list = ls())
gc()

test_example <- read.table(file = 'test_example.txt', sep = '\t', header = T)
bn_example <- readRDS('NCD_model.rds')

for (i in 1:ncol(test_example)) {
  test_example[,i] <- as.factor(test_example[,i])
}

myprocess <- function(x, vars_evidence, cfit){
  colnames(x) <- nodes(cfit)
  vars_evidence_nomi <- vars_evidence[which(is.na(x[vars_evidence])==F)]
  risk <- cpquery(cfit, 
                  event = (y=='1'),
                  evidence = as.list(x[vars_evidence_nomi]), method = "lw")
  return(risk)
}
risk_example <- c()
vars_evidence <- nodes(bn_example)[!nodes(bn_example) %in%　c("y","id_no","qh")]
for (i in 1:nrow(test_example)) {
  x <- test_example[i,]
  risk <- myprocess(x, vars_evidence = vars_evidence, cfit = bn_example)
  risk_example[i] <- risk
}
print(risk_example)
