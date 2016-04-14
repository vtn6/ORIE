rm(list = ls())
setwd("~/Downloads")
ortho = read.table("hospital.csv", header=T, sep = ",")
ortho2 = ortho[,-c(1:4, 10:11, 14:16)]
ortho3 = ortho2[ortho2[,"SIR"]>0,]
# pairs(ortho3)

ortho4 = log(ortho3+1)

# pairs(ortho4)

orthoStandard = scale(ortho4)
res = prcomp(orthoStandard)