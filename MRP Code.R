rm(list=ls())

headers = as.data.frame(read.table(file="employment.csv", header=FALSE, sep=',', skip=8, nrows=1, as.is=TRUE))
df = read.table(file="employment.csv", header=FALSE, sep=',', skip=10, nrows=253)
colnames(df) = headers

df_both = df[,0:23]
df_male = cbind(df[,0],df[,24:45])
df_female = cbind(df[,0],df[,46:67])

