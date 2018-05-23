library(rsubgroup)

data("credit.data")
attach(credit.data)

result1 = DiscoverSubgroups(
  credit.data, as.target("class", "good"), new("SDTaskConfig",
  attributes=c("checking_status", "credit_amount", "employment", "purpose")))

result1 = ToDataFrame(result1)
result1

## p = accuracy, mide la frecuencia relativa de ejemplos que satisfacen 
##     la regla completa entre los que satisfacen solo el antecedente
## size = numero de observaciones que cumplen el antecedente


nocheck = subset(credit.data, credit.data$checking_status == "no checking")

nocheckTv = subset(credit.data, credit.data$checking_status == "no checking"
                   & credit.data$purpose == "radio/tv")

## Specificity:

table(credit.data$checking_status != "no checking" & credit.data$class == "good")
table(credit.data$checking_status != "no checking" & credit.data$class == "bad")
table(credit.data$checking_status == "no checking" & credit.data$class == "bad")

254 / (254 + 46)

result2 = DiscoverSubgroups(
  credit.data, as.target("class", "good"), new("SDTaskConfig",
  attributes = c("checking_status", "employment")))

result2 = ToDataFrame(result2)
