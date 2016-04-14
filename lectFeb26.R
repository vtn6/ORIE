library(ISLR)
library(splines)
plot(Wage$age,Wage$wage)
Wage$age2 = Wage$age^2
Wage$age3 = Wage$age^3
Wage$age4 = Wage$age^4
wageFit = lm(formula = wage~age + age2 + age3 + age4, data = Wage)

#seq creates a vector of numerical values, from "from", to "to", at intervals of "by"
age.grid = seq( from = min(Wage$age),
                to = max(Wage$age),
                by = 1)

predDat = data.frame(age = age.grid, age2 = age.grid^2,age3 = age.grid^3,age4 = age.grid^4)
preds = predict(object = wageFit, newdata = predDat)
plot(x = Wage$age, y = Wage$wage, xlab = "age", ylab = "wage")
#lwd makes the line wider
lines(x = age.grid, y = preds, col = "blue", lwd = 2)

wageFit = lm(formula = wage ~ bs (age,knots = c(25,40,60) ) , data = Wage )
preds = predict(object = wageFit, newdata = predDat)
plot(x = Wage$age, y = Wage$wage, xlab = "age", ylab = "wage")
#lwd makes the line wider
lines(x = age.grid, y = preds, col = "blue", lwd = 2)

#R sets the # of knots = df-3; equally spaced quantiles -> 1st,2nd,3rd quantiles
wageFit = lm(formula = wage ~ bs (age,df = 6 ) , data = Wage )
preds = predict(object = wageFit, newdata = predDat)
plot(x = Wage$age, y = Wage$wage, xlab = "age", ylab = "wage")
#lwd makes the line wider
lines(x = age.grid, y = preds, col = "blue", lwd = 2)
attr(bs(Wage$age, df = 6), "knots")


wageFit = lm(formula = wage ~ bs (age,df = 6 ) + year, data = Wage )
year.grid = seq( from = min(Wage$year), to = max(Wage$year), by = 1)
predDat = data.frame( age = rep(age.grid, times = length(year.grid)), 
                      year = rep(year.grid, each = length(age.grid)) )
preds = predict(object = wageFit, newdata=predDat)
persp( x = age.grid, y = year.grid, z = matrix( preds, nrow = length(age.grid)),
       xlab = "age", ylab = "year", zlab = "wage")

wageFit = lm(formula = wage ~ bs (age,df = 6 ) + bs(year,df = 5), data = Wage )
predDat = data.frame( age = rep(age.grid, times = length(year.grid)), 
                      year = rep(year.grid, each = length(age.grid)) )
preds = predict(object = wageFit, newdata=predDat)
persp( x = age.grid, y = year.grid, z = matrix( preds, nrow = length(age.grid)),
       xlab = "age", ylab = "year", zlab = "wage",theta = 45)