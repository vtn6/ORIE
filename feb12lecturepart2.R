library(tree)
data(cpus,package = "MASS" )
hist(cpus$perf)
plot(cpus$perf,cpus$syct,col = c("red","green"))
plot(log(cpus$perf),cpus$syct,col = c("red","green"))
plot(log(cpus$perf),cpus$mmin,col = c("red","green"))

cpus.tree = tree( formula = log(perf) ~syct + mmin + mmax + cach + chmin + chmax,data = cpus)

plot(cpus.tree,type = "uniform")
text(cpus.tree)

plot(cpus$cach,log(cpus$perf),)

plot(cpus$mmax[cpus$cach < 27], log( cpus$perf[cpus$cach<27] ))

print(predict( object = cpus.tree, newdata = data.frame(cach = 40,mmax = 200000,syct = 300,mmin = 3000 , chmin = 8, chmax = 50)))