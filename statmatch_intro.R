####-----------Exercise based on DOrazio_statmatching(2011)-------------####

library(StatMatch)
library(simPop) #package simPop for data
data("eusilcS") #dataset
str(eusilcS)


# data transformations
##discarding units with age <16
silc.16 <- subset(eusilcS, age>15)
##categorize age
silc.16$c.age <- cut(silc.16$age, c(16,24,49,64,100), include.lowest=T)
##truncate hsize
aa <- as.numeric(silc.16$hsize)
aa[aa>6] <-6
silc.16$hsize6 <- factor(aa, ordered=T)
##recode personal economic status
aa <- as.numeric(silc.16$pl030)
aa[aa<3] <- 1
aa[aa>1] <- 2
silc.16$work <- factor(aa,levels = 1:2, labels = c("working","not working"))
##categorize personal net income
silc.16$c.netI <- cut(silc.16$net/1000, breaks = c(-6,0,5,10,15,20,25,30,40,50,200))
### silc.16 main data set


#recipient and donor dataset split
set.seed(123456)
obs.A <- sample(nrow(silc.16), 4000, replace = F)
X.vars <- c("hsize", "hsize6", "db040", "age", "c.age", "rb090", "pb220a", "rb050")
y.var <- c("pl030", "work")
z.var <- c("netIncome", "c.netI")
rec.A <- silc.16[obs.A, c(X.vars, y.var)]
don.B <- silc.16[-obs.A, c(X.vars, z.var)]
##weighting
###estimated size of population age>16
N <- round(sum(silc.16$rb050))
N
###rescale origin weights
rec.A$wwA <- rec.A$rb050/sum(rec.A$rb050)*N
don.B$wwB <- don.B$rb050/sum(don.B$rb050)*N


#choice of matching variables
##analysis on A - work status
work.glm <- glm(work ~ hsize+db040+age+rb090+pb220a, data=rec.A, family = binomial(link = "logit"))
summary(work.glm)
###stepwise selection
new.work.glm <- step(work.glm, trace=0) #step: choosing model by AIC in stepwise algorithm
summary(new.work.glm) #only significant predictors
###three variables important for matching variables in A: age, rb090, pb220a
X.Y.glm <- c("age", "rb090", "pb220a") #Y=work in rec.A

##analysis on B - income status
summary(don.B$netIncome)
don.B$lognetI <- log(don.B$netIncome+4373+1)#?
summary((don.B$lognetI))
###regression
lnetI.lm <- lm(lognetI ~ hsize+db040+age+rb090+pb220a, data=don.B)
summary(lnetI.lm)
###stepwise selection
new.lnetI.lm <- step(lnetI.lm, trace = 0)
summary(new.lnetI.lm)
###all predicters should obviously be included
X.Z.lm <- c("hsize", "db040", "age", "rb090", "pb220a")

##matching the variables
union(X.Y.glm, X.Z.lm)
###smallest subset of matching variables
intersect(X.Y.glm, X.Z.lm)

##uncertainty approach for matching variables
xx <- xtabs(~db040+hsize6+c.age+rb090+pb220a, data = rec.A)
xy <- xtabs(~db040+hsize6+c.age+rb090+pb220a+work, data = rec.A)
xz <- xtabs(~db040+hsize6+c.age+rb090+pb220a+c.netI, data = don.B)
out.fbw <- Fbwidths.by.x(tab.x = xx, tab.xy = xy, tab.xz = xz) #compute the bounds for cell probabilities in the contingency table Y vs. Z
###avg. widhts of uncertainty bounds
out.fbw ##?list object - hard to handle


#--------nonparametric approach: creating synthetic data set

#Nearest neighbor distance hot deck
group.v <- c("rb090", "db040")
X.mtc <- c("hsize", "age")
out.nnd <- NND.hotdeck(data.rec = rec.A, data.don = don.B,
                       match.vars = X.mtc, don.class = group.v,
                       dist.fun = "Manhattan")
###don.class: to reduce computation effort define donation classes (imputation calls)
###distances computed only among units in same class - donation classes based on categorial var. (geo area, etc.)

##summary rec&don
summary(out.nnd$dist.rd) #summary distances rec-don
summary(out.nnd$noad) #summary available donors at min distance
table(out.nnd$noad)

##derive synthetic data set
head(out.nnd$mtc.ids)
fA.nnd.m <- create.fused(data.rec = rec.A, data.don = don.B,
                          mtc.ids = out.nnd$mtc.ids,
                          z.vars = c("netIncome", "c.netI"))
head(fA.nnd.m)

##synthetic data set with constrained multiple usage of donor
group.v <- c("rb090", "db040")
X.mtc <- c("hsize", "age")
out.nnd.mc <- NND.hotdeck(data.rec = rec.A, data.don = don.B,
                       match.vars = X.mtc, don.class = group.v,
                       dist.fun = "Manhattan", constrained = T,
                       constr.alg = "lpSolve")

fA.nnd.mc <-- create.fused(data.rec = rec.A, data.don = don.B,
                          mtc.ids = out.nnd.mc$mtc.ids,
                          z.vars = c("netIncome", "c.netI"))
head(fA.nnd.mc)


#Random hot deck
group.v <- c("db040","rb090")
rnd.1 <- RANDwNND.hotdeck(data.rec = rec.A, data.don = don.B,
                          match.vars = NULL, don.class = group.v)

## syn. data set
fA.rnd <- create.fused(data.rec = rec.A, data.don = don.B,
                       mtc.ids = rnd.1$mtc.ids, z.vars = c("netIncome", "c.netI"))

##constrained syn. data set
###random choices of a donor among the closest k=20 wrt age
group.v <- c("db040","rb090")
X.mtc <- c("age")
rnd.2 <- RANDwNND.hotdeck(data.rec = rec.A, data.don = don.B, 
                          match.vars = X.mtc, don.class = group.v,
                          dist.fun = "Manhattan", cut.don = "exact", k = 20)

fA.knnd <- create.fused(data.rec = rec.A, data.don = don.B,
                       mtc.ids = rnd.2$mtc.ids, z.vars = c("netIncome", "c.netI"))
head(fA.knnd)


#Imputation with StatMatch
###exe for imputation with StatMatch
set.seed(123456)
miss <- rbinom(150, 1, 0.30) #generates randomly missing
iris.miss <- iris
iris.miss$Petal.Length[miss==1] <- NA
summary(iris.miss$Petal.Length)

##separate units into two data sets
rec <- subset(iris.miss, is.na(Petal.Length), select = -Petal.Length)
don <- subset(iris.miss, !is.na(Petal.Length))

##search for closest donors
X.mtc <- c("Sepal.Length", "Sepal.Width", "Petal.Width")
nnd <- NND.hotdeck(data.rec = rec, data.don = don,
                       match.vars = X.mtc, don.class = "Species",
                       dist.fun = "Manhattan")

##fills.rec
imp.rec <- create.fused(data.rec = rec, data.don = don,
                        mtc.ids = nnd$mtc.ids, z.vars = "Petal.Length")
imp.rec$imp.PL <- 1 #flag for imputed

##re-aggregate data sets
don$imp.PL <- 0
imp.iris <- rbind(imp.rec, don)
summary(imp.iris)

##summary stat of imputed and non-imputed Petal.Length
tapply(imp.iris$Petal.Length, imp.iris$imp.PL, summary)


#Statmatch for complex survey data - naive approach
##summary info on weights
sum(rec.A$wwA) #estimated pop size A
sum(don.B$wwB) #estimated pop size B

summary(rec.A$wwA)
summary(don.B$wwB)

##NND unconstrained hotdeck
group.v <- c("rb090","db040")
X.mtc <- c("hsize","age")
out.nnd <- NND.hotdeck(data.rec = rec.A, data.don = don.B,
                       match.vars = X.mtc, don.class = group.v,
                       dist.fun = "Manhattan")

fA.nnd.m <- create.fused(data.rec = rec.A, data.don = don.B,
                           mtc.ids = out.nnd$mtc.ids,
                           z.vars = c("netIncome", "c.netI"))

###estimating average net income
weighted.mean(fA.nnd.m$netIncome, fA.nnd.m$wwA) #imputed in A
weighted.mean(don.B$netIncome, don.B$wwB) #reference estimate in B





