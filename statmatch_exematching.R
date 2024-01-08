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