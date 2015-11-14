source('rolfunctions.r')
source('roldata.r')

library(e1071)

opg28 <- opg[opg$year==1828 & grepl("^[A-L]", opg$mlast), ]
opg26 <- opg[opg$year==1826 & grepl("^[A-L]", opg$mlast), ]
x <- strdistcombine(opg28, opg26)

x$correctmtch <- paste0(x$persid, x$persid.1) %in% paste0(tra$persid, tra$X)
x <- score(x)
set.seed(2718)
smpl <- rbinom(nrow(x), 1, p=0.5)
trn <- x[smpl==1, ]
tst <- x[smpl==0, ]

trn <- trn[, grep('correct|dist|sdx', names(trn))]
trn <- trn[complete.cases(trn), ]

m <- glm(correctmtch ~ mlastdist + mfirstdist + minidist
    + wlastdist + wfirstdist + winidist
    + mlastsdx + mfirstsdx + wlastsdx + wfirstsdx, 
    family=binomial(link="probit"),
    data=trn)
pred_probit <- predict(m, type='response')
fill <- NULL
cuts <- seq(0.01, 0.99, length=1000)
for (i in cuts){
    fill <- rbind(fill, glmperf(i, m, trn$correctmtch))
}
plot(cuts, fill[, 1], type='l')
lines(cuts, fill[, 2], col=2)
lines(cuts, fill[, 3], col=3)
lines(cuts, fill[, 4], col=4)
cutoff <- cuts[which.min(fill[,3])]

table(trn$correctmtch, pred_probit > cutoff)
# very few false negative, quite some false positives
# due to very low cutoff

fancy <- svm(correctmtch ~ ., data=trn)
summary(fancy)
pred_svm <- fitted(fancy)
table(trn$correctmtch, pred_svm)
# decent

tst <- tst[complete.cases(tst), ]
testpredict <- predict(fancy, newdata=tst)
table(tst$correctmtch, testpredict)

# compare with 
firstpass <- do.call(rbind, lapply(split(dat_y12, dat_y12$persid), function(dat) dat[which.min(dat$oscore), ]))
secondpass <- do.call(rbind, lapply(split(firstpass, firstpass$persid.1), function(dat) dat[which.min(dat$oscore), ]))
