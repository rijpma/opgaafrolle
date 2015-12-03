setwd('~/dropbox/opgaafrol/')

library(stringdist)
library(e1071)
library(nnet)
library(randomForest)

source('rolfunctions.r')
source('roldata.r')

opg28 <- opg[opg$year==1828 & grepl("^[A-L]", opg$mlast), ]
opg26 <- opg[opg$year==1826 & grepl("^[A-L]", opg$mlast), ]

opg28$clsidx <- closeindex(list(opg28$mlast, opg28$mfirst))
dplcts <- opg28$clsidx[duplicated(opg28$clsidx)]
opg28_d <- opg28[opg28$clsidx %in% dplcts, c('mlast', 'mfirst', 'old', 'young', 'clsidx', 'persid')]

x <- strdistcombine(opg28, opg26)
x <- x[complete.cases(x), ]

x$correct <- paste0(x$persid, x$persid.1) %in% paste0(tra$persid, tra$X)
x <- score(x)

x$mlastdist <- x$mlastdist / max(x$mlastdist)
x$mtchs <- x$mtchs / max(x$mlastdist)
x$namefreq <- x$namefreq / max(x$namefreq, na.rm=T)
x$namefreq.1 <- x$namefreq.1 / max(x$namefreq.1, na.rm=T)
x$nrdist <- abs(x$nrdist) / max(abs(x$nrdist), na.rm=T)

set.seed(2718)
smpl <- rbinom(nrow(x), 1, p=0.5)
trn <- x[smpl==1, ]
vld <- x[smpl==0, ]

# compare with manual method
firstpass <- do.call(rbind, lapply(split(trn, trn$persid), function(dat) dat[which.min(dat$oscore), ]))
secondpass <- do.call(rbind, lapply(split(firstpass, firstpass$persid.1), function(dat) dat[which.min(dat$oscore), ]))
secondpass$manual <- secondpass$correct
secondpass$mtchid <- paste0(secondpass$persid, secondpass$persid.1)
trn$mtchid <- paste0(trn$persid, trn$persid.1)
man <- data.frame(correct=trn[, 'correct'], secondpass[match(trn$mtchid, secondpass$mtchid), 8:ncol(secondpass)])

spec <- sum(is.na(man$manual)[!man$correct])
fane <- sum(is.na(man$manual)[man$correct])
fapo <- sum(!man$manual, na.rm=T) 
sens <- sum(man$manual, na.rm=T)
bsl <- matrix(c(spec, fane, fapo, sens), ncol=2)
# many false positives!

trn <- trn[, grep('correct|dist$|sdx|wife|mtchs|old|young|samedistrict|namefreq', 
    names(trn))]
apply(trn, 2, range)

m_lgt <- glm(correct ~ mlastdist + mfirstdist + minidist
    + mlastsdx + mfirstsdx 
    + wlastdist + wfirstdist # + winidist
    # + wlastsdx + wfirstsdx
    + wifepresent + mtchs, # + old + young,
    family=binomial,
    data=trn)
summary(m_lgt)
pred_lgt_trn <- predict(m_lgt, newdata=trn, type='response')

fill <- NULL
cuts <- seq(0.01, 0.99, length=1000)
for (i in cuts){
    pred <- m_lgt$fit > i
    fill <- rbind(fill, modperf(pred, trn$correct))
}

fillr <- fill / rowSums(fill)
fill$cut <- fillr$cut <- cuts
cutoff <- cuts[which.min(fill[,3])]
table(trn$correct, pred_lgt_trn > 0.7)
# check if fp get low probabilities

pred_lgt_vld <- predict(m_lgt, newdata=vld, type='response')
table(trn$correct, pred_lgt_trn > 0.7)
table(vld$correct, pred_lgt_vld > 0.7)

m_svm <- svm(as.factor(correct) ~ ., data=trn)
summary(m_svm)
pred_svm_trn <- predict(m_svm, newdata=trn)
table(trn$correct, pred_svm_trn)
# pred_svm_vld <- predict(m_svm, newdata=vld)
# table(vld$correct, pred_svm_vld)

# vrbs to add: region, some neighbour thing from hhid
# old/young not necessarily close
# wife surname change to husband's wife surnm equals husbsurnmn

# M <- 25
# testerr <- ooberr <- double(M)
# for (i in 1:M){
#     fit <- randomForest(as.factor(correct) ~ ., data=trn, mtry=i)
#     ooberr[i] <- fit$err.rate[500, 1]
#     pred <- predict(fit, newdata=vld)
#     testerr[i] <- 1 - sum(vld$correct==pred) / length(pred)
#     cat(i, '\n')
# }
# matplot(1:M, cbind(ooberr, testerr), type='b', pch=1, lty=1, col=1:2)
# legend('topright', legend=c('OutoBag', 'Validation'), fill=1:2, )
# suggests 3/4 for training, 5 for validation

m_rf <- randomForest(as.factor(correct) ~ ., data=trn, mty=5)
summary(m_rf)
varImpPlot(m_rf)
pred_rf_trn <- predict(m_rf, newdata=trn)
table(trn$correct, pred_rf_trn)
pred_rf_vld <- predict(m_rf, newdata=vld)
table(vld$correct, pred_rf_vld)

# find vote optimum
votes_vld <- predict(m_rf, newdata=vld, type='prob')
voteshares <- seq(0.1, 0.9, by=0.01)
fill <- NULL
for (share in voteshares){
    fill <- rbind(fill, c(table(vld$correct, votes_vld[, 2] > share)))
}
colnames(fill) <- c('ff', 'falseneg', 'falspos', 'tt')
fill <- fill / sum(vld$correct)
matplot(voteshares, fill[, 2:3], lty=1, type='l', col=1)
abline(v=0.5, col='gray', lty=2)
text(x=voteshares[10], y=fill[5, 2:3] * 1.5, labels=c('falseneg', 'falsepos'))

plot(sqrt(fill[,2]^2 + fill[,3]^2), type='l')


# do nowife on entire data with wife set to ''?
m_rf_yeswf <- randomForest(as.factor(correct) ~ ., data=trn[trn$wifepresent, ])
m_rf_nowf <- randomForest(as.factor(correct) ~ ., data=trn[!trn$wifepresent, ])

table(predict(m_rf_yeswf), trn$correct[trn$wifepresent])
table(predict(m_rf_nowf), trn$correct[!trn$wifepresent])
table(predict(m_rf_yeswf, newdata=vld[vld$wifepresent, ]), vld$correct[vld$wifepresent])
table(predict(m_rf_nowf, newdata=vld[!vld$wifepresent, ]), vld$correct[!vld$wifepresent])

# m_nn <- nnet(as.factor(correct) ~ ., data=trn, size=2)
# summary(m_nn)
# pred_nn_trn <- predict(m_nn, type='class', newdata=trn)
# table(trn$correct, pred_nn_trn)
# pred_nn_vld <- predict(m_nn, type='class', newdata=vld)
# table(vld$correct, pred_nn_vld)