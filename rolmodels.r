rm(list=ls())

setwd('~/dropbox/opgaafrol/')

library("stringdist")
library("e1071")
library("nnet")
library("randomForest")
library("data.table")
library("ROCR")
library("xtable")

source('rolfunctions.r')

# opg = data.table::fread(input="zcat < opg_cleaned.csv.gz")
opg = data.table::fread("gunzip -c opg_cleaned.csv.gz", check.names = TRUE)
tra = data.table::fread('matched.csv', check.names = TRUE)
setnames(tra, c("persid", "V11"), c("persid_1828", "persid_1826"))

dim(tra)
dim(tra[!is.na(persid_1828) & !is.na(persid_1826), ])
tra = tra[!is.na(persid_1828) & !is.na(persid_1826), ]

opg28 = opg[year == 1828 & grepl("^[A-L]", mlast), ]
opg26 = opg[year==1826 & grepl("^[A-L]", mlast), ]

cnd = candidates(opg28, opg26)

cnd[!is.na(persid_from) & !is.na(persid_to), correct := paste0(persid_from, persid_to) %in% paste0(tra$persid_1828, tra$persid_1826)]
nrow(tra) - sum(cnd$correct==TRUE, na.rm=T)
# diff is persons matched but not identified as candidates or with wrong candidates
cnd = cnd[!is.na(correct), ]

cnd[id_from == 6]

cnd = score(cnd, include_manual=T)

set.seed(2718)
smpl = rbinom(nrow(cnd), 1, p=0.5)
trn = cnd[smpl==1, ]
vld = cnd[smpl==0, ]

# compare with manual method
firstpass = do.call(rbind, lapply(split(trn, trn$persid_from), function(dat) dat[which.min(dat$score), ]))
secondpass = do.call(rbind, lapply(split(firstpass, firstpass$persid_to), function(dat) dat[which.min(dat$score), ]))
secondpass$manual = secondpass$correct
secondpass$mtchid = paste0(secondpass$persid_from, secondpass$persid_to)
trn$mtchid = paste0(trn$persid_from, trn$persid_to)
man = data.frame(correct=trn[, correct], secondpass[match(trn$mtchid, secondpass$mtchid), 8:ncol(secondpass)])

spec = sum(is.na(man$manual)[!man$correct], na.rm=T)
fane = sum(is.na(man$manual)[man$correct], na.rm=T)
fapo = sum(!man$manual, na.rm=T) 
sens = sum(man$manual, na.rm=T)
bsl = matrix(c(spec, fane, fapo, sens), ncol=2)
bsl

keep = names(trn)[grep('correct|[a-z]dist$|sdx|mtchs|both|namefreq|dchild|spouse', names(trn))]
# print(xtable(data.frame(variable = keep, explanation = NA)), include.rownames=F)

trn = trn[, keep, with=F]
trn = trn[complete.cases(trn), ]
trn = trn[, lapply(.SD, normalise)]
vld = vld[, lapply(.SD, normalise), .SDcols = names(trn)]
apply(trn, 2, range, na.rm=T)
apply(vld, 2, range, na.rm=T)
# bothwineprod removes a dozen observations or so, consider removing

m_lgt = glm(correct ~ ., family=binomial(link="logit"), data=trn)


# vrbs to add: region, some neighbour thing from hhid
# old/young not necessarily close
# wife surname change to husband's wife surnm equals husbsurnmn

# M = 25
# testerr = ooberr = double(M)
# for (i in 1:M){
#     fit = randomForest(as.factor(correct) ~ ., data=trn, mtry=i)
#     ooberr[i] = fit$err.rate[500, 1]
#     pred = predict(fit, newdata=vld)
#     testerr[i] = 1 - sum(vld$correct==pred) / length(pred)
#     cat(i, '\n')
# }
# matplot(1:M, cbind(ooberr, testerr), type='b', pch=1, lty=1, col=1:2)
# legend('topright', legend=c('OutoBag', 'Validation'), fill=1:2, )
# suggests 3/4 for training, 5 for validation

m_rf = randomForest(as.factor(correct) ~ ., data=trn, mty=5)

set.seed(2718)
smpl = rbinom(nrow(cnd), 1, p=0.5)
trn = cnd[smpl==1, ]
vld = cnd[smpl==0, ]

keep = names(trn)[grep('correct|[a-z]dist$|sdx|mtchs|wife|both|namefreq|dchild|spouse', names(trn))]

trn = trn[, keep, with=F]
trn = trn[complete.cases(trn), ]
trn = trn[, lapply(.SD, normalise)]
vld = vld[, lapply(.SD, normalise), .SDcols = names(trn)]
apply(trn, 2, range, na.rm=T)
apply(vld, 2, range, na.rm=T)

# do nowife on entire data with wife set to ''?
m_rf_yeswf = randomForest(as.factor(correct) ~ ., data=trn[(wifepresent_from | wifepresent_to), ])
m_rf_nowf = randomForest(as.factor(correct) ~ ., data=trn[!(wifepresent_from | wifepresent_to), ])

# m_svm = svm(as.factor(correct) ~ ., data=trn)
# summary(m_svm)
# pred_svm_trn = predict(m_svm, newdata=trn)
# table(trn$correct, pred_svm_trn)
# pred_svm_vld = predict(m_svm, newdata=vld)
# table(vld$correct, pred_svm_vld)

# nn does not perform well, may need tuning
# m_nn = nnet(as.factor(correct) ~ ., data=trn, size=2)
# summary(m_nn)
# pred_nn_trn = predict(m_nn, type='class', newdata=trn)
# table(trn$correct, pred_nn_trn)
# pred_nn_vld = predict(m_nn, type='class', newdata=vld)
# table(vld$correct, pred_nn_vld)
