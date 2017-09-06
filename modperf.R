rm(list=ls())

setwd('~/dropbox/opgaafrol/')

source('rolmodels.R')

# logit model

summary(m_lgt)

texreg::texreg(m_lgt, label = "tab:logitmod", file = "logitmod.tex",
    caption = "Logistical regression predicting record matches")
pred_lgt_trn = predict(m_lgt, newdata=trn, type='response')
fill = NULL
cuts = seq(0.01, 0.99, length=1000)
for (i in cuts){
    pred = m_lgt$fit > i
    fill = rbind(fill, modperf(pred, trn$correct))
}
fillr = fill / rowSums(fill)
fill$cut = fillr$cut = cuts

pdf("linkpaper/logitperformance.pdf", width=9, height=5)
par(mfrow=c(1, 2))
plot(cuts, fill$fapo / length(!trn$correct), type='l', col=2, xlab = 'threshold', ylab='error rate')
lines(cuts, fill$fane / length(!trn$correct), type='l', col=2)
abline(v = c(0.5, cuts[which.min(sqrt(fill[,2]^2 + fill[,3]^2))]), col='gray70')
text(c(0.25, 0.7), c(0.06, 0.03), c("False positives", "false negatives"))

# plot(cuts, sqrt(fill[,2]^2 + fill[,3]^2), type='l', col=2)
perf = performance(prediction(pred_lgt_trn, trn$correct), measure="tpr", x.measure="fpr")
plot(perf, col=2)

# plot(cuts, fill$spec / sum(!trn$correct), type='l', col=2, ylim=c(0, 1))
# lines(cuts, fill$sens / sum(trn$correct), type='l', col=2)
dev.off()

conf_lgt_trn = table(trn$correct, pred_lgt_trn > 0.5, dnn=c('actual', 'predicted'))
pred_lgt_vld = predict(m_lgt, newdata=vld, type='response')
conf_lgt_vld = table(vld$correct, pred_lgt_vld > 0.5, dnn=c('actual', 'predicted'))

conf = cbind(conf_lgt_trn, conf_lgt_vld)
conf = rbind(colnames(conf), conf)
conf = cbind(rownames(conf), conf)
conf = cbind(c("", "Actual", ""), conf)
print(xtable(conf,
        caption = "Confusion matrix for logit models", 
        label = "tab:lgt_cfm"), 
    include.colnames=F, include.rownames=F,
    hline.after=c(nrow(conf)),
    add.to.row = list(pos=list(-1), command=c("& & \\multicolumn{4}{c}{Predicted} \\\\ & & \\multicolumn{2}{c}{Train} & \\multicolumn{2}{c}{Test} \\\\ \\hline \\\\")),
    file = "linkpaper/lgt_confmat.tex")

conf_lgt_trn / rowSums(conf_lgt_trn)
conf_lgt_vld / rowSums(conf_lgt_vld)

# random forests

pdf("linkpaper/coefplots.pdf", width=9, height=5)
par(mfrow=c(1, 2), font.main=1)
cf_lgt = coef(m_lgt)
dotchart(sort(cf_lgt), main="Logistic reg.", xlab='reg. coef') 
abline(v=0, col='gray70')
varImpPlot(m_rf, main='Random forests')
dev.off()

m_rf # 
pred_rf_trn = predict(m_rf, newdata=trn, type='prob')
conf_rf_trn = table(trn$correct, pred_rf_trn[, 2] > 0.5, dnn=c('actual', 'predicted'))
pred_rf_vld = predict(m_rf, newdata=vld, type='prob')
conf_rf_vld = table(vld$correct, pred_rf_vld[, 2] > 0.5, dnn=c('actual', 'predicted'))

# tp = data.table(dist = vld$minidist, pred = pred_rf_vld[,2 ])
# plot(tp)
# lines(seq(0, 1, length.out = 1e3),
#     predict(loess(pred ~ dist, data = tp), newdata = seq(0, 1, length.out = 1e3)), col = 2)

conf = cbind(conf_rf_trn, conf_rf_vld)
conf = rbind(colnames(conf), conf)
conf = cbind(rownames(conf), conf)
conf = cbind(c("", "Actual", ""), conf)
print(xtable(conf,
        caption = "Confusion matrix for random forests model", 
        label = "tab:rf_cfm"), 
    include.colnames=F, include.rownames=F,
    hline.after=c(nrow(conf)),
    add.to.row = list(pos=list(-1), command=c("& & \\multicolumn{4}{c}{Predicted} \\\\ & & \\multicolumn{2}{c}{Train} & \\multicolumn{2}{c}{Test} \\\\ \\hline \\\\")),
    file = "linkpaper/rf_confmat.tex")

conf_rf_trn / rowSums(conf_rf_trn)
conf_rf_vld / rowSums(conf_rf_vld)

# find vote optimum
votes_vld = predict(m_rf, newdata=vld, type='prob')
voteshares = seq(0.1, 0.9, by=0.01)
fill = NULL
for (share in voteshares){
    fill = rbind(fill, c(table(vld$correct, votes_vld[, 2] > share)))
}
colnames(fill) = c('ff', 'fane', 'fapo', 'tt')

pdf("linkpaper/rfperformance.pdf", width=9, height=5)
par(mfrow=c(1, 2))
plot(voteshares, fill[, "fapo"] / length(!trn$correct), type='l', col=2, xlab = 'threshold', ylab='error rate', ylim=range(fill[, 2:3] / length(trn$correct)))
lines(voteshares, fill[, "fane"] / length(!trn$correct), type='l', col=2)
text(c(0.3, 0.7), c(0.025, 0.025), c("False positives", "false negatives"))
abline(v = c(0.5, voteshares[which.min(sqrt(fill[,2]^2 + fill[,3]^2))]), col='gray70')

# plot(voteshares, sqrt(fill[,2]^2 + fill[,3]^2), type='l', col=2)
perf = performance(prediction(pred_rf_trn[, 2], trn$correct), measure="tpr", x.measure="fpr")
plot(perf, col=2)

# plot(voteshares, fill$spec / sum(!trn$correct), type='l', col=2, ylim=c(0, 1))
# lines(voteshares, fill$sens / sum(trn$correct), type='l', col=2)
dev.off()

false_positives_rf = cnd[smpl==0, ][pred_rf_vld[, 2] < 0.5 & correct == TRUE, ]

false_positives_rf[, sum(wlast_to == "" | wlast_from == "")]
false_positives_rf[, sum(wlast_to == "" & wlast_from == "")]
false_positives_rf[, sum(wfirst_to == "")]
# 2 were both missing, 16 had one or both wives missing
false_positives_rf[(wlast_to == "" | wlast_from == ""), grep("(last|first)_", names(false_positives_rf)), with = F]
# 1 wrong, 15 correct based on men's name only

false_positives_rf[, grep("(last|first)_", names(false_positives_rf)), with = F]
false_positives_rf[!(wlast_to == "" | wlast_from == ""), grep("(last|first)_", names(false_positives_rf)), with = F]


print(xtable(false_positives_rf[, lapply(.SD, tolower), .SDcols = grep("(last|first)_", names(false_positives_rf))],
        caption = "False positives created by random forest classifier", 
        label = "tab:falsepos"), 
    include.colnames=T, include.rownames=F,
    size="footnotesize",
    floating.environment="sidewaystable",
    file = "linkpaper/falsepositives.tex")
# 1/9 seems wrong (KNOETZE), different wife surname
# 1/9 is probably wrong due to different wife (BEETGE)

# 3/9 seems very likely correct (LABUSCHAGNE, KRUGER), but slightly different m first name 
# 1/9 seems likely correct (KRUGER), but different m first name 
# 3/9 are probably correct


# sensitivity to variable inclusion
# base AUC
base_auc = performance(prediction(as.numeric(predict(m_rf, vld)), vld$correct), 'auc')
base_auc
# commented out because slow
# ml = list()
# for (vrb in names(trn)[-3]){
#     frm = as.formula(paste("factor(correct) ~ . -", vrb))
#     ml[[vrb]] = randomForest(frm, data = trn)
#     print(vrb)
# }
# perfs = sapply(ml, function(x) performance(prediction(as.numeric(predict(x, vld)), vld$correct), 'auc')@y.values)
# dotchart(unlist(perfs))
# sns1 = data.table(names(trn)[-3], AUC = unlist(perfs))[order(AUC)]
# print(xtable(sns1,
#         caption = "AUC after omitting one variable (full model AUC: 0.94)", 
#         label = "tab:sens_1var"),
#     type = 'latex', include.rownames=F,
#     file = "linkpaper/sensitivity_1var.tex")


ml = list()
sum(choose(length(names(trn)) - 1, 1:(length(names(trn)) - 1)))
ncomb = choose(length(names(trn)) - 1, 2)
combinations = combn(names(trn)[-3], 2)
for (i in 1:ncomb){
    frm = as.formula(paste("factor(correct) ~ . -", paste0(combinations[, i], collapse = ' - ')))
    ml[[i]] = randomForest(frm, data = trn)
    print(i)
}
perfs = sapply(ml, function(x) performance(prediction(as.numeric(predict(x, vld)), vld$correct), 'auc')@y.values)
dotchart(unlist(perfs))
plot(unlist(perfs))
plot(ecdf(unlist(perfs)))
abline(v = 0.931)
out = data.table(t(combinations), AUC = unlist(perfs))[order(AUC), ]

print(xtable(head(out, 20),
    caption = "AUC after omitting two variables (full model AUC: 0.94). Note: only 20 lowest AUC values reported.", 
    label = "tab:sens_2var"),
    type = 'latex', include.rownames=F,
    file = "linkpaper/sensitivity_2var.tex")


# separate rf
m_rf
m_rf_yeswf
m_rf_nowf

table(predict(m_rf_yeswf, newdata=trn[(wifepresent_from | wifepresent_to), ]), trn[(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_nowf, newdata=trn[!(wifepresent_from | wifepresent_to), ]), trn[!(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_yeswf, newdata=vld[(wifepresent_from | wifepresent_to),]), vld[(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_nowf, newdata=vld[!(wifepresent_from | wifepresent_to),]), vld[!(wifepresent_from | wifepresent_to), correct])
# improvement in wife present
# deterioration if wife absent

# proper check on comparable datasets suggests overall improvement if applied separately
# would still be better to incorporate this in model directly
table(predict(m_rf, newdata=vld[(wifepresent_from | wifepresent_to),]), vld[(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_yeswf, newdata=vld[(wifepresent_from | wifepresent_to),]), vld[(wifepresent_from | wifepresent_to), correct])

table(predict(m_rf, newdata=vld[!(wifepresent_from | wifepresent_to),]), vld[!(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_nowf, newdata=vld[!(wifepresent_from | wifepresent_to),]), vld[!(wifepresent_from | wifepresent_to), correct])


table(predict(m_rf, newdata=vld[(wifepresent_from | wifepresent_to),]), vld[(wifepresent_from | wifepresent_to), correct]) + 
    table(predict(m_rf, newdata=vld[!(wifepresent_from | wifepresent_to),]), vld[!(wifepresent_from | wifepresent_to), correct])


table(predict(m_rf_nowf, newdata=vld[!(wifepresent_from | wifepresent_to),]), vld[!(wifepresent_from | wifepresent_to), correct]) + 
    table(predict(m_rf_yeswf, newdata=vld[(wifepresent_from | wifepresent_to),]), vld[(wifepresent_from | wifepresent_to), correct])



m_rf_gnl # 
pred_rf_trn_gnl = predict(m_rf_gnl, newdata=trn_gnl, type='prob')
conf_rf_trn_gnl = table(trn_gnl$correct, pred_rf_trn_gnl[, 2] > 0.5, dnn=c('actual', 'predicted'))
pred_rf_vld_gnl = predict(m_rf_gnl, newdata=vld_gnl, type='prob')
conf_rf_vld_gnl = table(vld_gnl$correct, pred_rf_vld_gnl[, 2] > 0.5, dnn=c('actual', 'predicted'))

conf_rf_trn_gnl / rowSums(conf_rf_trn_gnl)
conf_rf_vld_gnl / rowSums(conf_rf_vld_gnl)
