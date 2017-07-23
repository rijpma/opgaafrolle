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
plot(cuts, fill$fapo / length(!trn$correct), type='l', col=2, ylab='error rate')
lines(cuts, fill$fane / length(!trn$correct), type='l', col=2)
abline(v = c(0.5, cuts[which.min(sqrt(fill[,2]^2 + fill[,3]^2))]), col='gray70')
text(c(0.25, 0.7), c(0.06, 0.03), c("False positives", "false negatives"))

# plot(cuts, sqrt(fill[,2]^2 + fill[,3]^2), type='l', col=2)
perf = performance(prediction(pred_lgt_trn, trn$correct), measure="tpr", x.measure="fpr")
plot(perf, col=2)

# plot(cuts, fill$spec / sum(!trn$correct), type='l', col=2, ylim=c(0, 1))
# lines(cuts, fill$sens / sum(trn$correct), type='l', col=2)
dev.off()

conf_lgt_trn = table(pred_lgt_trn > 0.5, trn$correct, dnn=c('predicted', 'true'))
pred_lgt_vld = predict(m_lgt, newdata=vld, type='response')
conf_lgt_vld = table(pred_lgt_vld > 0.5, vld$correct, dnn=c('predicted', 'true'))

conf = cbind(conf_lgt_trn, conf_lgt_vld)
conf = rbind(colnames(conf), conf)
conf = cbind(rownames(conf), conf)
conf = cbind(c("", "Predicted", ""), conf)
print(xtable(conf,
        caption = "Confusion matrix for logit models", 
        label = "tab:lgt_cfm"), 
    include.colnames=F, include.rownames=F,
    hline.after=c(nrow(conf)),
    add.to.row = list(pos=list(-1), command=c("& & \\multicolumn{4}{c}{Actual} \\\\ & & \\multicolumn{2}{c}{Train} & \\multicolumn{2}{c}{Test} \\\\ \\hline \\\\")),
    file = "linkpaper/lgt_confmat.tex")

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
conf_rf_trn = table(trn$correct, pred_rf_trn[, 2] > 0.5)
pred_rf_vld = predict(m_rf, newdata=vld, type='prob')
conf_rf_vld = table(vld$correct, pred_rf_vld[, 2] > 0.5)

# tp = data.table(dist = vld$mlast_neighbour_lag_dist, pred = pred_rf_vld[,2 ])
# plot(tp)
# plot(predict(loess(pred ~ dist, data = tp), newdata = seq(0, 1, length.out = 1e3)))
# tp = data.table(dist = vld$wmlastdist_from, pred = pred_rf_vld[,2 ])
# plot(tp)
# plot(predict(loess(pred ~ dist, data = tp), newdata = seq(0, 1, length.out = 1e3)))
# plot(predict(m, newdata = data.frame(dist = seq(0, 1, length.out=1000))))
# tp = data.table(dist = vld$minidist, pred = pred_rf_vld[,2 ])
# plot(tp)
# plot(predict(loess(pred ~ dist, data = tp), newdata = seq(0, 1, length.out = 1e3)))
# lines(x=seq(0, 1, length.out=1e3), predict(m, newdata = data.frame(dist = seq(0, 1, length.out=1000))))

conf = cbind(conf_rf_trn, conf_rf_vld)
conf = rbind(colnames(conf), conf)
conf = cbind(rownames(conf), conf)
conf = cbind(c("", "Predicted", ""), conf)
print(xtable(conf,
        caption = "Confusion matrix for random forests model", 
        label = "tab:rf_cfm"), 
    include.colnames=F, include.rownames=F,
    hline.after=c(nrow(conf)),
    add.to.row = list(pos=list(-1), command=c("& & \\multicolumn{4}{c}{Actual} \\\\ & & \\multicolumn{2}{c}{Train} & \\multicolumn{2}{c}{Test} \\\\ \\hline \\\\")),
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
plot(voteshares, fill[, "fapo"] / length(!trn$correct), type='l', col=2, ylab='error rate', ylim=range(fill[, 2:3] / length(trn$correct)))
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
# 15/31 had one wife missing, not the other ones
false_positives_rf[, grep("(last|first)_", names(false_positives_rf)), with = F]
false_positives_rf[!(wlast_to == "" | wlast_from == ""), grep("(last|first)_", names(false_positives_rf)), with = F]
# 12/15 remaining ones seem correct (3 of which have KRUGER surname (fourth most frequent)
# 1/15 seems very likely (LABUSCHAGNE), but different m first name 
# 1/15 seems very likely (LIEBENBERG), but different w surname 
# 1/15 seems wrong (JANZEN)

# sensitivity to variable inclusion
# base AUC
base_auc = performance(prediction(as.numeric(predict(m_rf, vld)), vld$correct), 'auc')

# commented out because slow
# ml = list()
# for (vrb in names(trn)[-3]){
#     frm = as.formula(paste("factor(correct) ~ . -", vrb))
#     ml[[vrb]] = randomForest(frm, data = trn)
#     print(i)
# }
# perfs = sapply(ml, function(x) performance(prediction(as.numeric(predict(x, vld)), vld$correct), 'auc')@y.values)
# dotchart(unlist(perfs))
# print(xtable(data.table(names(trn)[-3], AUC = unlist(perfs))[order(AUC)],
#         caption = "AUC after omitting one variable", 
#         label = "tab:sens_1var"),
#     file = "linkpaper/sensitivity_1var.tex", 
#     type = 'latex', include.rownames=F)


# ml = list()
# ncomb = choose(length(names(trn)) - 1, 2)
# combinations = combn(names(trn)[-3], 2)
# for (i in 1:ncomb){
#     frm = as.formula(paste("factor(correct) ~ . -", paste0(combinations[, i], collapse = ' - ')))
#     ml[[i]] = randomForest(frm, data = trn)
#     print(i)
# }
# perfs = sapply(ml, function(x) performance(prediction(as.numeric(predict(x, vld)), vld$correct), 'auc')@y.values)
# dotchart(unlist(perfs))
# plot(unlist(perfs))
# plot(ecdf(unlist(perfs)))
# abline(v = 0.91)
# print(xtable(data.table(t(combinations[, perfs < 0.91]), AUC = unlist(perfs[perfs < 0.91]))[order(AUC)]),
#     type = "latex", "linkpaper/sensitivity_2var.tex")
# combinations[, perfs < 0.88]


# separate rf
keep = names(trn)[grep('correct|[a-z]dist$|sdx|mtchs|wife|both|namefreq|dchild|spouse', names(trn))]

trn = trn[, keep, with=F]
trn = trn[complete.cases(trn), ]
trn = trn[, lapply(.SD, normalise)]
vld = vld[, lapply(.SD, normalise), .SDcols = names(trn)]
apply(trn, 2, range, na.rm=T)
apply(vld, 2, range, na.rm=T)


m_rf
m_rf_yeswf
m_rf_nowf

table(predict(m_rf_yeswf, newdata=trn[(wifepresent_from | wifepresent_to), ]), trn[(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_nowf, newdata=trn[!(wifepresent_from | wifepresent_to), ]), trn[!(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_yeswf, newdata=vld[(wifepresent_from | wifepresent_to),]), vld[(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_nowf, newdata=vld[!(wifepresent_from | wifepresent_to),]), vld[!(wifepresent_from | wifepresent_to), correct])
# improvement in wife present
# deterioration if wife absent

# proper check on comparable datasets suggests overall improvement if applied separtely
# would still be better to incorporate this in model
table(predict(m_rf, newdata=vld[(wifepresent_from | wifepresent_to),]), vld[(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_yeswf, newdata=vld[(wifepresent_from | wifepresent_to),]), vld[(wifepresent_from | wifepresent_to), correct])

table(predict(m_rf, newdata=vld[!(wifepresent_from | wifepresent_to),]), vld[!(wifepresent_from | wifepresent_to), correct])
table(predict(m_rf_nowf, newdata=vld[!(wifepresent_from | wifepresent_to),]), vld[!(wifepresent_from | wifepresent_to), correct])
