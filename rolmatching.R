rm(list=ls())
options(stringsAsFactors = FALSE)
setwd("~/dropbox/opgaafrol")

library("stringdist")
library("data.table")

source("rolfunctions.r")
source("rolmodels.r")

opg = data.table::fread(input="zcat < opg_cleaned.csv.gz")

mtchlist = list()
yr = unique(opg$year)[1]
for (yr in unique(opg$year)){
    combined = candidates(opg[year == yr, ], opg[year < yr, ])
    combined = score(combined)
    # combined[persid_from==725 & persid_to %in% c(725, 5608, 4103), ]

    # fix this nastyness in model
    # setnames(combined, names(combined), gsub('_from', '', names(combined)))
    # setnames(combined, names(combined), gsub('_to', '.1', names(combined)))

    combined$mscore = predict(m_rf, newdata=combined, type='prob')[, 2]
    # combined[persid==725 & persid.1 %in% c(725, 5608, 4103), ]

    combined[, rnk_from := rank(-mscore), by=list(year_to, persid_from)]
    combined[, rnk_to := rank(-mscore), by=list(persid_to)]

    # combined[persid==9400 & rnk==1, list(mfirst, mlast, year, year.1, rnk, mscore)]
    # combined[(persid==9400 | persid==9925) & rnk==1, list(mfirst, mlast, year, year.1, rnk, mscore)]
    # combined[persid==9400 & rnk==1, list(lastnamemen, lastnamemen.1, year.1, rnk, mscore)]
    # combined[persid.1==9400, list(lastnamemen, lastnamemen.1, year.1, rnk, mscore)]

    # combined[rnk==1 & mscore > 0.5, list(persid, persid.1)]
    # combined[rnk==1 & rnk.1==1 & mscore > 0.5, list(persid, persid.1)]
    out = combined[rnk_to==1 & rnk_from==1 & mscore > 0.5, list(persid_from, persid_to)]
    # out = combined[rnk==1 & mscore > 0.5, list(persid, persid.1)]

    # ties?

    # add original persid back into persid group
    out = out[, c(unique(persid_from), persid_to), by=persid_from]
    # setnames(out, names(out), c('persid', 'persid.1'))
    # careful, used to be different order
    # though at same times pairs should always hold
    # setnames(out, names(out), c('persid_to', 'persid_from'))
    setnames(out, names(out), c('persid_from', 'persid_to'))

    mtchlist[[as.character(yr)]] = out
    cat(yr, '\n')
}

expand_index = function(dat){ 

    # if all candidate not yet indexed, assign index from persid
    dat[!is.na(index_candidate), allna := all(is.na(index)), by=index_candidate]
    dat[!is.na(index_candidate) & allna == T, index:=index_candidate]

    # if all candidate indexed, do nothing
    dat[!is.na(index_candidate), allpresent:=!anyNA(index), by=index_candidate]
    dat[allpresent==T, index:=index] # just do nothing?

    # if candidate has more than one index and any NA, set old index
    dat[, bridge:=anyNA(index) & length(unique(na.omit(index)))==1, by=index_candidate]
    dat[!is.na(index_candidate) & is.na(index) & bridge==T, index:=unique(na.omit(index)), by=index_candidate]

    return(dat)
}
opg[, index := NULL]
opg[, index2 := NULL]
opg[, index_candidate := NULL]
# mtchlist[[1]]$persid[match(opg$persid, mtchlist[[1]]$persid.1)]

opg[match(mtchlist[[1]]$persid_to, persid), index := mtchlist[[1]]$persid_from]
opg[index %in% sample(index[!is.na(index)], 1), list(mfirst, mlast, wfirst, wlast, year)]


# seed with index where persid in original matched to rest
# opg[, index := mtchlist[[1]][, persid[match(opg[, persid_from], mtchlist[[1]][, persid_to])]]]
# opg[, index := mtchlist[[1]][, persid_from[match(opg[, persid], mtchlist[[1]][, persid_from])]]]

opg[, fromyear := as.character(NA)]
# opg[, fromyear := as.character(fromyear)]
opg[, frompersid := as.integer(NA)]
all(opg[year==1828, persid == index], na.rm=T)
opg[year==1828, index := persid]

for (i in 2:length(mtchlist)){
    cat("Indexed: ", sum(!is.na(opg$index)), '\n')
    opg[, index_candidate := as.integer(NA)]
    opg[match(mtchlist[[i]]$persid_to, persid), index_candidate := mtchlist[[i]]$persid_from]
    # opg[, index_candidate := mtchlist[[i]][, persid[match(opg[, persid_from], mtchlist[[i]][, persid_to])]]]
    # also add the necessary persid somehow...
    # note that persid.1 can be duplicated because dupl killing is within persid AND persid.1


    # opg$index_candidate = mtchlist[[i]]$persid[match(opg$persid, mtchlist[[i]]$persid.1)]

    # add a from somehow?
    opg = expand_index(opg)
    opg[index %in% persid & is.na(frompersid), fromyear := names(mtchlist)[i]]
    opg[index %in% persid & is.na(frompersid), frompersid := index]
}

sum(!is.na(opg$index))
dim(opg)

naind = opg[is.na(index), persid]
opg[index %in% naind, ] # should be none
opg[persid %in% naind, ]
opg[is.na(index), index := persid]

opg[, len := 1L]
opg[!is.na(index), len:=length(persid), by=index]

# any multiple years in indices?
opg[!is.na(index), ][, duplicated(year), by=index][V1==T, ]
# impossible linking
opg[len > 35, ]
opg[len > 22, ]

# test a few
opg[len == 4, ][index %in% sample(index[!is.na(index)], 1), list(mfirst, mlast, wfirst, wlast, year, fromyear, len)]
opg[len > 20, ][index %in% sample(index[!is.na(index)], 1), list(mfirst, mlast, wfirst, wlast, year, fromyear, len)]

opg[len == 1 & year == 1828, ][index %in% sample(index[!is.na(index)], 2), list(mfirst, mlast, wfirst, wlast, year, old, young, index)]
opg[len > 3, ][index %in% sample(index[!is.na(index)], 2), list(mfirst, mlast, wfirst, wlast, year, old, young, index)]
out = opg[len > 3, ][index %in% sample(index[!is.na(index)], 2), list(mfirst, mlast, wfirst, wlast, year, old, young, index)]

out = opg[index %in% c(1012, 1301, 1478, 1032), list(year, mfirst, mlast, wfirst, wlast, old, index)]
out = out[, .SD[1:2], by=index][order(-year, mlast)]
out = out[, lapply(.SD, as.character)]
out = out[, lapply(.SD, tolower)]
out[index == 1032 | is.na(year), ] = '...'
out[, `...` := '...']
print(xtable::xtable(out[, -"index"],
        caption = "Example records from Graaff Reinet opgaafrollen"), 
    include.rownames=F, size="footnotesize", 
    file="examplerecords.tex")

# any made from more than one year?
opg[len > 1, length(unique(fromyear)), by=index][order(V1)]

table(opg$len)
obs_at_least_length = cumsum(rev(table(opg$len)))
ind_at_least_length = obs_at_least_length / as.numeric(names(obs_at_least_length))
obs_at_least_length
ind_at_least_length

dotchart(obs_at_least_length)

pdf("cumulativelinks.pdf")
plot(as.numeric(names(ind_at_least_length)), ind_at_least_length,
    log='y', ylab='N series', xlab='Series length >=', type='b', col=2)
dev.off()

plot(ind_at_least_length,  -as.numeric(names(ind_at_least_length)), 
    log='x', yaxt='n', xlab='N series', ylab='Series length ≥')
axis(side = 2, at = -(1:max(as.numeric(names(ind_at_least_length)))),
    labels = 1:max(as.numeric(names(ind_at_least_length))))
abline(h=-c(1:33), lty=3, col='gray70')

opg[index==1, ]
opg[index==3437, ]
data.frame(opg[year==1787, ][order(mlast), list(mfirst, mlast)])

outfile = gzfile("opg_doublelinked_2017apr18.csv.gz", 'w')
write.csv(opg, outfile)
close(outfile)


opg[index==sample(index, 1), ]
opg[!is.na(index), len:=length(year), by=index]
opg[index==sample(opg[len > 30, index], 1), ]


hist(opg[!is.na(index), length(year), by=index]$V1)
table(opg[!is.na(index), length(year), by=index]$V1)
cumsum(table(opg[!is.na(index), length(year), by=index]$V1))
opg[len > 1, sum(!is.na(index))]
opg[len == 1, sum(!is.na(index))]

sum(!is.na(opg$index))
opg$index_candidate = mtchlist[[2]]$persid[match(opg$persid, mtchlist[[2]]$persid.1)]
opg = expand_index(opg)
sum(!is.na(opg$index))
opg$index_candidate = mtchlist[[3]]$persid[match(opg$persid, mtchlist[[3]]$persid.1)]
opg = expand_index(opg)
sum(!is.na(opg$index))


test = data.table::fread('roltest.csv')
test$index_candidate = test$index_cnd
test = expand_index(test)

opg$index_candidate = mtchlist[[2]]$persid[match(opg$persid, mtchlist[[2]]$persid.1)]

opg[, allna:=all(is.na(index)), by=index_candidate]
unique(opg[allna==T, list(index, index2)])
opg[allna==T, index2:=index_candidate]

opg[, allpresent:=!anyNA(index), by=index_candidate]
unique(opg[allpresent==T, list(index, index2)])
opg[allpresent==T, index2:=index] # just do nothing?

opg[, bridge:=!is.na(index_candidate) & is.na(index),]
opg[bridge==T, index:=unique(index), by=index_candidate]
opg[index_candidate==1602, ]

test = data.table::fread('roltest.csv')

test[, allna:=all(is.na(index)), by=index_cnd]
test[allna==T, index:=index_cnd]
test[, allpresent:=!anyNA(index), by=index_cnd]
test[allpresent==T, index:=index] # just do nothing?
test[, anyna:=anyNA(index), by=index_cnd]
test[anyna==T, index:=unique(na.omit(index))]
all(test$index, test$index_des)




test[, bridge:=!is.na(index_cnd) & is.na(index),]
test[bridge==T, index:=unique(na.omit(index)), by=index_cnd]

test[, bridge:=is.na(index), by=index_cnd]




# this does not consider whether index exceeds index_candidate
opg[, allpres:=!anyNA(index), by=index_candidate]
opg[allpres==T, ][index==sample(index, 1), ]


opg[index_candidate==2891, ]
opg[index==198, ]

opg[!is.na(index_candidate), index2:=ifelse(all(is.na(index)), index_candidate, index), by=index_candidate]
opg[is.na(index), ][index2==sample(na.omit(index2), 1), ]
opg[!is.na(index) & (index2!=index), ][order(index2), ]
opg[index2==198, ]

opg[year==1826, index:=persid]

# opg[, ifelse(anyNA(index) & all.equal(index[!is.na(index)]), 'fill', 'leave'), by=index_cnd]
opg[, index := ifelse(anyNA(index) & !anyDuplicated(index), index, NA), by=index_cnd]
opg[!is.na(index_cnd), bridge := ifelse(anyNA(index) & !anyDuplicated(index), 'bridge', 'no bridge'), by=index_cnd]

opg[index==725, ]
opg[index_cnd==2221, ]
mtchlist[[1]][persid==725, ]
combined[persid==725 & persid.1 %in% c(725, 5608, 4103), ]

predict(m_rf, )

opg[is.na(index) & !is.na(index_cnd), ]

opg[, index:=ifelse(all(is.na(index)), index_cnd, index), by=index]

opg[!is.na(index) & !is.na(index_cnd), ]

opg[index== 1194, ]
opg[index_cnd== 1605, ]
opg[index_cnd== 1602, ]
opg[index_cnd== 2339, ]
opg[index== 496, ]
opg[is.na(index), ]
opg

if index NA
new index from new baseyear
if index not NA 
check if new index has new links
connect if true


combined = candidates(opg, baseyear=1828)
combined = score(combined)

# fix this nastyness in model
setnames(combined, names(combined), gsub('_from', '', names(combined)))
setnames(combined, names(combined), gsub('_to', '.1', names(combined)))

combined$mscore = predict(m_rf, newdata=combined, type='prob')[, 2]
combined[, rnk := rank(-mscore), by=list(year.1, persid)]

mtchmt = combined[rnk==1 & mscore > 0.5, list(persid, persid.1)]

opg$index = mtchmt$persid[match(opg$persid, mtchmt$persid.1)]
opg[year==1828, index:=persid]




opg[!is.na(index), len:=length(persid), by=index]




opg_oneyear = opg[year==1828, ]
opg_rest    = opg[year!=1828, ]

system.time(m <- stringdistmatrix(opg_oneyear$mlast, opg_rest$mlast, method='jw'))

# x = apply(m, 1, function(x) x[x < 0.15])
x = apply(m, 1, function(x) which(x < 0.15))

opg_tomerge = opg_rest[unlist(x), ]
opg_tomerge[, linked_to := rep(opg_oneyear$persid, times=sapply(x, length))]
opg_oneyear[, linked_from := persid]

combined = merge(opg_oneyear, opg_tomerge, all=T, by.x='linked_from', by.y='linked_to', suffixes=c('', '.1'))


combined = score(combined)

combined$mscore = predict(m_rf, newdata=combined, type='prob')[, 2]
# predict(m_rf, newdata=combined[1:5, ], type='prob')
# ParallelForest for speed?

combined[, rnk := rank(-mscore), by=list(year.1, persid)]

vrbs = c('persid', 'mlast', 'mfirst', 'wfirst', 'wlast', 'year',
         'mlast.1', 'mfirst.1', 'wfirst.1', 'wlast.1', 'year.1',
         'mscore')

hist(combined[rnk==1, length(year), by=persid]$V1)

combined[rnk==1, ][order(persid, year)]
# set some minimum as well

mtchmt = combined[rnk==1 & mscore > 0.5, list(persid, persid.1)]

opg$index = mtchmt$persid[match(opg$persid, mtchmt$persid.1)]
opg[year==1828, index:=persid]
opg[!is.na(index), len:=length(persid), by=index]



opg[len > 30, ]
opg[index==137, ]
hist(opg[!is.na(index), length(persid), by=index]$V1)


opg[index==1, ]
opg[index==3, ]

outfile = gzfile("opg_doublelinked.csv.gz", 'w')
write.csv(opg, outfile)
close(outfile)


opg$index = mtchmt$persid[match(mtchmt$persid.1, opg$persid)]

opg[, index := mtchmt[match(persid, mtchmt$persid.1), persid]]

data.table(opg[])

combined[rnk==1, ][duplicated(paste0(persid, year.1)), ]
# surely some ties in rank

combined[persid==1, c(vrbs, 'rnk'), with=F]
combined[persid==2, c(vrbs, 'rnk'), with=F]
combined[persid==3, c(vrbs, 'rnk'), with=F]
combined[rnk==1, ][persid==3, vrbs, with=F]
combined[rnk==1, ][persid==3, vrbs, with=F]

combined[rnk==1, ][persid==sample(persid, 1), vrbs, with=F]
combined[rnk==1, ][persid==77, list(mlast, wlast, mlast.1, wlast.1, year.1, mscore, best)]

combined[, list(V1.1[which.min(mscore)], mlast[which.min(mscore)], mlast.1[which.min(mscore)]), by=list(year.1, persid)]

    # combined$oscore <- ifelse(combined$wifepresent, (sum(mwgts) * combined$mscore + sum(wwgts) * combined$wscore) / (sum(mwgts) + sum(wwgts)), combined$mscore)

system.time(combined_scored = score(combined))

system.time(stringdist(combined$mlast, combined$i.mlast, method='jw', p=0.1))
system.time(combined[, wlastdist:=stringdist(mlast, i.mlast, method='jw', p=0.1)])