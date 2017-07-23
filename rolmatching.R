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

    combined$mscore = predict(m_rf, newdata=combined, type='prob')[, 2]
    # combined[(wifepresent_from | wifepresent_to), mscore := predict(m_rf_yeswf, newdata=combined[(wifepresent_from | wifepresent_to), ], type='prob')[, 2]]
    # combined[!(wifepresent_from | wifepresent_to), mscore := predict(m_rf_nowf, newdata=combined[!(wifepresent_from | wifepresent_to), ], type='prob')[, 2]]

    combined[, rnk_from := rank(-mscore), by=list(year_to, persid_from)]
    combined[, rnk_to := rank(-mscore), by=list(persid_to)]

    out = combined[rnk_to==1 & rnk_from==1 & mscore > 0.5, list(persid_from, persid_to)]

    out = out[, list(persid_to = c(unique(persid_from), persid_to)), by=persid_from]

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

opg[match(mtchlist[[1]]$persid_to, persid), index := mtchlist[[1]]$persid_from]
opg[index %in% sample(index[!is.na(index)], 1), list(mfirst, mlast, wfirst, wlast, year)]


# seed with index where persid in original matched to rest
# opg[, index := mtchlist[[1]][, persid[match(opg[, persid_from], mtchlist[[1]][, persid_to])]]]
# opg[, index := mtchlist[[1]][, persid_from[match(opg[, persid], mtchlist[[1]][, persid_from])]]]

opg[, fromyear := as.character(NA)]
opg[, frompersid := as.integer(NA)]
all(opg[year==1828, persid == index], na.rm=T)
opg[year==1828, index := persid]

for (i in 2:length(mtchlist)){
    cat("Indexed: ", sum(!is.na(opg$index)), '\n')
    opg[, index_candidate := as.integer(NA)]
    opg[match(mtchlist[[i]]$persid_to, persid), index_candidate := mtchlist[[i]]$persid_from]
    # opg[, index_candidate := mtchlist[[i]][, persid[match(opg[, persid_from], mtchlist[[i]][, persid_to])]]]
    # also add the necessary persid somehow...


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


plot(ind_at_least_length,  -as.numeric(names(ind_at_least_length)), 
    log='x', yaxt='n', xlab='N series', ylab='Series length ≥')
axis(side = 2, at = -(1:max(as.numeric(names(ind_at_least_length)))),
    labels = 1:max(as.numeric(names(ind_at_least_length))))
abline(h=-c(1:33), lty=3, col='gray70')

opg[index==1, ]
opg[index==3437, ]
data.frame(opg[year==1787, ][order(mlast), list(mfirst, mlast)])

outfile = gzfile("opg_doublelinked_2017jul22.csv.gz", 'w')
write.csv(opg, outfile)
close(outfile)
