rm(list=ls())
setwd('~/dropbox/opgaafrol/')
options(stringsAsFactors=FALSE)

library("stringdist")

source('rolfunctions.r')
source('roldata.r')
source('rolmodels.r')

opg <- opg[order(-opg$year), ]
years <- unique(opg$year)

datlist <- list()
for (i in 2:length(years)){
# for (year in data.frame(combn(years, 2))){
    # cat(year[1], year[2], '\n\n')
    print(i)
    dat_y1 <- opg[opg$year==years[i - 1], ]
    dat_y2 <- opg[opg$year==years[i], ]

    dat_y12 <- strdistcombine(dat_y1, dat_y2)
    dat_y12 <- score(dat_y12)

    dat_y12$mpred <- predict(m_rf, newdata=dat_y12, type='response')
    votes <- predict(m_rf, newdata=dat_y12, type='prob')
    dat_y12$mscore <- votes[, 2]

    dat_y12_mtchd <- dat_y12[dat_y12$mscore > 0.5, ]

    firstpass <- do.call(rbind, lapply(split(dat_y12_mtchd, dat_y12_mtchd$persid), function(dat) dat[which.min(dat$mscore), ]))
    secondpass <- do.call(rbind, lapply(split(firstpass, firstpass$persid.1), function(dat) dat[which.min(dat$mscore), ]))

    # firstpass <- do.call(rbind, lapply(split(dat_y12, dat_y12$persid), function(dat) dat[which.min(dat$oscore), ]))
    # secondpass <- do.call(rbind, lapply(split(firstpass, firstpass$persid.1), function(dat) dat[which.min(dat$oscore), ]))

    out_y1 <- data.frame(dat_y1[, ], secondpass[match(dat_y1$persid, secondpass$persid), 8:ncol(secondpass)])

    cat(years[i:(i - 1)], 'matched:')
    cat(round(sum(!is.na(out_y1$persid.1)) / length(out_y1$persid.1), 2))
    cat('\n-----\n')

    nm <- paste('dat', years[i - 1], years[i], sep='-')
    datlist[[nm]] <- out_y1
}

matchmat <- merge(datlist[[1]][, c('persid', 'persid.1')], 
                  datlist[[2]][, c('persid', 'persid.1')], 
                by.x='persid.1', by.y='persid', all=T)
for (i in 3:length(datlist)){
    rks <- rank(sapply(matchmat, function(x) min(range(x, na.rm=T))))
    names(matchmat) <- paste0('persid', rks)
    matchmat <- merge(matchmat, 
        datlist[[i]][, c('persid', 'persid.1')], 
        by.x=paste0('persid', max(rks)), by.y='persid', all=T)    
}

x = apply(matchmat, 1, function(x) x[!is.na(x)])
names(x) = 1:length(x)
x = unlist(x)
names(x) = gsub('\\..*', '', names(x))
opg$index = names(x)[match(opg$persid, x)]

opg = as.data.table(opg)
opg[index==sample(opg$index, 1), ]

opg[, len:=length(persid[!is.na(index)]), by=index]
hist(opg[, length(persid[!is.na(index)]), by=index][,V1])

opg[, ti:=1:length(persid), by=index]
opg[, first:=ti==1]
opg[, last:=ti==max(ti) & ti!=1, by=index]
opg[index==sample(index[len ==6], 1), ]


lasts = opg[last==TRUE, ]
firsts = opg[first==TRUE]
x = strdistcombine(lasts, firsts)
x = score(x)

x$mpred <- predict(m_rf, newdata=x, type='response')
votes <- predict(m_rf, newdata=x, type='prob')
x$mscore <- votes[, 2]

x_mtchd <- x[x$mscore > 0.5, ]
x_mtchd <- x_mtchd[x_mtchd$year.1 - x_mtchd$year > 1, ]

firstpass <- do.call(rbind, lapply(split(x_mtchd, x_mtchd$persid), function(dat) dat[which.min(dat$mscore), ]))
secondpass <- do.call(rbind, lapply(split(firstpass, firstpass$persid.1), function(dat) dat[which.min(dat$mscore), ]))

y = secondpass[secondpass$mscore > 0.7, ]
opg$index2 = opg$index
for (row in 1:nrow(y)){
    persids = y[row, c('persid', 'persid.1')]
    indices = opg$index[opg$persid %in% persids]
    opg$index2[opg$index %in% indices] = indices[1]
}

duplids = opg[, list(dupl=duplicated(year), index2), by=index2][dupl==T, index2]
opg[index2 %in% duplids, index2:=index]
opg[, len2:=length(persid[!is.na(index2)]), by=index2]

outfile = gzfile("opg_doublelinked.csv.gz", 'w')
write.csv(opg, outfile)
close(outfile)

par(mfrow=c(1, 1))
plot(cumsum(opg[len2 > 1, length(persid), by=len2][order(len2), V1]),
    type='l', log='y', bty='l', ylab='obs.', xlab='series length', 
    main='N. obs in series of at least length x')
lines(cumsum(opg[len > 1, length(persid), by=len][order(len), V1]), col=2)
legend('bottomright', legend=c('linked', '2x linked'), fill=c(2, 1))

# --------- notes -------#
# need to find (near) duplicated in each year 
# j: would you say true duplicates are more likely to be close to each other?

# ties
# commonness of names on year or overall?

# marriages and widowing
# exact (highly certain) and only match of mname should always be linked
# exact (highly certain) match of mname and marriage (not widowing, creates son danger)
# sr and jr might help 

# multiple year combinations?
# only if the average match length is poor

# passforward

# start fresh for the remaining 20%

# what if there is no wife in one year and in the other
# widow, old, young dummies

# are multiple exact matches a problem?

# see what happens when you NA some things
# you get NAs where you do have some matching info
# e.g. mfirst missing, mlast, wfirst, wlast present
# if there is a wife, she is likely to be recorded 
# gaining a wife more likely to losing a wife

# what should happen with no wives? 
# they create large score differences

# firstpass <- dat_y12[paste0(dat_y12$index, dat_y12$score) %in% paste0(lowscores[, 1], lowscores[, 2]), ] # keeps all duplicated mins
# but this misses out on some that have no score or something

# get some solution for empty cells

# stringdist between '' and any non-empty string is 1, too high?
# setting to NA loses all other information
# could set it to 0.5, don't know?

# how to use the information that it is already matched in previous years?
# first though: how to grow it