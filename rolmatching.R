rm(list=ls())
setwd('~/dropbox/opgaafrol/')
options(stringsAsFactors=FALSE)

library(stringdist)
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
    datlist[[2]][, c('persid', 'persid.1')], by.x='persid.1', by.y='persid', all=T)
for (i in 3:length(datlist)){
    rks <- rank(sapply(matchmat, function(x) min(range(x, na.rm=T))))
    names(matchmat) <- paste0('persid', rks)
    matchmat <- merge(matchmat, datlist[[i]][, c('persid', 'persid.1')], by.x=paste0('persid', max(rks)), by.y='persid', all=T)    
}

# cumulative matchlengths
tb <- data.frame(table(rowSums(!is.na(matchmat))))
data.frame(rev(tb[, 1]), cumsum(rev(tb[, 2])))

sum(as.numeric(tb[,1]) * tb[,2])
dim(opg)
pdf('matchlengths.pdf')
hist(rowSums(!is.na(matchmat)), breaks=23)
dev.off()

matchdat <- do.call(rbind, datlist) # tweak a little to end up with the complete original dataset + matches

matchmat <- as.matrix(matchmat)
matchdat$index <- NA
opg$index <- NA
for (row in 1:nrow(matchmat)){
    matchdat$index[matchdat$persid %in% na.omit(matchmat[row, ])] <- row
    opg$index[opg$persid %in% na.omit(matchmat[row, ])] <- row
}
opg$len <- tapply(opg$index, opg$index, length)[opg$index]

smplseries(opg, opg$index)[, grep('last|first', names(opg))]

out <- cbind(opg_full, opg)
out <- out[order(out$index, out$year), ]
write.csv(out, 'opgaafrollen_lnkd.csv', row.names=F, na='.')
# works, but what about those last matches?
# they're in, score indeed
tail(x)


# order dataset by series with worst vote
matchdat$len <- tapply(matchdat$index, matchdat$index, length)[matchdat$index]
matchdat$worstvote <- tapply(matchdat$mscore, matchdat$index, min, na.rm=T)[matchdat$index]
matchdat$worstvote[matchdat$len==1] <- 1.1
matchdat <- matchdat[order(matchdat$worstvote, matchdat$len, matchdat$index), ]

smplseries(matchdat, matchdat$index)

table(matchdat$len)

write.csv(matchdat[, c('index', 'persid', 'len',
                       'mfirst', 'minitials', 'mlast',
                       'wfirst', 'winitials', 'wlast',
                       'wifepresent', 'old', 'young',
                       'mpred', 'mscore', 'worstvote')], 'mtchseries.csv')


# write.csv(opg[, c(idvars, 'index')], 'mtchdopg.csv', row.names=F)
# write.csv(matchdat, '~/desktop/opg1828-1826.csv')


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