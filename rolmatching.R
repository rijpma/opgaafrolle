rm(list=ls())
setwd('~/downloads/data/opgaafrol/')
options(stringsAsFactors=FALSE)
library(stringdist)

source('rolfunctions.r')
source('roldata.r')
source('rolmodels.r')

# need to find (near) duplicated in each year 
# j: would you say true duplicates are more likely to be close to each other?

opg <- opg[order(-opg$year), ]
years <- unique(opg$year)


# for (year in data.frame(combn(years, 2))){
#     cat(year[1], year[2], '\n\n')
# }

datlist <- list()
for (i in 2:length(years)){
    print(i)
    dat_y1 <- opg[opg$year==years[i - 1], ]
    dat_y2 <- opg[opg$year==years[i], ]

    dat_y12 <- strdistcombine(dat_y1, dat_y2)
    dat_y12 <- score(dat_y12)

    dat_y12$mlpred <- predict(m_rf, newdata=dat_y12, type='response')
    votes <- predict(m_rf, newdata=dat_y12, type='prob')
    dat_y12$mlscore <- apply(votes, 1, max)

    dat_y12_mtchd <- dat_y12[dat_y12$mlpred=="TRUE", ]

    firstpass <- do.call(rbind, lapply(split(dat_y12_mtchd, dat_y12_mtchd$persid), function(dat) dat[which.min(dat$mlscore), ]))
    secondpass <- do.call(rbind, lapply(split(firstpass, firstpass$persid.1), function(dat) dat[which.min(dat$mlscore), ]))

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

smplseries(matchdat, matchdat$index)

# order dataset by series with worst vote
matchdat$len <- tapply(matchdat$index, matchdat$index, length)[matchdat$index]
matchdat$worstvote <- tapply(matchdat$mlscore, matchdat$index, min, na.rm=T)[matchdat$index]
matchdat$worstvote[matchdat$len==1] <- 1.1
matchdat <- matchdat[order(matchdat$worstvote, matchdat$len, matchdat$index), ]

table(matchdat$len)

write.csv(matchdat[, c('index', 'persid', 'len',
                       'mfirst', 'minitials', 'mlast',
                       'wfirst', 'winitials', 'wlast',
                       'wifepresent', 'old', 'young',
                       'mlpred', 'mlscore', 'worstvote')], 'mtchseries.csv')


# write.csv(opg[, c(idvars, 'index')], 'mtchdopg.csv', row.names=F)
# write.csv(matchdat, '~/desktop/opg1828-1826.csv')


# --------- notes -------#
# ties
# commonness of names
# second stricter round after first pass

# marriages and widowing
# exact (highly certain) and only match of mname should always be linked
# exact (highly certain) match of mname and marriage (not widowing, creates son danger)
# sr and jr might help 

# MULTIPLE year combinations
# only if the average match length is poor

# commonness of name
# bin the names on string distance
# get a frequency

# make a nowife dummy
# give that a heigh weight

# passforward
# set c at t to 1 if there is a match between t and t -1
# set x at t to 1 if there is a match between t and t -1
# add x to c

# start fresh for the remaining 20%
# panel-id for the matches, get length of individual series
# exact match should not choose one at random but both

# what if there is no wife in one year and in the other
# widow, old, young dummies

# are multiple exact matches a problem?

# see what happens when you NA some things
# you get NAs where you do have some matching info
# e.g. mfirst missing, mlast, wfirst, wlast present
# you'd want that 
# could set it to one? though 0.6 seems to 
# if there is a wife, she is likely to be recorded 
# gaining a wife more likely to losing a wife

# what should happen with no wives? 
# they create large score differences

# firstpass <- dat_y12[paste0(dat_y12$index, dat_y12$score) %in% paste0(lowscores[, 1], lowscores[, 2]), ] # keeps all duplicated mins
# but this misses out on some that have no score or something

# remaining improvements:

# get some solution for empty cells

# stringdist between '' and any non-empty string is 1, too high?
# setting to NA loses all other information
# could set it to 0.5, don't know?

# the weighting of the score is pretty much random
# get a few hundred true observations 
# and see what weights come out?

# how to use the information that it is already matched in previous years?
# first though: how to grow it