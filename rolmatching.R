rm(list=ls())
setwd('~/downloads/data/opgaafrol/')
options(stringsAsFactors=FALSE)
library(stringdist)

gregexprr <- function(pattern, string){
    # return all string matches of a regular expression
    # todo: check whether/how it work on multiple strings at once
    rgx <- gregexpr(pattern, string)
    out <- substring(string, rgx[[1]], rgx[[1]] + attr(rgx[[1]], 'match.length') - 1)
    return(out)
}

initials <- function(strings){
    intls <- gregexprr("^[A-z]|\\s[A-z]|[.][A-z]", strings)
    intls <- gsub('[ .]', '', intls)
    return(paste0(intls, collapse=''))
}

opg <- read.csv('fgvf15oct.csv')
tra <- read.csv('matched.csv')
dim(opg)

opg$lastnamemen <- gsub('\x86', 'u', opg$lastnamemen)
opg$lastnamemen <- gsub('\x83', 'e', opg$lastnamemen)
opg$lastnamewomen <- gsub('\x83', 'e', opg$lastnamewomen)

tools::showNonASCII(opg$lastnamemen)
tools::showNonASCII(opg$firstnamemen)
tools::showNonASCII(opg$lastnamewomen)
tools::showNonASCII(opg$firstnamewomen)

opg$mlast <- iconv(opg$lastnamemen, from='macroman', to='utf8')
opg$mfirst <- iconv(opg$firstnamemen, from='macroman', to='utf8')
opg$wlast <- iconv(opg$lastnamewomen, from='macroman', to='utf8')
opg$wfirst <- iconv(opg$firstnamewomen, from='macroman', to='utf8')

# rm 
opg$mfirst[grep("[^A-Z .]", opg$mfirst)]
opg$mfirst[grep("[^A-Z .]", opg$mfirst)] <- 
    sapply(opg$mfirst[grep("[^A-Z .]", opg$mfirst)], function(x)gregexprr(".*[^A-Z .]", x))
opg$mfirst[grep("[^A-Z .]", opg$mfirst)] <- 
    gsub("[^A-Z .]", "", opg$mfirst[grep("[^A-Z .]", opg$mfirst)])
opg$mfirst[grep("[^A-Z .]", opg$mfirst)]
# get rid of spaces?

opg[(grepl("^ *$", opg$mfirst) & grepl("^ *$", opg$mlast) 
    & grepl("^ *$", opg$wfirst) & grepl("^ *$", opg$wlast)), ] 
# set NA or drop?
# NA in stringdistmatrix returns NA
# strings <- c('johan', 'johann', NA, 'jimbo')
# stringdistmatrix(strings, method='jw')

opg[grep("^ *$", opg$mfirst), c('year', 'mlast', 'mfirst', 'wfirst', 'wlast', 'wid', 'settlerwomen')]
opg[grep("^ *$", opg$mlast), c('year', 'mlast', 'mfirst', 'wfirst', 'wlast', 'wid', 'settlerwomen')]
# settlerwomen should be one
# they should be moved to the men-columns
# opg[grepl("^ *$", opg$mfirst) & grepl("^ *$", opg$mlast), c('mlast', 'mfirst')] <- opg[grepl("^ *$", opg$mfirst) & grepl("^ *$", opg$mlast), c('wlast', 'wfirst')]

# or would it be better to set them to NA
# and then hope the match happens on the wife's name?

opg[opg$mfirst=='X', c('mfirst', 'mlast')]
opg$mfirst[opg$mfirst=='X'] <- NA
# set to NA? or maybe some funky string is better?

opg[grep("^ *$", opg$mfirst), c('mfirst', 'mlast')]
opg$mfirst[grep("^ *$", opg$mfirst)] <- NA

opg[opg$mlast=='X', c('mfirst', 'mlast', 'wfirst', 'wlast')]
opg[grep("^ *$", opg$mlast), c('mfirst', 'mlast')]
# X = illegible/faulty
# '' = no name in original

opg$minitials <- sapply(opg$mfirst, initials)
opg$winitials <- sapply(opg$wfirst, initials)

opg$wifepresent <- !(opg$wfirst=='' & opg$wlast=='')

# need to find (near) duplicated in each year 
# tricky because you need to do all that stuff with wifepresent etc. again

plot(tapply(opg$wifepresent, opg$year, function(x) sum(x)/length(x)), type='l')

opg <- opg[order(-opg$year), ]
rownames(opg) <- opg$persid <- 1:nrow(opg)
years <- unique(opg$year)


idvars <- c('mfirst', 'minitials', 'mlast', 
        'wfirst', 'winitials', 'wlast',
         'wifepresent', 'young', 'old', 'persid')
# write.csv(opg[opg$year==1828, idvars[1:10]], '~/desktop/opg1828.csv', row.names=F)
# write.csv(opg[opg$year==1826, idvars[1:10]], '~/desktop/opg1826.csv', row.names=F)

datlist <- list()

# for (year in data.frame(combn(years, 2))){
#     cat(year[1], year[2], '\n\n')
# }

# make this a function somehow?
for (i in 2:length(years)){
# for (i in 2:3){
    print(i)
    dat_y1 <- opg[opg$year==years[i - 1], ]
    dat_y2 <- opg[opg$year==years[i], ]

    # function datcombine
    lastmat <- stringdistmatrix(dat_y1$mlast, dat_y2$mlast, method='jw', p=0.1, useNames=TRUE)
    candidates <- apply(lastmat, 2, function(x) which(x < 0.15))
    candidates[lapply(candidates, length)==0] <- NA
    y1candidates <- unlist(candidates)
    y2positions <- rep(1:length(candidates), lapply(candidates, length))
    dat_y2$linkid[y2positions] <- y1candidates

    dat_y12 <- data.frame(dat_y1[y1candidates, idvars], dat_y2[y2positions, idvars])

    # function stringdists/makevrbs
    dat_y12$index <- y2positions
    dat_y12$mlastdist <- stringdist(dat_y12$mlast, dat_y12$mlast.1, method='jw', p=0.1)
    dat_y12$mfirstdist <- stringdist(dat_y12$mfirst, dat_y12$mfirst.1, method='jw', p=0.1)
    dat_y12$minidist <- stringdist(dat_y12$minitials, dat_y12$minitials.1, method='jw', p=0.1)
    dat_y12$wlastdist <- stringdist(dat_y12$wlast, dat_y12$wlast.1, method='jw', p=0.1)
    dat_y12$wfirstdist <- stringdist(dat_y12$wfirst, dat_y12$wfirst.1, method='jw', p=0.1)
    dat_y12$winidist <- stringdist(dat_y12$winitials, dat_y12$winitials.1, method='jw', p=0.1)

    dat_y12$mlastsdx <- stringdist(dat_y12$mlast, dat_y12$mlast.1, method='soundex')
    dat_y12$mfirstsdx <- stringdist(dat_y12$mfirst, dat_y12$mfirst.1, method='soundex')
    dat_y12$wlastsdx <- stringdist(dat_y12$wlast, dat_y12$wlast.1, method='soundex')
    dat_y12$wfirstsdx <- stringdist(dat_y12$wfirst, dat_y12$wfirst.1, method='soundex')

    dat_y12$mtchs <- rep(sapply(candidates, length), lapply(candidates, length))
    dat_y12$mtchs[is.na(y1candidates)] <- 0 # easier way?
    dat_y12$mtchs <- dat_y12$mtchs / max(dat_y12$mtchs, na.rm=T)
    dat_y12$exactmtch <- dat_y12$mfirst==dat_y12$mfirst.1 & dat_y12$mlast==dat_y12$mlast.1

    weights <- c(mlastdist=10, mfirstdist=6, minidist=2, 
                 winidist=1, wlastdist=5, wfirstdist=2.5,  
                 mlastsdx=4, mfirstsdx=2, wlastsdx=2, wfirstsdx=1,
                 mtchs=1)
    mwgts <- c(mlastdist=10, mfirstdist=6, minidist=2, 
                 mlastsdx=4, mfirstsdx=2), old=2, young=2)
    wwgts <- c(winidist=1, wlastdist=5, wfirstdist=2.5,  
                 wlastsdx=2, wfirstsdx=1)
    dat_y12$score <- rowSums(t(t(dat_y12[names(weights)]) * weights)) / sum(weights)

    dat_y12$mscore <- rowSums(t(t(dat_y12[names(mwgts)]) * mwgts)) / sum(mwgts)
    dat_y12$wscore <- rowSums(t(t(dat_y12[names(wwgts)]) * wwgts)) / sum(wwgts)
    dat_y12$oscore <- ifelse(dat_y12$wifepresent, (sum(mwgts) * dat_y12$mscore + sum(wwgts) * dat_y12$wscore) / (sum(mwgts) + sum(wwgts)), dat_y12$mscore)
 
    # aggregate(dat_y12$score, by=list(dat_y12$index), function(x) x[which.min(x)])

    firstpass <- do.call(rbind, lapply(split(dat_y12, dat_y12$persid), function(dat) dat[which.min(dat$oscore), ]))
    secondpass <- do.call(rbind, lapply(split(firstpass, firstpass$persid.1), function(dat) dat[which.min(dat$oscore), ]))

    out_y1 <- data.frame(dat_y1[, idvars], secondpass[match(dat_y1$persid, secondpass$persid), 8:ncol(secondpass)])

    cat(years[i:(i - 1)], 'matched:')
    cat(round(sum(!is.na(out_y1$persid.1)) / length(out_y1$persid.1), 2))
    cat('\n-----\n')

    nm <- paste('dat', years[i - 1], years[i], sep='-')
    datlist[[nm]] <- out_y1
}

for (dat in datlist){
    cat(sum(duplicated(dat$persid, incomparables=NA)), '\n')
    cat(sum(duplicated(dat$persid.1, incomparables=NA)), '\n\n---\n')
}

matchmat <- merge(datlist[[1]][, c('persid', 'persid.1')], 
    datlist[[2]][, c('persid', 'persid.1')], by.x='persid.1', by.y='persid', all=T)
for (i in 3:length(datlist)){
    rks <- rank(sapply(matchmat, function(x) min(range(x, na.rm=T))))
    names(matchmat) <- paste0('persid', rks)
    matchmat <- merge(matchmat, datlist[[i]][, c('persid', 'persid.1')], by.x=paste0('persid', max(rks)), by.y='persid', all=T)    
}
sum(duplicated(matchmat))

tb <- data.frame(table(rowSums(!is.na(matchmat))))
data.frame(rev(tb[, 1]), cumsum(rev(tb[, 2])))

sum(as.numeric(tb[,1]) * tb[,2])
dim(opg)
pdf('matchlengths.pdf')
hist(rowSums(!is.na(matchmat)))
dev.off()

matchdat <- do.call(rbind, datlist) # tweak a little to end up with the complete original dataset + matches

matchmat <- as.matrix(matchmat)
matchdat$index <- NA
opg$index <- NA
for (row in 1:nrow(matchmat)){
    matchdat$index[matchdat$persid %in% na.omit(matchmat[row, ])] <- row
    opg$index[opg$persid %in% na.omit(matchmat[row, ])] <- row
}
# write.csv(matchdat[, c(idvars, 'index')], 'mtchdopg.csv', row.names=F)
# go over that, more stringently

matchdat[matchdat$index==sample(matchdat$index, 1) & !is.na(matchdat$index), c(idvars, 'index', 'oscore', 'wscore', 'mscore')]
opg[opg$index==sample(opg$index, 1) & !is.na(opg$index), c('index', idvars)]

write.csv(opg[, c(idvars, 'index')], 'mtchdopg.csv', row.names=F)
write.csv(matchdat, '~/desktop/opg1828-1826.csv')

# panel index
# ties
# incorporate nowife dummy
# commonness of names
# second stricter round after first pass

# 

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

hist(nchar(opg$mfirst))
hist(nchar(opg$mlast))

rstring <- function(l=10){
    paste0(sample(letters, size=l, replace=T), collapse='')
}
stringdist(rstring(), rstring(), method='jw')

N <- 1e4
fill <- numeric(N)
for (i in 1:N){
    x <- stringdist(rstring(), rstring(), method='jw')
    fill[i] <- x
}
conv <- cumsum(fill) / 1:length(fill)
plot(conv, type='l')

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