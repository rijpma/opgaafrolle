score = function(dat){
    stopifnot(data.table::is.data.table(dat))

    dat[, mlastdist := stringdist(mlast_from, mlast_to, method='jw', p=0.1)]
    dat[, mfirstdist := stringdist(mfirst_from, mfirst_to, method='jw', p=0.1)]
    dat[, minidist := stringdist(minitials_from, minitials_to, method='jw', p=0.1)]
    dat[, wlastdist := stringdist(wlast_from, wlast_to, method='jw', p=0.1)]
    dat[, wfirstdist := stringdist(wfirst_from, wfirst_to, method='jw', p=0.1)]
    dat[, winidist := stringdist(winitials_from, winitials_to, method='jw', p=0.1)]

    # somewhat expensive and hardly used
    dat[, mlastsdx := stringdist(mlast_from, mlast_to, method='soundex')]
    dat[, mfirstsdx := stringdist(mfirst_from, mfirst_to, method='soundex')]
    dat[, wlastsdx := stringdist(wlast_from, wlast_to, method='soundex')]
    dat[, wfirstsdx := stringdist(wfirst_from, wfirst_to, method='soundex')]

    dat[, nrdist := nr_from - nr_to]
    dat[, wifeinboth := wifepresent_from == wifepresent_to]
    dat[, samedistrict := districtall_from == districtall_to]
    dat[, bothwineprod := wineproducer_from == wineproducer_to]
    # stay open

    dat[, mtchs := length(year_from), by=persid_from]

    dat[, exactmtch := mfirst_from==mfirst_to & mlast_from==mlast_to]

    # weights <- c(mlastdist=10, mfirstdist=6, minidist=2, 
    #              winidist=1, wlastdist=5, wfirstdist=2.5,  
    #              mlastsdx=4, mfirstsdx=2, wlastsdx=2, wfirstsdx=1,
    #              mtchs=1)
    # mwgts <- c(mlastdist=10, mfirstdist=6, minidist=2, 
    #              mlastsdx=4, mfirstsdx=2) #, old=2, young=2)
    # wwgts <- c(winidist=1, wlastdist=5, wfirstdist=2.5,  
    #              wlastsdx=2, wfirstsdx=1)

    # dat[, score:= rowSums(.SD * weights) / sum(weights), .SDcols=names(weights)]
    # dat[, score:= rowSums(.SD * mwgts) / sum(mwgts), .SDcols=names(mwgts)]
    # dat[, score:= rowSums(.SD * wwgts) / sum(wwgts), .SDcols=names(wwgts)]

    return(dat)
}


candidates = function(dat, baseyear){
    stopifnot(data.table::is.data.table(dat))

    dat_firstyear = dat[year==baseyear, ]
    dat_rest      = dat[year < baseyear, ]

    stopifnot(nrow(dat_firstyear) > 0)
    stopifnot(nrow(dat_rest) > 0)

    distmat <- stringdist::stringdistmatrix(dat_firstyear$mlast, dat_rest$mlast, 
        method='jw', p=0.1)

    candidate_list = apply(distmat, 1, function(x) which(x < 0.15))
    # names(candidate_list) = dat_firstyear$persid

    # distmat[725, ]
    # dat_rest[candidate_list[[725]], list(persid, mlast)][persid %in% c(5608, 4103), ]

    dat_tomerge = dat_rest[unlist(candidate_list), ]
    dat_tomerge[, linked_to     := rep(dat_firstyear$persid, times=sapply(candidate_list, length))]
    dat_firstyear[, linked_from := persid]

    # dat_tomerge[linked_to==725, ][persid %in% c(5608, 4103), ]
    # dat_firstyear[linked_from==725, ]

    combined = merge(dat_firstyear, dat_tomerge, all=T, 
        by.x='linked_from', by.y='linked_to', suffixes=c('_from', '_to'))

    # combined[persid_from==725, ][persid_to %in% c(5608, 4103), ]

    return(combined)
}

gregexprr <- function(pattern, string){
    # return all string matches of a regular expression
    # todo: check whether/how it work on multiple strings at once

    rgx <- gregexpr(pattern, string)
    out <- substring(string, rgx[[1]], rgx[[1]] + attr(rgx[[1]], 'match.length') - 1)
    return(out)
}

initials <- function(strings){
    # return first letter of each word in a string

    intls <- gregexprr("^[A-z]|\\s[A-z]|[.][A-z]", strings)
    intls <- gsub('[ .]', '', intls)
    return(paste0(intls, collapse=''))
}

modperf <- function(yhat, y){
    sens <- sum(y[yhat==y])
    spec <- sum(!yhat[yhat==y])
    fane <- sum(!yhat[yhat!=y])
    fapo <- sum(yhat[yhat!=y])
    out <- data.frame(spec, fane, fapo, sens)
    return(out)
}

glmperf <- function(cutoff, mod, y){
    yhat <- mod$fit > cutoff
    w <- which(y==1)
    sens <- mean(yhat[w] == 1)
    spec <- mean(yhat[-w] == 0)
    dstnc <- sqrt((sens - 1)^2 + (spec - 1)^2)
    corclsf <- mean(y==yhat)
    out <- t(as.matrix(c(sens, spec, dstnc, corclsf)))
    return(out)
}

strdistcombine <- function(dat_y1, dat_y2, mtchvrb1='mlast', mtchvrb2='mlast', ...){
    lastmat <- stringdistmatrix(dat_y1[[mtchvrb1]], dat_y2[[mtchvrb2]], method='jw', p=0.1, useNames=TRUE, ...)
    candidates <- apply(lastmat, 2, function(x) which(x < 0.15))
    candidates[lapply(candidates, length)==0] <- NA
    y1candidates <- unlist(candidates)
    y2positions <- rep(1:length(candidates), lapply(candidates, length))
    dat_y2$linkid[y2positions] <- y1candidates

    dat_y12 <- data.frame(dat_y1[y1candidates, ], dat_y2[y2positions, ])

    dat_y12$mtchs <- rep(sapply(candidates, length), lapply(candidates, length))
    dat_y12$mtchs[is.na(y1candidates)] <- 0 # easier way?
    dat_y12$mtchs <- dat_y12$mtchs / max(dat_y12$mtchs, na.rm=T)

    return(dat_y12)
}

closeindex <- function(stringvrbs, cutoff=0.1){
    # return index of similar strings
    # multiple string variables permitted, weighted equally
    # cutoff bit arbitrary

    stopifnot(class(stringvrbs)=='list')

    N <- length(stringvrbs[[1]])
    distmat <- matrix(0, nrow=N, ncol=N)
    for (vrb in stringvrbs){
        distmat <- distmat + stringdistmatrix(vrb, vrb, method='jw', p=0.1)
    }
    
    candidates <- apply(distmat, 2, function(x) which(x < cutoff))
    out <- 1:N
    for (i in seq_along(candidates)){
        out[candidates[[i]][candidates[[i]] > i]] <- i
    }

    return(out)
}

smplseries <- function(dat, index, len=NULL){
    lens <- tapply(index, index, length)[index]
    if (is.null(len)){
        len <- sample(1:max(lens, na.rm=T), 1)
    }
    dat[index==sample(index[dat$len %in% len], 1) & !is.na(index), ]
}

uniformise_string <- function(string, maxdist=0.2, quiet=FALSE){
    str_srtd <- names(sort(-table(string)))
    n_start <- length(str_srtd)
    strmat <- stringdistmatrix(str_srtd, str_srtd, method='jw', p=0.1)
    fill <- NULL
    while(nrow(strmat) > 0){
        ind <- strmat[1, ] < maxdist
        similar_strs <- str_srtd[ind]
        str_srtd <- str_srtd[!ind]
        strmat <- strmat[!ind, !ind, drop=FALSE]
        string[string %in% similar_strs] <- similar_strs[1]
        if (length(similar_strs) > 1 & !quiet){
            cat(similar_strs, sep=', ')
            cat('----->')
            cat(similar_strs[1], '\n')
        }
    }
    cat('From', n_start, ' to ', length(unique(string)), '\n')
    return(string)
}
