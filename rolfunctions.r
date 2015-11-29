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
    lastmat <- stringdistmatrix(dat_y1$mlast, dat_y2$mlast, method='jw', p=0.1, useNames=TRUE, ...)
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

score <- function(dat_y12){
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

    dat_y12$nrdist <- dat_y12$nr - dat_y12$nr.1
    dat_y12$wifeinboth <- dat_y12$wifepresent == dat_y12$wifepresent.1
    dat_y12$samedistrict <- dat_y12$districtall == dat_y12$districtall.1
    dat_y12$bothwineprod <- dat_y12$wineproducer == dat_y12$wineproducer.1
    # stay open

    dat_y12$exactmtch <- dat_y12$mfirst==dat_y12$mfirst.1 & dat_y12$mlast==dat_y12$mlast.1

    weights <- c(mlastdist=10, mfirstdist=6, minidist=2, 
                 winidist=1, wlastdist=5, wfirstdist=2.5,  
                 mlastsdx=4, mfirstsdx=2, wlastsdx=2, wfirstsdx=1,
                 mtchs=1)
    mwgts <- c(mlastdist=10, mfirstdist=6, minidist=2, 
                 mlastsdx=4, mfirstsdx=2) #, old=2, young=2)
    wwgts <- c(winidist=1, wlastdist=5, wfirstdist=2.5,  
                 wlastsdx=2, wfirstsdx=1)

    dat_y12$score <- rowSums(t(t(dat_y12[names(weights)]) * weights)) / sum(weights)

    dat_y12$mscore <- rowSums(t(t(dat_y12[names(mwgts)]) * mwgts)) / sum(mwgts)
    dat_y12$wscore <- rowSums(t(t(dat_y12[names(wwgts)]) * wwgts)) / sum(wwgts)
    dat_y12$oscore <- ifelse(dat_y12$wifepresent, (sum(mwgts) * dat_y12$mscore + sum(wwgts) * dat_y12$wscore) / (sum(mwgts) + sum(wwgts)), dat_y12$mscore)

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
    if (is.null(len)){
        len <- sample(1:max(dat$len, na.rm=T), 1)
    }
    dat[index==sample(index[dat$len %in% len], 1) & !is.na(index), 
        c('index', 'len', 'mscore',
          'mfirst', 'minitials', 'mlast',
          'wfirst', 'winitials', 'wlast')]
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
