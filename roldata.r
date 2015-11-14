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
