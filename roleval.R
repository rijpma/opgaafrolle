setwd("~/dropbox/opgaafrol")
library("data.table")
library("plm")
mar = c(4, 4, 2.5, 0.5)


source("rolfunctions.r")

links = data.table::fread("zcat < opg_doublelinked_2017apr18.csv.gz")
opg = data.table::fread('fgvf15oct.csv', sep=',')

opg[, 15:64] = opg[, lapply(.SD, as.numeric), .SDcols = 15:64]

dim(links)
dim(opg)

all.equal(links$id, opg$id)

opg[, index := links$index]

opg[, rindex := sample(index), by = year]

yrs = unique(opg$year)
for (i in 2:length(yrs)){
    prb = sum(opg[year == yrs[i], index] %in% opg[year == yrs[i - 1], index]) / nrow(opg[year == yrs[i - 1]])
    # oldindex = opg[year == yrs[i - 1], index]
    # n_old = nrow(opg[year == yrs[i - 1]])
    # n_new = nrow(opg[year == yrs[i]])
    # newindex = sample(oldindex, n_old * prb)
    # newindex = c(newindex, )
    # opg[year == yrs[i], rindex := sample(oldindex, floor(n_old * prb))]
}

opg[, index[year %in% shift(year)], by = year]

opg[, rindex := sample(1:3000), by = year]
opg[rindex %in% sample(rindex, 1), list(lastnamemen)]

table(opg[, length(year), by = rindex][, V1])

popg = plm::pdata.frame(opg, index=c('index', 'year'))
popg$lsettlerchildren = lag(popg$settlerchildren)
popg$lcattle = lag(popg$cattle)
fpopg = plm::pdata.frame(opg, index=c('rindex', 'year'))
fpopg$lsettlerchildren = lag(fpopg$settlerchildren)
fpopg$lcattle = lag(fpopg$cattle)

pdf("linkpaper/panelresults.pdf")
par(mfrow=c(2, 2), mar=mar)
smoothScatter(popg$lsettlerchildren, popg$settlerchildren, 
    nbin=256, colramp=colorRampPalette(magma(256)),
    xlab='lag(children)', ylab='children', main='Panel')
smoothScatter(fpopg$lsettlerchildren, fpopg$settlerchildren, 
    nbin=256, colramp=colorRampPalette(magma(256)),
    xlab='lag(children)', ylab='children', main='Fake panel')
smoothScatter(log1p(popg$cattle), log1p(popg$lcattle),
    nbin=256,  colramp=colorRampPalette(magma(256)),
    xlab='lag(log(cattle))', ylab='log(cattle)')
smoothScatter(log1p(fpopg$cattle), log1p(fpopg$lcattle),
    nbin=256,  colramp=colorRampPalette(magma(256)),
    xlab='lag(log(cattle)', ylab='log(cattle')
dev.off()

opg[, len := links$len]
pdf("linkpaper/distrbylinklen_log1p.pdf")
par(mfrow=c(2, 2), mar=mar)
for (i in list(c(0, 1), c(1, 5), c(5, 10), c(10, 22))){
    plot(density(log1p(opg[!is.na(cattle), cattle])), main = paste0(i[1], " > x >= ", i[2]), col='gray70', xlab='cattle')
    lines(density(log1p(opg[!is.na(cattle) & len > i[1] & len <=i[2], cattle])))
}
legend('topright', fill=c('gray70', 1), legend=c('all', 'subset'))
dev.off()

opg[, wifepresent := links$wifepresent]
opg[, round(sum(wifepresent) / length(wifepresent), 2), by=len][order(len)]
plot(density(log1p(opg[!is.na(cattle) & wifepresent == T, cattle])))
lines(density(log1p(opg[!is.na(cattle) & wifepresent == F, cattle])), col=2)