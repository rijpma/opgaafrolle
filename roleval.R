setwd("~/dropbox/opgaafrol")
library("data.table")
library("plm")
mar = c(4, 4, 2.5, 0.5)


source("rolfunctions.r")

# links = data.table::fread("zcat < opg_doublelinked_2017apr18.csv.gz")
links = fread("gunzip -c opg_doublelinked_2017jul22.csv.gz")

opg = data.table::fread('fgvf15oct.csv', sep=',')

opg[, 15:64] = opg[, lapply(.SD, as.numeric), .SDcols = 15:64]

dim(links)
dim(opg)

all.equal(links$id, opg$id)

opg[, index := links$index]

opg[, rindex := sample(index), by = year]

yrs = unique(opg$year)
prbs = list()
for (i in 2:length(yrs)){
    prb = sum(opg[year == yrs[i], index] %in% opg[year == yrs[i - 1], index]) / nrow(opg[year == yrs[i - 1]])
    prbs[[as.character(yrs[i])]] = prb
    # oldindex = opg[year == yrs[i - 1], index]
    # n_old = nrow(opg[year == yrs[i - 1]])
    # n_new = nrow(opg[year == yrs[i]])
    # newindex = sample(oldindex, n_old * prb)
    # newindex = c(newindex, )
    # opg[year == yrs[i], rindex := sample(oldindex, floor(n_old * prb))]
}
plot(unlist(prbs))

set.seed(4302)
nlinks = links[, sum(len >= 2)]
links[, max(index)]
opg[, rindex := sample(1:25000), by = year]
opg[, rlen := .N, by = rindex]
opg[, mean(rlen >= 2)]
links[, mean(len >= 2)]
hist(table(opg$rindex))
hist(table(opg$index))
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
quantile(opg$len)

pdf("linkpaper/distrbylinklen_log1p.pdf")
par(mfrow = c(2, 2), mar = mar, font.main = 1)
overall = density(log1p(opg[!is.na(cattle), cattle]))
for (i in list(c(0, 1), c(1, 8), c(8, 15), c(15, 32))){
    plot(overall, main = paste0(i[1], " > x >= ", i[2]), col='pink', xlab='cattle', ylim = range(overall$y) * 1.1)
    lines(density(log1p(opg[!is.na(cattle) & len > i[1] & len <= i[2], cattle])), col = 'red')
}
legend('topright', fill=c('pink', 'red'), legend=c('all', 'subset'))
dev.off()


opg[, wifepresent := links$wifepresent]
opg[, round(sum(wifepresent) / length(wifepresent), 2), by=len][order(len)]
plot(density(log1p(opg[!is.na(cattle) & wifepresent == T, cattle])))
lines(density(log1p(opg[!is.na(cattle) & wifepresent == F, cattle])), col=2)


gnl = data.table::fread("genealogymatch1828.csv")

gnl[, .N, by = is.na(persid_to)]