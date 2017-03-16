require(data.table)
require(bit64)
require(stringr)

ipo <- fread("./IPO review chapter/Chapter write up/SDC-IPO-data/ipo_all_variables.csv")
setkey(ipo, Issue_date)
ipo[, Issue_date := as.Date(Issue_date)]

ipo[Year < 1980, cohort := "1973-1979"]
ipo[Year >= 1980 & Year < 1990, cohort := "1980-1989"]
ipo[Year >= 1990 & Year < 2000, cohort := "1990-1999"]
ipo[Year >= 2000 & Year <= 2010, cohort := "2000-2010"]

ipo[Year >= 1980 & Year <= 1989, cohortR := "1980-1989"]
ipo[Year >= 1990 & Year <= 1994, cohortR := "1990-1994"]
ipo[Year >= 1995 & Year <= 1998, cohortR := "1995-1998"]
ipo[Year >= 1999 & Year <= 2000, cohortR := "1999-2000"]
ipo[Year >= 2001 & Year <= 2014, cohortR := "2001-2014"]

load("./CRSP_COMP/CRSP_daily_1926_1989.rda")
load("./CRSP_COMP/CRSP_daily_1990_2015.rda")

### long-run returns
crsp_daily_1989 <- crsp_daily_1989[PERMNO %in% ipo$Permno,]
crsp_daily <- crsp_daily[PERMNO %in% ipo$Permno,]
crsp <- rbind(crsp_daily_1989, crsp_daily)
rm(crsp_daily, crsp_daily_1989)

match <- match(crsp$PERMNO, ipo$Permno)
crsp$IPO_date <- ipo$Issue_date[match]

setkey(crsp, PERMNO, date)
crsp[, RET := as.numeric(as.character(RET))]
crsp <- crsp[!is.na(RET)]
setkey(crsp, PERMNO)
crsp[, min_date := min(date), by = PERMNO]
crsp[, dif := as.numeric(as.character(date - min_date))]
crsp <- crsp[dif <= 365*3 & dif > 0] ### this is where I cut the sample

crsp[, ret3 := prod(1+RET, na.rm = T) - 1, by = PERMNO]
match <- match(ipo$Permno, crsp$PERMNO)
ipo$cumret3 <- crsp$ret3[match] ### here I get returns

### estimation of market returns
source("./R codes/wrds connect.R")
line <- "select date, vwretd, ewretd from CRSP.DSI where date between '01jan1966'd and '31dec2016'd"
res <- dbSendQuery(wrds, line)
data <- fetch(res, n = -1)

data <- as.data.table(data)
data[, DATE := as.Date(DATE)]
match <- match(crsp$date, data$DATE)
crsp$ewretd <- as.numeric(as.character(data$ewretd[match]))
crsp$vwretd <- as.numeric(as.character(data$vwretd[match]))

crsp[, ret_ew_3year := prod(1+ewretd, na.rm = T) - 1, by = PERMNO]
crsp[, ret_vw_3year := prod(1+vwretd, na.rm = T) - 1, by = PERMNO]

match <- match(ipo$Permno, crsp$PERMNO)
ipo$cumretew3 <- crsp$ret_ew_3year[match]
ipo$cumretvw3 <- crsp$ret_vw_3year[match]
ipo$wr_ew3 <- (1 + ipo$cumret3)/(1 + ipo$cumretew3)
ipo$wr_vw3 <- (1 + ipo$cumret3)/(1 + ipo$cumretvw3)

### size and bm portfolios
source("./R codes/wrds connect.R")
sql <- "select gvkey, fyear, fyr, pstkl, pstkrv, pstk, seq,ceq, at, lt, txditc, dcvt, consol, indfmt, datafmt, popsrc from COMPM.FUNDA where fyear between 1965 and 2015"
res <- dbSendQuery(wrds, sql)
comp <- fetch(res, n = -1)
comp <- as.data.table(comp)
comp <- comp[ fyr > 0 & at > 0]
comp[, year := fyear]
comp[fyr %in% 1:5, year := fyear + 1]
comp[, pstock := pstkl]
comp[is.na(pstock), pstock := pstkrv]
comp[is.na(pstock), pstock := pstk]
comp[is.na(pstock), pstock := 0] ### is it right?
comp[, se := seq]
comp[is.na(se)&!is.na(ceq)&!is.na(pstk), se := ceq + pstk]
comp[is.na(se), se := at - lt]
comp[is.na(txditc), txditc := 0]
comp[is.na(dcvt), dcvt := 0]
comp[, be := se - pstock + txditc + dcvt]

load("./CRSP_COMP/crsp_monthly_1970_2015.rda")
source("./R codes/wrds connect.R")
sql <- "select gvkey, lpermno from CRSPA.CCMXPF_LINKTABLE"
res <- dbSendQuery(wrds, sql)
link <- fetch(res, n = -1)
link <- as.data.table(link)
link <- link[!is.na(link$lpermno)]
crsp_monthly$gvkey <- link$gvkey[match(crsp_monthly$PERMNO, link$lpermno)]

comp[, id := paste(gvkey, year)]
comp[, lead.id := paste(gvkey, year+1)]
comp <- comp[!duplicated(id)]

crsp_monthly[, year := date %/% 10000]
crsp_monthly[, id := paste(gvkey, year)]
match <- match(crsp_monthly$id, comp$id)
crsp_monthly$be <- comp$be[match]
lead.match <- match(crsp_monthly$id, comp$lead.id)
ind <- which(is.na(crsp_monthly$be))
crsp_monthly$be[ind] <- comp$be[lead.match[ind]]

crsp_monthly[, marcap := SHROUT*abs(PRC)]
crsp_monthly[, ME := marcap/10^3]
crsp_monthly[, BM := be/ME]
crsp_monthly <- setDT(crsp_monthly)[, BM_q := cut(BM,
                                           quantile(BM, probs=0:5/5, na.rm = T),
                                           include.lowest=TRUE, labels=FALSE), by = year]

crsp_monthly <- setDT(crsp_monthly)[, ME_q := cut(ME,
                                                  quantile(ME, probs=0:5/5, na.rm = T),
                                                  include.lowest=TRUE, labels=FALSE), by = year]
crsp[, year := year(date)]
crsp[, id1:= paste(PERMNO, year)]
crsp_monthly[, id := paste(as.character(PERMNO), year)]
match <- match(crsp$id, crsp_monthly$id)
crsp$port_5ME <- crsp_monthly$ME_q[match]
crsp_monthly$port_25 <- (crsp_monthly$ME_q - 1)*5 + crsp_monthly$BM_q
crsp$port_25 <- crsp_monthly$port_25[match]

port25 <- fread("./CRSP_COMP/25_Portfolios_5x5_Daily.CSV", header = F)
port25 <- as.data.table(port25)
port25[, date := as.Date(as.character(V1), format = "%Y%m%d")]
port25[, year := year(date)]
port25 <- port25[ year > 1968]
port25 <- as.data.frame(port25)

for(i in 1:25)
{
  print(i)
  ind <- which(crsp$port_25 == i)
  print(length(ind))
  match <- match(crsp$date[ind],port25$date)
  crsp$ret_port25[ind] <- port25[match,i + 1]
}

port_size <- fread("./CRSP_COMP/Portfolios_Formed_on_ME_daily.CSV", header = F)
port_size[, date := as.Date(as.character(V1), format = "%Y%m%d")]
port_size[, year := year(date)]
port_size <- port_size[year > 1968]
port_size <- as.data.frame(port_size)
i <- 1
crsp$ret_port_size <- NA
for(i in 1:5)
{
  print(i)
  ind <- which(crsp$port_5ME == i)
  print(length(ind))
  match <- match(crsp$date[ind],port_size$date)
  print(length(match))
  crsp$ret_port_size[ind] <- port_size[match,i + 5]
}
crsp[, ret_size_num := as.numeric(as.character(ret_port_size))]
crsp[, ret_size_3year := prod(1+ret_size_num/100, na.rm = T) - 1, by = PERMNO]
crsp[, ret_size_BM_num := as.numeric(as.character(ret_port25))]
crsp[, ret_size_BM_3year := prod(1+ret_size_BM_num/100, na.rm = T) - 1, by = PERMNO]


match <- match(ipo$Permno, crsp$PERMNO)
ipo$cumret_size <- crsp$ret_size_3year[match]
ipo$cumret_BM <- crsp$ret_size_BM_3year[match]

df <- ipo[Year %in% 1973:2010, list(cohort = "1973-2012",n = length(Deal_number), ret3 = mean(cumret3, na.rm = T),
           retvw = mean(cumretvw3, na.rm = T),
           wrvw = mean((1+cumret3)/(1+cumretvw3), na.rm = T),
           retew = mean(cumretew3, na.rm = T),
           wrew = mean((1+cumret3)/(1+cumretew3), na.rm = T),
           retsize = mean(cumret_size, na.rm = T),
           vwsize = mean((1+cumret3)/(1+cumret_size), na.rm = T),
           retBM = mean(cumret_BM, na.rm = T),
           vwBM = mean((1+cumret3)/(1+cumret_BM), na.rm = T))]

df <- rbind(df, ipo[Year %in% 1973:2010, list(n = length(Deal_number), ret3 = mean(cumret3, na.rm = T),
                                    retvw = mean(cumretvw3, na.rm = T),
                                    wrvw = mean((1+cumret3)/(1+cumretvw3), na.rm = T),
                                    retew = mean(cumretew3, na.rm = T),
                                    wrew = mean((1+cumret3)/(1+cumretew3), na.rm = T),
                                    retsize = mean(cumret_size, na.rm = T),
                                    vwsize = mean((1+cumret3)/(1+cumret_size), na.rm = T),
                                    retBM = mean(cumret_BM, na.rm = T),
                                    vwBM = mean((1+cumret3)/(1+cumret_BM), na.rm = T)), by = cohort])

clip <- pipe("pbcopy", "w") 
write.table(df, file=clip, sep = "\t", row.names = F) 
close(clip)
