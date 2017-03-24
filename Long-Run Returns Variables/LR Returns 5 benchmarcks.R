require(data.table)
require(bit64)

### loading crsp tape recods
load("./CRSP_COMP/CRSP_daily_1926_1989.rda")
crsp_daily_1989[, year := year(date)]
crsp_daily_1989 <- crsp_daily_1989[year >= 1970]
load("./CRSP_COMP/CRSP_daily_1990_2015.rda")
crsp_daily[, year := year(date)]
crsp <- rbind(crsp_daily_1989, crsp_daily)
rm(crsp_daily, crsp_daily_1989)
crsp[, RET := as.numeric(as.character(RET))]
crsp <- crsp[!is.na(RET)]
setkey(crsp, PERMNO, date)

### loadings IPOs
ipo <- fread("./IPO review chapter/Chapter write up/Data 20170315/ipo.csv")
ipo[, Issue_date := as.Date(Issue_date)]

### function to estimate buy-and-hold returns for given company dates
BH_ret <- function(permnos, dates, crsp)
{
  crsp_match <- crsp[PERMNO %in% permnos]
  match <- match(crsp_match$PERMNO,permnos)
  crsp_match$ipo_date <- dates[match]
  crsp_match <- crsp_match[date >= ipo_date]
  crsp_match[, dif := 1:.N, by = PERMNO]
  crsp_match <- crsp_match[dif <= 251*5 & dif > 0]
  crsp_match[, BH3 := prod(1+RET[dif <= 251*3 & dif > 0], na.rm = T) - 1, by = PERMNO]
  crsp_match[, BH5 := prod(1+RET, na.rm = T) - 1, by = PERMNO]
  match <- match(permnos, crsp_match$PERMNO)
  return(crsp_match[match])
}

crsp_match <- BH_ret(ipo$Permno, ipo$Issue_date, crsp)
ipo[, `:=` (BH3 = crsp_match$BH3, BH5 = crsp_match$BH5)]

load("./CRSP_COMP/crsp_monthly_1970_2015.rda")
crsp_monthly[,`:=`( year = substr(date, 1, 4), month = substr(date,5,6), size = abs(PRC)*SHROUT)]

### here prepare december file for matching
crsp_monthly_dec <- crsp_monthly[!is.na(RET) & month == 12 & !is.na(size)]
setkey(crsp_monthly_dec, year, size)

### excluding IPOs and SE0s
iposeo <- fread("./IPO review chapter/Chapter write up/SDC-PULL-IPO/ipo_seo_permno.csv")
iposeo <- iposeo[!is.na(Permno)]
iposeo[,`:=`(Py1 = paste(Permno, Year), Py2 = paste(Permno, Year - 1), Py3 = paste(Permno, Year -2),
             Py4 = paste(Permno, Year - 4), Py5 = paste(Permno, Year - 5))]
flags <- c(iposeo$Py1, iposeo$Py2, iposeo$Py3, iposeo$Py4, iposeo$Py5)

crsp_monthly_dec[, iposeoflag := 0]
crsp_monthly_dec[, Permno_year := paste(PERMNO, year)]
crsp_monthly_dec[Permno_year %in% flags, iposeoflag := 1]
crsp_monthly_dec[, Permno1 :=shift(PERMNO[iposeoflag == 0], 1, type='lead'), by = year]
crsp_monthly_dec[, Permno2 :=shift(PERMNO[iposeoflag == 0], 2, type='lead'), by = year]
crsp_monthly_dec[, Permno3 :=shift(PERMNO[iposeoflag == 0], 3, type='lead'), by = year]

match <- match(ipo$Permno, crsp_monthly_dec$PERMNO)
match_size_permno1 <- crsp_monthly_dec$Permno1[match]
match_size_permno2 <- crsp_monthly_dec$Permno2[match]
match_size_permno3 <- crsp_monthly_dec$Permno3[match]

### calculate returns of first matched stock
crsp_match <- BH_ret(match_size_permno1, ipo$Issue_date, crsp)
ipo[, `:=`(MSIZERET3_1 = crsp_match$BH3, MSIZERET5_1 = crsp_match$BH5)]

### calculate returns of second matched stock
crsp_match <- BH_ret(match_size_permno2, ipo$Issue_date, crsp)
ipo[, `:=`(MSIZERET3_2 = crsp_match$BH3, MSIZERET5_2 = crsp_match$BH5)]

### calculate returns of third matched stock
crsp_match <- BH_ret(match_size_permno3, ipo$Issue_date, crsp)
ipo[, `:=`(MSIZERET3_3 = crsp_match$BH3, MSIZERET5_3 = crsp_match$BH5)]

#### calcualting BM for IPOs and matching
get_comp <- function(year)
{
  source("./R codes/wrds connect.R")
  sql_comp <- paste0( "select gvkey, fyear, fyr, pstkl, pstkrv, pstk, seq,ceq, at, lt, 
                      txditc, dcvt, consol, indfmt, datafmt, popsrc from COMPM.FUNDA 
                      where fyear between ", year - 1, " and ", year)
  res <- dbSendQuery(wrds, sql_comp)
  comp <- fetch(res, n = -1)
  comp <- as.data.table(comp)
  comp <- comp[ fyr > 0 & at > 0]
  comp[, year := fyear]
  comp[fyr %in% 1:5, year := fyear + 1]
  real_year <- year
  comp <- comp[year == real_year]
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
  return(comp)
}
get_links <- function()
{
  source("./R codes/wrds connect.R")
  sql <- "select gvkey, lpermno from CRSPA.CCMXPF_LINKTABLE"
  res <- dbSendQuery(wrds, sql)
  link <- fetch(res, n = -1)
  link <- as.data.table(link)
  link <- link[!is.na(link$lpermno)]
  return(link)
}
### calculating thresholds for portfolios:
get_port_rank <- function(crsp, year)
{
  #crsp_june <- crsp[month == "06"]
  crsp_june <- crsp
  crsp_june <- crsp_june[!is.na(size) & !is.na(BE) & (BE) > 0]
  crsp_june <- crsp_june[!duplicated(PERMNO)]
  crsp_june[, BM := BE/size]
  
  crsp_june <- setDT(crsp_june)[, size_port := cut(size, quantile(size, probs=0:5/5), 
                                                   include.lowest=TRUE, labels=FALSE)]
  crsp_june <- setDT(crsp_june)[, BM5 := cut(BM, quantile(BM, probs=0:5/5), 
                                             include.lowest=TRUE, labels=FALSE)]
  crsp_june[, size_BM_port := (size_port - 1)*5 + BM5]
  return(crsp_june)
}
links <- get_links()
ipo[, `:=`( size = NA, BE = NA, size_port = NA, size_BM_port  = NA, 
            match_size_BM_permno1 = NA,match_size_BM_permno2 = NA,
            match_size_BM_permno3 = NA)]

match_BM <- function(ipo_id, crsp_tmp = crsp_cut)
{
  ipopermno <- ipo$Permno[ipo_id]
  iposize <- ipo$size[ipo_id]
  ipobm <- ipo$BM[ipo_id]
  if(is.na(ipopermno) | is.na(iposize) | is.na(ipobm)) return(NA)
  crsp_foo <- crsp_tmp[size >= 0.7*iposize & size <= 1.3*iposize]
  i <- which.min(abs(crsp_foo$BM - ipobm))
  return(crsp_foo$PERMNO[i])
}
for(cyear in 1972:2015)
{
  print(cyear)
  ipo[, Year := year(Issue_date)]
  ind <- which(ipo$Year == cyear)
  comp <- get_comp(cyear)
  crsp_cut <- crsp[year == cyear]
  crsp_cut$gvkey <- links$gvkey[match(crsp_cut$PERMNO, links$lpermno)]
  crsp_cut$BE <- comp$be[match(crsp_cut$gvkey, comp$gvkey)]
  crsp_cut[, size := abs(PRC)*SHROUT]
  crsp_cut <- get_port_rank(crsp_cut)
  ipo$size[ind] <- crsp_cut$size[match(ipo$Permno[ind], crsp_cut$PERMNO)] 
  ipo$BM[ind] <- crsp_cut$BM[match(ipo$Permno[ind], crsp_cut$PERMNO)] 
  ipo$size_port[ind] <- crsp_cut$size_port[match(ipo$Permno[ind], crsp_cut$PERMNO)] 
  ipo$size_BM_port[ind] <- crsp_cut$size_BM_port[match(ipo$Permno[ind], crsp_cut$PERMNO)] 
  
  crsp_cut <- crsp_cut[!paste(PERMNO, year) %in% flags]
  ipo$match_size_BM_permno1[ind] <- sapply(ind, match_BM)
  
  crsp_cut <- crsp_cut[!PERMNO %in% ipo$match_size_BM_permno1[ind]]
  ipo$match_size_BM_permno2[ind] <- sapply(ind, match_BM)
  
  crsp_cut <- crsp_cut[!PERMNO %in% ipo$match_size_BM_permno2[ind]]
  ipo$match_size_BM_permno3[ind] <- sapply(ind, match_BM)
}


### calculate returns of size and BM matched stocks
crsp_match <- BH_ret(ipo$match_size_BM_permno1, ipo$Issue_date, crsp)
ipo[, `:=` (MSIZEBMRET3_1 = crsp_match$BH3, MSIZEBMRET5_1 = crsp_match$BH3)]
### calculate returns of size and BM matched stocks
crsp_match <- BH_ret(ipo$match_size_BM_permno2, ipo$Issue_date, crsp)
ipo[, `:=` (MSIZEBMRET3_2 = crsp_match$BH3, MSIZEBMRET5_2 = crsp_match$BH3)]
### calculate returns of size and BM matched stocks
crsp_match <- BH_ret(ipo$match_size_BM_permno3, ipo$Issue_date, crsp)
ipo[, `:=` (MSIZEBMRET3_3 = crsp_match$BH3, MSIZEBMRET5_3 = crsp_match$BH3)]


crsp_ipo <- crsp[PERMNO %in% ipo$Permno]
setkey(crsp_ipo, PERMNO, date)
crsp_ipo[, dif := 1:.N, by = PERMNO]
crsp_ipo <- crsp_ipo[dif <= 252*5]

crsp_ipo$port <- ipo$size_port[match(crsp_ipo$PERMNO, ipo$Permno)]
crsp_ipo[, port_date := paste(port, date)]

name <- "Size_Ret_EX_IPO_SEO.csv"
size_ret <- fread(paste0("./IPO review chapter/Chapter write up/SDC-PULL-IPO/Long-Run Returns Variables/", name))
colnames(size_ret) <- c("port_date", "ret")
match <- match(crsp_ipo$port_date, size_ret$port_date)
crsp_ipo$port_ret <- size_ret$ret[match]
crsp_ipo[, portret3 := prod(1+port_ret[dif <= 251*3 & dif > 0], na.rm = T) - 1, by = PERMNO]
crsp_ipo[, portret5 := prod(1+port_ret, na.rm = T) - 1, by = PERMNO]

match <- match(ipo$Permno, crsp_ipo$PERMNO)
ipo[, `:=` (port_SIZERET3 = crsp_ipo$portret3[match],port_SIZERET5 = crsp_ipo$portret5[match])]

df <- ipo[, list(BH = mean(BH3, na.rm = T), ret3_size1 = mean(BH3 - MSIZERET3_1,na.rm = T),
           ret3_size2 = mean(BH3 - MSIZERET3_2,na.rm = T),
           ret3_size3 = mean(BH3 - MSIZERET3_3,na.rm = T),
           ret3_bm1 = mean(BH3 - MSIZEBMRET3_1,na.rm = T),
           ret3_bm2 = mean(BH3 - MSIZEBMRET3_2,na.rm = T),
           ret3_bm3 = mean(BH3 - MSIZEBMRET3_3,na.rm = T)), by = Year]
ipo[Year >= 1980 & Year <= 1989, cohortR := "1980-1989"]
ipo[Year >= 1990 & Year <= 1994, cohortR := "1990-1994"]
ipo[Year >= 1995 & Year <= 1998, cohortR := "1995-1998"]
ipo[Year >= 1999 & Year <= 2000, cohortR := "1999-2000"]
ipo[Year >= 2001 & Year <= 2014, cohortR := "2001-2014"]


ipo[, list(dup1 = mean(duplicated(match_size_BM_permno1)),
                       dup2 = mean(duplicated(match_size_BM_permno2))), by = Year]
View(df)
