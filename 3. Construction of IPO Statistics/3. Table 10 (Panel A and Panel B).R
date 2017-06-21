#############################################
###!!!!!!!!!!!! README START !!!!!!!!!!!!####
#############################################
# This code uses clean IPO datafile to contruct 
# table 10 Panel A and Panel B
# in Lowry, Michaely, Volkova, 
# "Initial Public Offerings: A synthesis of the literature and directions for future research",(2017)
# output tables are located in Tables.xlsx github file 
# output figures are located in Figures.xlsx github file
# for this file you need
# - ipo data file
# - CRSP daily file between 01-01-1973 and 12-31-2016 
#   with varibles: date, PERMNO, PRC, SHROUT, RET, wvret, evret, EXCHCD, SHRCD
# - COMPUSTAT file with variables:
#   LPERMNO, fyear, fyr, pstkl, pstkrv, pstk, seq,
#   ceq, at, lt, txditc, dcvt, consol, indfmt, datafmt, popsrc
#############################################
###!!!!!!!!!!!! README END !!!!!!!!!!!!######
#############################################

### Path to IPO datafile
ipo.datafile <- "ipo_all_variables.csv"
### Path to CRSP file 
crsp.datafile <- "crsp_info.rds"
### Path to CRSP/Compustat merged file
comp.datafile <- "compustat.rds"
### FF size and book to market portfolio breakpoint and portfolio returns (all companies)
size.breaks <- "ME_Breakpoints.CSV" # taken from Kenneth French website
bm.breaks <- "BE-ME_Breakpoints.CSV"  # taken from Kenneth French website
size_bm.returns <- "25_Portfolios_5x5_Daily.CSV" # taken from Kenneth French website

### FF size and book to market portfolio breakpoint and portfolio returns (EXCLUDING RECENT IPOs)
size_bm_noipo.breaks <- "FF 25 portfolio breakpoints without IPO firms 20170504.csv" # manually constructed
size_bm_noipo.returns <- "FF 25 portfolio without IPO firms 20170504.csv" # manually constructed

### loading packages:
require(data.table)
require(bit64)
require(stringr)
require(lubridate)

### loading IPO data:
ipo <-  as.data.table(read.csv(ipo.datafile))
setkey(ipo, Issue_date)

### Loading CRSP data:
print(Sys.time())
crsp <- readRDS(crsp.datafile)
print(Sys.time())

#############################################
###!!!!!!! Buy-and-Hold returns !!!!!!!######
###!!!!!!!! VW-market returns !!!!!!!!#######
###!!!!!!!! EW-market returns !!!!!!!!#######
#############################################
crsp <- crsp[PERMNO %in% ipo$Permno]
setkey(crsp, PERMNO, date)

crsp[, dif := 1:.N, by = PERMNO] # dif - number of trading days after IPO
crsp <- crsp[dif <= 5*252] #dropping observations from more than five years


n <- function(x) # function to clean and convert variables
{
  x <- as.numeric(as.character(x))
  x[is.infinite(x)] <- NA
  return(x)
}
crsp[, `:=` (vwretd = n(vwretd), ewretd = n(ewretd))]
crsp[, `:=` (BH3 = prod(1+RET[dif <= 3*252], na.rm = T)- 1, BH5 = prod(1+RET, na.rm = T)- 1), by = PERMNO] # 3 and 5 years buy-and-hold
crsp[, `:=` (VW3 = prod(1+vwretd[dif <= 3*252], na.rm = T) - 1, VW5 = prod(1+vwretd, na.rm = T) - 1), by = PERMNO] # 3 and 5 years value-weight market returns
crsp[, `:=` (EW3 = prod(1+ewretd[dif <= 3*252], na.rm = T) - 1, EW5 = prod(1+ewretd, na.rm = T) - 1), by = PERMNO] # 3 and 5 years equal-weight market returns

m <- match(ipo$Permno, crsp$PERMNO)
ipo[, `:=` (BH3 = crsp$BH3[m], BH5 = crsp$BH5[m], VW3 = crsp$VW3[m], VW5 = crsp$VW5[m], EV3 = crsp$EW3[m], EV5 = crsp$EW5[m])]

#############################################
###!!!!!!! Calculate Book Value !!!!!!!######
#############################################
# load compuatat
compustat <- readRDS(comp.datafile)
calculate_book_value <- function(comp)
{
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
  comp <- comp[!is.na(be)]
  comp[, year := year - 1]
  return(comp)
}
compustat <- calculate_book_value(compustat)
m <- match(paste(ipo$Permno, ipo$Year), paste(compustat$LPERMNO, compustat$year))
ipo$BE <- compustat$be[m]

#############################################
###!!!!!!!!! Calculate Size  !!!!!!!!!#######
#############################################
crsp[, `:=` (month = month(date), day = day(date))]
crsp_yearend <- crsp[month == 12]
m <- match(crsp_yearend$PERMNO, ipo$Permno)
crsp_yearend$ipo_year <- ipo$Year[m]
crsp_yearend <- crsp_yearend [year(date) == ipo_year]
crsp_yearend[, day_till_year_end := 31 - day]
setkey(crsp_yearend, PERMNO, day_till_year_end)
m <- match(ipo$Permno, crsp_yearend$PERMNO)
ipo$size_yearend <- crsp_yearend$SHROUT[m]*abs(crsp_yearend$PRC[m])
rm(crsp_yearend)
#############################################
###!!!!!!!!! Calculate BM !!!!!!!!!##########
#############################################
ipo$BM <- 10^3*ipo$BE/ipo$size_yearend
########################################################
###!!!!!!! Determine Size and BM Portfolio !!!!!!!######
###!!!!!!! Here ALL companies are use to  !!!!!!!######
###!!!!!!! construct portfolio (i.e. standard FF)  !!!!!!!######
########################################################
tmp <- fread(size.breaks)
quant <- data.table(date = n(tmp$V1), sizebp1 = n(tmp$V6), sizebp2 = n(tmp$V10), sizebp3 = n(tmp$V14), sizebp4 = n(tmp$V18))
quant[, `:=` (pyear = substr(date, 1, 4), month = n(substr(date,5, 6)))]
quant <- quant[pyear >= 1970 & month == 12]

tmp <- fread(bm.breaks)
match <- match(quant$pyear, tmp$V1)
quant[, `:=` (bmbp1 = n(tmp$V7[match]), bmbp2 = n(tmp$V11[match]), bmbp3 = n(tmp$V15[match]), bmbp4 = n(tmp$V19[match]))]

for(cyear in 1973:2013)
{
  print(cyear)
  ind <- which(ipo$Year == cyear)
  line <- quant[pyear == cyear]
  line[, `:=` (sizebp1 = 10^3*sizebp1, sizebp2 = 10^3*sizebp2, sizebp3 = 10^3*sizebp3, sizebp4 = 10^3*sizebp4)]
  
  size1 <- which(ipo$Year == cyear & ipo$size_yearend <= line$sizebp1)
  size2 <- which(ipo$Year == cyear & ipo$size_yearend > line$sizebp1 & ipo$size_yearend <= line$sizebp2)
  size3 <- which(ipo$Year == cyear & ipo$size_yearend > line$sizebp2 & ipo$size_yearend <= line$sizebp3)
  size4 <- which(ipo$Year == cyear & ipo$size_yearend > line$sizebp3 & ipo$size_yearend <= line$sizebp4)
  size5 <- which(ipo$Year == cyear & ipo$size_yearend > line$sizebp4)
  
  bm1 <- which(ipo$Year == cyear & ipo$BM <= line$bmbp1)
  bm2 <- which(ipo$Year == cyear & ipo$BM > line$bmbp1 & ipo$BM <= line$bmbp2)
  bm3 <- which(ipo$Year == cyear & ipo$BM > line$bmbp2 & ipo$BM <= line$bmbp3)
  bm4 <- which(ipo$Year == cyear & ipo$BM > line$bmbp3 & ipo$BM <= line$bmbp4)
  bm5 <- which(ipo$Year == cyear & ipo$BM > line$bmbp4)
  ipo$portfolio_size[size1] <- 1
  ipo$portfolio_size[size2] <- 2
  ipo$portfolio_size[size3] <- 3
  ipo$portfolio_size[size4] <- 4
  ipo$portfolio_size[size5] <- 5
  
  ipo$portfolio_bm[bm1] <- 1
  ipo$portfolio_bm[bm2] <- 2
  ipo$portfolio_bm[bm3] <- 3
  ipo$portfolio_bm[bm4] <- 4
  ipo$portfolio_bm[bm5] <- 5
}
ipo$port_25 <- (ipo$portfolio_size - 1)*5 + ipo$portfolio_bm

#########################################################
###!!!!!!! Determine Size and BM Portfolio !!!!!!!#######
###!!!!!!! Here we explude IPOs from factors !!!!!!!#####
#########################################################

quant_noipo <- fread(size_bm_noipo.breaks)
for(cyear in 1973:2013)
{
  print(cyear)
  ind <- which(ipo$Year == cyear)
  line <- quant_noipo[pyear == cyear]
  line[, `:=` (sizebp1 = 10^3*sizebp1, sizebp2 = 10^3*sizebp2, sizebp3 = 10^3*sizebp3, sizebp4 = 10^3*sizebp4)]
  
  size1 <- which(ipo$Year == cyear & ipo$size_yearend <= line$sizebp1)
  size2 <- which(ipo$Year == cyear & ipo$size_yearend > line$sizebp1 & ipo$size_yearend <= line$sizebp2)
  size3 <- which(ipo$Year == cyear & ipo$size_yearend > line$sizebp2 & ipo$size_yearend <= line$sizebp3)
  size4 <- which(ipo$Year == cyear & ipo$size_yearend > line$sizebp3 & ipo$size_yearend <= line$sizebp4)
  size5 <- which(ipo$Year == cyear & ipo$size_yearend > line$sizebp4)
  
  bm1 <- which(ipo$Year == cyear & ipo$BM <= line$bmbp1)
  bm2 <- which(ipo$Year == cyear & ipo$BM > line$bmbp1 & ipo$BM <= line$bmbp2)
  bm3 <- which(ipo$Year == cyear & ipo$BM > line$bmbp2 & ipo$BM <= line$bmbp3)
  bm4 <- which(ipo$Year == cyear & ipo$BM > line$bmbp3 & ipo$BM <= line$bmbp4)
  bm5 <- which(ipo$Year == cyear & ipo$BM > line$bmbp4)
  ipo$portfolio_size_noipo[size1] <- 1
  ipo$portfolio_size_noipo[size2] <- 2
  ipo$portfolio_size_noipo[size3] <- 3
  ipo$portfolio_size_noipo[size4] <- 4
  ipo$portfolio_size_noipo[size5] <- 5
  
  ipo$portfolio_bm_noipo[bm1] <- 1
  ipo$portfolio_bm_noipo[bm2] <- 2
  ipo$portfolio_bm_noipo[bm3] <- 3
  ipo$portfolio_bm_noipo[bm4] <- 4
  ipo$portfolio_bm_noipo[bm5] <- 5
}

ipo$port_25_noipo <- (ipo$portfolio_size_noipo - 1)*5 + ipo$portfolio_bm_noipo

#########################################################
###!!!!!!! Calculating matched portfolio returns !!!!!!!#######
#########################################################
m <- match(crsp$PERMNO, ipo$Permno)
crsp[, `:=`(port_25 = ipo$port_25[m], port_25_noipo = ipo$port_25_noipo[m])]

port25 <- fread(size_bm.returns, header = F)
port25[, date := ymd(V1)]
port25[, year := year(date)]
port25 <- port25[ year > 1968]
port25 <- as.data.frame(port25)
for(i in 2:26) port25[,i] = n(port25[,i])/100

port25_noipo <- fread(size_bm_noipo.returns)
port25_noipo[, date := mdy(date)]
port25_noipo <- as.data.frame(port25_noipo)

for(i in 1:25)
{
  print(i)
  print(Sys.time())
  ind <- which(crsp$port_25 == i)
  print(length(ind))
  match <- match(crsp$date[ind],port25$date)
  crsp$ret_port25[ind] <- port25[match,i + 1]
  
  ind <- which(crsp$port_25_noipo == i)
  print(length(ind))
  noipo <- port25_noipo[port25_noipo$portfolio == i,]
  match <- match(crsp$date[ind],noipo$date)
  crsp$ret_port25_noipo[ind] <- noipo$vw_ret_daily[match]
}

crsp[, `:=` (BH3_port = prod(1 + ret_port25[dif <= 252*3], na.rm = T)- 1, 
             BH5_port = prod(1 + ret_port25, na.rm = T)- 1,
             BH3_port_noipo = prod(1 + ret_port25_noipo[dif <= 252*3], na.rm = T)- 1, 
             BH5_port_noipo = prod(1 + ret_port25_noipo, na.rm = T)- 1), by = PERMNO]

m <- match(ipo$Permno, crsp$PERMNO)
ipo[, `:=` (BH3_port = crsp$BH3_port[m], BH5_port = crsp$BH5_port[m],
            BH3_port_noipo = crsp$BH3_port_noipo[m], BH5_port_noipo = crsp$BH5_port_noipo[m])]


###########################################
###!!!!!!! Table 10, Panel A !!!!!!!#######
###########################################
years3 <- NULL
years3[[1]] <- 1973:2013
years3[[2]] <- 1973:1979
years3[[3]] <- 1980:1989
years3[[4]] <- 1990:1999
years3[[5]] <- 2000:2013

table10a <- NULL
for(i in 1:5)
{
  tmp <- ipo[Year %in% years3[[i]], list(n = length(Issuer), BH3 = mean(BH3, na.rm = T), VW3 = mean(VW3, na.rm = T),
                                         EW3 = mean(EV3, na.rm = T), BH3_port = mean(BH3_port, na.rm = T), BH3_port_noipo = mean(BH3_port_noipo))]
  table10a <- rbind(table10a, tmp)
}

###########################################
###!!!!!!! Table 10, Panel B !!!!!!!#######
###########################################
years5 <- NULL
years5[[1]] <- 1973:2011
years5[[2]] <- 1973:1979
years5[[3]] <- 1980:1989
years5[[4]] <- 1990:1999
years5[[5]] <- 2000:2011

table10b <- NULL
for(i in 1:5)
{
  tmp <- ipo[Year %in% years5[[i]], list(n = length(Issuer), BH5 = mean(BH5, na.rm = T), VW5 = mean(VW5, na.rm = T),
                                         EW5 = mean(EV5, na.rm = T), BH5_port = mean(BH5_port, na.rm = T), BH5_port_noipo = mean(BH5_port_noipo))]
  table10b <- rbind(table10b, tmp)
}

write.csv(ipo, ipo.datafile, row.names = F)

export <- function(x)
{
  clip <- pipe("pbcopy", "w") 
  write.table(x, file=clip, sep = "\t", row.names = F) 
  close(clip)
}
export(table10a)
export(table10b)
