require(data.table)
require(bit64)
require(stringr)
require(lubridate)
### loadings IPOs
ipo <- fread("/Users/orhahog/Dropbox/Projects/IPO review chapter/Chapter write up/Data 20170315/ipo_with_table10.csv")
ipo[, Issue_date := as.Date(Issue_date)]
ipo[, Year := year(Issue_date)]
setkey(ipo, Issue_date)

n <- function(x) return(as.numeric(as.character(x)))
tmp <- fread("./CRSP_COMP/ME_Breakpoints.CSV")
quant <- data.table(date = n(tmp$V1), sizebp1 = n(tmp$V6), sizebp2 = n(tmp$V10), sizebp3 = n(tmp$V14), sizebp4 = n(tmp$V18))
quant[, `:=` (pyear = substr(date, 1, 4), month = as.numeric(as.character(substr(date,5, 6))))]
quant <- quant[pyear >= 1970 & month == 12]

tmp <- fread("./CRSP_COMP/BE-ME_Breakpoints.CSV")
match <- match(quant$pyear, tmp$V1)
quant[, `:=` (bmbp1 = n(tmp$V7[match]), bmbp2 = n(tmp$V11[match]), bmbp3 = n(tmp$V15[match]), bmbp4 = n(tmp$V19[match]))]

for(cyear in 1973:2012)
{
  print(cyear)
  ind <- which(ipo$Year == cyear)
  line <- quant[pyear == cyear]
  line[, `:=` (sizebp1 = 10^3*sizebp1, sizebp2 = 10^3*sizebp2, sizebp3 = 10^3*sizebp3, sizebp4 = 10^3*sizebp4)]
  
  size1 <- which(ipo$Year == cyear & ipo$size <= line$sizebp1)
  size2 <- which(ipo$Year == cyear & ipo$size > line$sizebp1 & ipo$size <= line$sizebp2)
  size3 <- which(ipo$Year == cyear & ipo$size > line$sizebp2 & ipo$size <= line$sizebp3)
  size4 <- which(ipo$Year == cyear & ipo$size > line$sizebp3 & ipo$size <= line$sizebp4)
  size5 <- which(ipo$Year == cyear & ipo$size > line$sizebp4)
  
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
quant_noipo <- fread("./FF 25 portfolio breakpoints without IPO firms 20170504.csv")
for(cyear in 1973:2012)
{
  print(cyear)
  ind <- which(ipo$Year == cyear)
  line <- quant_noipo[pyear == cyear]
  line[, `:=` (sizebp1 = 10^3*sizebp1, sizebp2 = 10^3*sizebp2, sizebp3 = 10^3*sizebp3, sizebp4 = 10^3*sizebp4)]
  
  size1 <- which(ipo$Year == cyear & ipo$size <= line$sizebp1)
  size2 <- which(ipo$Year == cyear & ipo$size > line$sizebp1 & ipo$size <= line$sizebp2)
  size3 <- which(ipo$Year == cyear & ipo$size > line$sizebp2 & ipo$size <= line$sizebp3)
  size4 <- which(ipo$Year == cyear & ipo$size > line$sizebp3 & ipo$size <= line$sizebp4)
  size5 <- which(ipo$Year == cyear & ipo$size > line$sizebp4)
  
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

### loading daily CRSP 
load("./CRSP_COMP/CRSP_daily_1926_1989.rda")
load("./CRSP_COMP/CRSP_daily_1990_2016.rda")

### long-run returns
crsp_daily_1989 <- crsp_daily_1989[PERMNO %in% ipo$Permno,]
crsp_daily <- crsp_daily[PERMNO %in% ipo$Permno,]
crsp <- rbind(crsp_daily_1989, crsp_daily)
rm(crsp_daily, crsp_daily_1989)

crsp[, RET := as.numeric(as.character(RET))]
crsp <- crsp[!is.na(RET)]
setkey(crsp, PERMNO, date)
crsp[, dif := 1:.N, by = PERMNO]
crsp <- crsp[dif < 5*252]
crsp[, date := ymd(date)]
crsp[, year := year(date)]
crsp[, id := paste(PERMNO, year)]

match <- match(crsp$PERMNO, ipo$Permno)
crsp$port_25 <- ipo$port_25[match]
crsp$port_25_noipo <- ipo$port_25_noipo[match]

port25 <- fread("./CRSP_COMP/25_Portfolios_5x5_Daily.CSV", header = F)
port25[, date := ymd(V1)]
port25[, year := year(date)]
port25 <- port25[ year > 1968]
port25 <- as.data.frame(port25)
for(i in 2:26) port25[,i] = n(port25[,i])/100

port25_noipo <- fread("./CRSP_COMP/FF 25 portfolio without IPO firms 20170504.csv")
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


#crsp[, `:=` (ret_port25_noipo = as.numeric(as.character(ret_port25_noipo)))]
crsp[, `:=` (BH3_port = prod(1 + ret_port25[dif <= 252*3], na.rm = T)- 1, 
             BH5_port = prod(1 + ret_port25, na.rm = T)- 1,
             BH3_port_noipo = prod(1 + ret_port25_noipo[dif <= 252*3], na.rm = T)- 1, 
             BH5_port_noipo = prod(1 + ret_port25_noipo, na.rm = T)- 1), by = PERMNO]

match <- match(ipo$Permno, crsp$PERMNO)
ipo[, `:=` (BH3_port = crsp$BH3_port[match], BH5_port = crsp$BH5_port[match],
            BH3_port_noipo = crsp$BH3_port_noipo[match], BH5_port_noipo = crsp$BH5_port_noipo[match])]

ipo[, cohort := NA]
ipo[Year < 1980, cohort := "1973-1979"]
ipo[Year >= 1980 & Year < 1990, cohort := "1980-1989"]
ipo[Year >= 1990 & Year < 2000, cohort := "1990-1999"]
ipo[Year >= 2000 & Year <= 2012, cohort := "2000-2012"]

df <- ipo[Year %in% 1973:2012 & !is.na(BH3_port)  & !is.na(ipo$be)& !is.na(BH3_port_noipo), 
          list(cohort = "1973-2012", n = length(Deal_number), 
           BH3 = mean(BH3, na.rm = T),
           BH3_port = mean(BH3_port, na.rm = T),
           wr_port = mean(1 + BH3, na.rm = T)/mean(1 + BH3_port, na.rm = T),
           BH3_port_noipo = mean(BH3_port_noipo, na.rm = T),
           wr_port_noipo = mean(1 + BH3, na.rm = T)/mean(1 + BH3_port_noipo, na.rm = T))]

df <- rbind(df, ipo[Year %in% 1973:2012 & !is.na(BH3_port) & !is.na(ipo$be) & !is.na(BH3_port_noipo), 
                    list(n = length(Deal_number), 
                         BH3 = mean(BH3, na.rm = T),
                         BH3_port = mean(BH3_port, na.rm = T),
                         wr_port = mean(1 + BH3, na.rm = T)/mean(1 + BH3_port, na.rm = T),
                         BH3_port_noipo = mean(BH3_port_noipo, na.rm = T),
                         wr_port_noipo = mean(1 + BH3, na.rm = T)/mean(1 + BH3_port_noipo, na.rm = T)), by = cohort])

### table 10 panel d
ipo[, cohort := NULL]
ipo[Year < 1980, cohort := "1973-1979"]
ipo[Year >= 1980 & Year < 1990, cohort := "1980-1989"]
ipo[Year >= 1990 & Year < 2000, cohort := "1990-1999"]
ipo[Year >= 2000 & Year <= 2010, cohort := "2000-2010"]
df <- ipo[Year %in% 1973:2010 & !is.na(BH5_port)  & !is.na(ipo$be)& !is.na(BH5_port_noipo), 
          list(cohort = "1973-2010", n = length(Deal_number), 
               BH5 = mean(BH5, na.rm = T),
               BH5_port = mean(BH5_port, na.rm = T),
               wr_port = mean(1 + BH5, na.rm = T)/mean(1 + BH5_port, na.rm = T),
               BH5_port_noipo = mean(BH5_port_noipo, na.rm = T),
               wr_port_noipo = mean(1 + BH5, na.rm = T)/mean(1 + BH5_port_noipo, na.rm = T))]

df <- rbind(df, ipo[Year %in% 1973:2010 & !is.na(BH5_port) & !is.na(ipo$be) & !is.na(BH5_port_noipo), 
                    list(n = length(Deal_number), 
                         BH5 = mean(BH5, na.rm = T),
                         BH5_port = mean(BH5_port, na.rm = T),
                         wr_port = mean(1 + BH5, na.rm = T)/mean(1 + BH5_port, na.rm = T),
                         BH5_port_noipo = mean(BH5_port_noipo, na.rm = T),
                         wr_port_noipo = mean(1 + BH5, na.rm = T)/mean(1 + BH5_port_noipo, na.rm = T)), by = cohort])
print(df)
clip <- pipe("pbcopy", "w") 
write.table(df, file=clip, sep = "\t", row.names = F) 
close(clip)
