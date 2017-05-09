require(data.table)
require(bit64)
require(lubridate)
setwd("/Users/orhahog/Dropbox/")
path_crsp <- "/Volumes/SD_card/Yandex.Disk.localized/CRSP/DSF/CRSP_DSF_"
information_crsp <- fread( "/Volumes/SD_card/Yandex.Disk.localized/CRSP/DSF/info.csv", select = c("year", "n"))
### loadings IPOs
ipo <- fread("./Projects/IPO review chapter/Chapter write up/Data 20170315/ipo.csv")
ipo[, Issue_date := as.Date(Issue_date)]
ipo[, Year := year(Issue_date)]
### Loading compustat
comp <- fread("/Volumes/SD_card/Yandex.Disk.localized/Compustat/crsp_compustat_merger_annual.csv", 
              select = c("LPERMNO", "fyear", "fyr", "pstkl", "pstkrv", "pstk", "seq",
                         "ceq", "at", "lt", "txditc", "dcvt", "consol", "indfmt", "datafmt", "popsrc"))

### Loading CRSP monthly
crsp_month <- fread("/Volumes/SD_card/Yandex.Disk.localized/CRSP/MSF/CRSP_MSF.csv",
              select = c("PERMNO", "date","SHRCD", "EXCHCD","PRC", "RET", "CUSIP", "SHROUT"))
crsp_month[, date := ymd(date)]
crsp_month[, `:=` (month = month(date), year = year(date))]
#### calcualting BM for IPOs and matching
clean_comp <- function(comp, real_year)
{
  comp <- comp[!is.na(LPERMNO)]
  comp <- comp[ fyr > 0 & at > 0]
  comp[, year := fyear]
  comp[fyr %in% 1:5, year := fyear + 1]
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
  comp <- comp[!is.na(be)]
  return(comp)
}

### exclude ipos 
ipo[,`:=`(Py1 = paste(Permno, Year), Py2 = paste(Permno, Year - 1), Py3 = paste(Permno, Year -2),
          Py4 = paste(Permno, Year - 4), Py5 = paste(Permno, Year - 5))]
flags <- c(ipo$Py1, ipo$Py2, ipo$Py3, ipo$Py4, ipo$Py5)

match_BM <- function(ipo_id, crsp_tmp = crsp_year)
{
  ipopermno <- ipo$Permno[ipo_id]
  iposize <- as.numeric(as.character(ipo$size[ipo_id]))
  ipobm <- as.numeric(as.character(ipo$BM[ipo_id]))
  if(is.na(ipopermno) | is.na(iposize) | is.na(ipobm)) return(NA)
  crsp_foo <- crsp_tmp[size >= 0.7*iposize & size <= 1.3*iposize]
  i <- which.min(abs(crsp_foo$BM - ipobm))
  return(crsp_foo$PERMNO[i])
}

for(cyear in 1973:2012)
{
  print(cyear)
  
  ind <- which(ipo$Year == cyear)
  
  ### matching book value
  ### current year
  comp_year <- clean_comp(comp, cyear)
  match <- match(ipo$Permno[ind], comp_year$LPERMNO)
  ipo$be[ind] <- comp_year$be[match]
  ### next year
  comp_year <- clean_comp(comp, cyear + 1)
  match <- match(ipo$Permno[ipo$Year == cyear & is.na(ipo$be)], comp_year$LPERMNO)
  ipo$be[ipo$Year == cyear & is.na(ipo$be)] <- comp_year$be[match]
  
  ### matching size in december
  crsp_year <- crsp_month[year == cyear]
  crsp_year <- crsp_year[month == 12]
  crsp_year <- crsp_year[!duplicated(PERMNO)]
  crsp_year[, size := abs(PRC)*SHROUT]
  match <- match(ipo$Permno[ind], crsp_year$PERMNO)
  ipo$size[ind] <- crsp_year$size[match]
  
  ### crsp should have all info
  match <- match(crsp_year$PERMNO, comp_year$LPERMNO)
  crsp_year$BE <- comp_year$be[match]
  crsp_year[, BM := 10^3*BE/size]
  
  ipo$BM[ind] <- 10^3*ipo$be[ind]/ipo$size[ind]
  crsp_year <- crsp_year[!paste(PERMNO, year) %in% flags & EXCHCD %in% 1:3 & SHRCD %in% 10:19]
  for(i in ind)
  {
    permno <- match_BM(i, crsp_year)
    crsp_year <- crsp_year[!PERMNO %in% permno]
    ipo$match_size_BM_permno1[i] <- permno
  }
  
  for(i in ind)
  {
    permno <- match_BM(i)
    crsp_year <- crsp_year[!PERMNO %in% permno]
    ipo$match_size_BM_permno2[i] <- permno
  }
  
  for(i in ind)
  {
    permno <- match_BM(i)
    crsp_year <- crsp_year[!PERMNO %in% permno]
    ipo$match_size_BM_permno3[i] <- permno
  }
  
}


permnos <- c(ipo$Permno, ipo$match_size_BM_permno1, ipo$match_size_BM_permno2, ipo$match_size_BM_permno3)

### obtain CRSP to calculate returns
crsp <- NULL
for(cyear in 1973:2015)
{
  print(cyear)
  n <- information_crsp$n[information_crsp$year == cyear]
  tmp <- fread(paste0(path_crsp, cyear, ".csv"), 
               select = c("PERMNO", "date", "RET", "vwretd", "ewretd"),
               nrows = n)
  tmp <- tmp[PERMNO %in% permnos]
  tmp[, date := ymd(date)]
  tmp[, year := year(date)]
  tmp[, RET := as.numeric(as.character(RET))] 
  tmp <- tmp[!is.na(RET)]
  
  crsp <- rbind(crsp, tmp)
}

setkey(crsp, PERMNO, date)

get_BH <- function(permnos, start_date, end_date, crsp)
{
  crsp <- crsp[PERMNO %in% permnos]
  match <- match(crsp$PERMNO, permnos)
  crsp$start_date <- start_date[match]
  crsp$end_date <- end_date[match]
  crsp <- crsp[date >= ymd(start_date) & date <= ymd(end_date)]
  setkey(crsp, PERMNO, date)
  crsp[, BH := prod(1 + RET, na.rm = T) - 1, by = PERMNO]
  match <- match(permnos, crsp$PERMNO)
  return(crsp$BH[match])
}
ipo[, `:=` (BH3 = NA, vwBH3 = NA, ewBH3 = NA, match1_BH3 = NA, match2_BH3 = NA, match3_BH3 = NA, end3 = NA,
            BH5 = NA, vwBH5 = NA, ewBH5 = NA, match1_BH5 = NA, match2_BH5 = NA, match3_BH5 = NA, end5 = NA)]

for(cyear in 1973:2012)
{
  print(cyear)
  ind <- which(ipo$Year == cyear)
  ### get ipo returns
  crsp_year <- crsp[PERMNO %in% ipo$Permno[ind]]
  setkey(crsp_year, PERMNO, date)
  crsp_year[, dif := 1:.N, by = PERMNO]
  ### five years
  crsp_year <- crsp_year[dif <= 252*5]
  crsp_year[, `:=`(BH5 = prod(1+RET, na.rm = T)- 1, vwBH5 = prod(1+vwretd, na.rm = T) - 1, 
              ewBH5 = prod(1+ewretd, na.rm = T) - 1, end5 = as.character(max(date, na.rm = T))), by = PERMNO]
  
  match <- match(ipo$Permno[ind], crsp_year$PERMNO)
  ipo$BH5[ind] <- crsp_year$BH5[match]
  ipo$vwBH5[ind] <- crsp_year$vwBH5[match]
  ipo$ewBH5[ind] <- crsp_year$ewBH5[match]
  ipo$end5[ind] <- crsp_year$end5[match]
  
  ### three years
  crsp_year <- crsp_year[dif <= 252*3]
  crsp_year[, `:=`(BH3 = prod(1+RET, na.rm = T)- 1, vwBH3 = prod(1+vwretd, na.rm = T) - 1, 
                   ewBH3 = prod(1+ewretd, na.rm = T) - 1, end3 = as.character(max(date, na.rm = T))), by = PERMNO]
  
  match <- match(ipo$Permno[ind], crsp_year$PERMNO)
  ipo$BH3[ind] <- crsp_year$BH3[match]
  ipo$vwBH3[ind] <- crsp_year$vwBH3[match]
  ipo$ewBH3[ind] <- crsp_year$ewBH3[match]
  ipo$end3[ind] <- crsp_year$end3[match]
  
  ipo$match1_BH3[ind] <- get_BH(ipo$match_size_BM_permno1[ind], ipo$Issue_date[ind], ipo$end3[ind], crsp)
  ipo$match2_BH3[ind] <- get_BH(ipo$match_size_BM_permno2[ind], ipo$Issue_date[ind], ipo$end3[ind], crsp)
  ipo$match3_BH3[ind] <- get_BH(ipo$match_size_BM_permno3[ind], ipo$Issue_date[ind], ipo$end3[ind], crsp)

  ipo$match1_BH5[ind] <- get_BH(ipo$match_size_BM_permno1[ind], ipo$Issue_date[ind], ipo$end5[ind], crsp)
  ipo$match2_BH5[ind] <- get_BH(ipo$match_size_BM_permno2[ind], ipo$Issue_date[ind], ipo$end5[ind], crsp)
  ipo$match3_BH5[ind] <- get_BH(ipo$match_size_BM_permno3[ind], ipo$Issue_date[ind], ipo$end5[ind], crsp)
}

### table 10 panel A
ipo[, cohort := NA]
ipo[Year < 1980, cohort := "1973-1979"]
ipo[Year >= 1980 & Year < 1990, cohort := "1980-1989"]
ipo[Year >= 1990 & Year < 2000, cohort := "1990-1999"]
ipo[Year >= 2000 & Year <= 2012, cohort := "2000-2012"]

df <- ipo[Year %in% 1973:2012, list(cohort = "1973-2012",n = length(Issuer), IPO_return = mean(BH3, na.rm = T),
                 VW_index = mean(vwBH3, na.rm = T), 
                 VW_wealth_relative = mean(1 + BH3, na.rm =T)/mean(1+vwBH3, na.rm = T),
                 EW_index = mean(ewBH3, na.rm = T), 
                 EW_wealth_relative = mean(1+BH3, na.rm = T)/mean(1+ewBH3, na.rm = T))]

table10a <- rbind(df, ipo[Year %in% 1973:2012, list(n = length(Issuer), IPO_return = mean(BH3, na.rm = T),
                                                    VW_index = mean(vwBH3, na.rm = T), 
                                                    VW_wealth_relative = mean(1 + BH3, na.rm =T)/mean(1+vwBH3, na.rm = T),
                                                    EW_index = mean(ewBH3, na.rm = T), 
                                                    EW_wealth_relative = mean(1+BH3, na.rm = T)/mean(1+ewBH3, na.rm = T)), by = cohort])

### table 10 panel B
ipo[, cohort := NA]
ipo[Year < 1980, cohort := "1973-1979"]
ipo[Year >= 1980 & Year < 1990, cohort := "1980-1989"]
ipo[Year >= 1990 & Year < 2000, cohort := "1990-1999"]
ipo[Year >= 2000 & Year <= 2010, cohort := "2000-2010"]

df <- ipo[Year %in% 1973:2010, list(cohort = "1973-2010",n = length(Issuer), IPO_return = mean(BH5, na.rm = T),
                 VW_index = mean(vwBH5, na.rm = T), 
                 VW_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+vwBH5, na.rm = T),
                 EW_index = mean(ewBH5, na.rm = T), 
                 EW_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+ewBH5, na.rm = T))]

table10b <- rbind(df, ipo[Year %in% 1973:2010, list(n = length(Issuer), IPO_return = mean(BH5, na.rm = T),
                                 VW_index = mean(vwBH5, na.rm = T), 
                                 VW_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+vwBH5, na.rm = T),
                                 EW_index = mean(ewBH5, na.rm = T), 
                                 EW_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+ewBH5, na.rm = T)), by = cohort])

### table 10 panel C
ipo[, cohort := NA]
ipo[Year < 1980, cohort := "1973-1979"]
ipo[Year >= 1980 & Year < 1990, cohort := "1980-1989"]
ipo[Year >= 1990 & Year < 2000, cohort := "1990-1999"]
ipo[Year >= 2000 & Year <= 2012, cohort := "2000-2012"]


df <- ipo[Year %in% 1973:2012, 
          list(cohort = "1973-2012",n = length(Issuer), IPO_return = mean(BH3, na.rm = T),
              BM_match1 = mean(match1_BH3, na.rm = T), 
              BM_match1_wealth_relative = mean(1+BH3, na.rm = T)/mean(1+match1_BH3, na.rm = T),
              BM_match2 = mean(match2_BH3, na.rm = T), 
              BM_match2_wealth_relative = mean(1+BH3, na.rm = T)/mean(1+match2_BH3, na.rm = T),
              BM_match3 = mean(match3_BH3, na.rm = T), 
              BM_match3_wealth_relative = mean(1+BH3, na.rm = T)/mean(1+match3_BH3, na.rm = T))]

table10c <- rbind(df, ipo[Year %in% 1973:2012, 
                          list(n = length(Issuer), IPO_return = mean(BH3, na.rm = T),
                               BM_match1 = mean(match1_BH3, na.rm = T), 
                               BM_match1_wealth_relative = mean(1+BH3, na.rm = T)/mean(1+match1_BH3, na.rm = T),
                               BM_match2 = mean(match2_BH3, na.rm = T), 
                               BM_match2_wealth_relative = mean(1+BH3, na.rm = T)/mean(1+match2_BH3, na.rm = T),
                               BM_match3 = mean(match3_BH3, na.rm = T), 
                               BM_match3_wealth_relative = mean(1+BH3, na.rm = T)/mean(1+match3_BH3, na.rm = T)), by = cohort])

### table 10 panel d
ipo[, cohort := NULL]
ipo[Year < 1980, cohort := "1973-1979"]
ipo[Year >= 1980 & Year < 1990, cohort := "1980-1989"]
ipo[Year >= 1990 & Year < 2000, cohort := "1990-1999"]
ipo[Year >= 2000 & Year <= 2010, cohort := "2000-2010"]

df <- ipo[Year %in% 1973:2010, 
          list(cohort = "1973-2010",n = length(Issuer), IPO_return = mean(BH5, na.rm = T),
               BM_match1 = mean(match1_BH5, na.rm = T), 
               BM_match1_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+match1_BH5, na.rm = T),
               BM_match2 = mean(match2_BH5, na.rm = T), 
               BM_match2_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+match2_BH5, na.rm = T),
               BM_match3 = mean(match3_BH5, na.rm = T), 
               BM_match3_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+match3_BH5, na.rm = T))]

table10d <- rbind(df, ipo[Year %in% 1973:2010, 
                          list(n = length(Issuer), IPO_return = mean(BH5, na.rm = T),
                               BM_match1 = mean(match1_BH5, na.rm = T), 
                               BM_match1_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+match1_BH5, na.rm = T),
                               BM_match2 = mean(match2_BH5, na.rm = T), 
                               BM_match2_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+match2_BH5, na.rm = T),
                               BM_match3 = mean(match3_BH5, na.rm = T), 
                               BM_match3_wealth_relative = mean(1+BH5, na.rm = T)/mean(1+match3_BH5, na.rm = T)), 
                          by = cohort])

export <- function(x)
{
  clip <- pipe("pbcopy", "w") 
  write.table(x, file=clip, sep = "\t", row.names = F) 
  close(clip)
}

write.csv(ipo, "./Projects/IPO review chapter/Chapter write up/Data 20170315/ipo_with_table10.csv", row.names = F)
export(table10a)
export(table10b)
export(table10c)
export(table10d)
