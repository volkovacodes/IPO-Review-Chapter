#############################################
###!!!!!!!!!!!! README START !!!!!!!!!!!!####
#############################################
# This code uses clean IPO datafile to contruct 
# table 10 Panel C and Figure 15
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
crsp.datafile <- "crsp.rds"
crsp.info <- "crsp_info.csv"
### Path to CRSP/Compustat merged file
comp.datafile <- "compustat.rds"

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
crsp[, `:=` (year = year(date), month = month(date), day = day(date))]
crsp[month == 12, day_til_yearend := 31 - day]
print(Sys.time())

crsp_info <- read.csv(crsp.info)
m <- match(crsp$PERMNO, crsp_info$PERMNO)
crsp$EXCHCD <- crsp_info$HEXCD[m]
crsp$SHRCD <- crsp_info$HSHRCD[m]
### Loading Compustat data:
compustat <- readRDS(comp.datafile)
calculate_book_value <- function(comp) # calculating book value
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

### Finding PERMNO-Year pairs of recent IPOs
### These pairs would be excluded after
ipo[,`:=`(Py1 = paste(Permno, Year), Py2 = paste(Permno, Year - 1), Py3 = paste(Permno, Year -2),
          Py4 = paste(Permno, Year - 4), Py5 = paste(Permno, Year - 5))]
flags <- c(ipo$Py1, ipo$Py2, ipo$Py3, ipo$Py4, ipo$Py5)

match_BM <- function(ipo_id, crsp_december)
{
  n <- function(x) return(as.numeric(as.character(x)))
  ipopermno <- ipo$Permno[ipo_id]
  iposize <- n(ipo$size_yearend[ipo_id])
  ipobm <- n(ipo$BM[ipo_id])
  if(is.na(ipopermno) | is.na(iposize) | is.na(ipobm)) return(NA)
  crsp_foo <- crsp_december[size >= 0.7*iposize & size <= 1.3*iposize]
  if(sum(!is.na(crsp_foo$BM)) == 0) return(NA)
  i <- which.min(abs(crsp_foo$BM - ipobm))
  return(crsp_foo$PERMNO[i])
}

for(cyear in 1973:2013)
{
  print(cyear)
  ind <- which(ipo$Year == cyear)
  
  ### matching size in december
  crsp_december <- crsp[year == cyear & month == 12]
  crsp_december <- crsp_december[EXCHCD %in% 1:3 & SHRCD %in% 10:19]
  setkey(crsp_december, day_til_yearend)
  crsp_december <- crsp_december[!duplicated(PERMNO)]
  crsp_december[, permno_year := paste(PERMNO, year)]
  crsp_december <- crsp_december[!permno_year %in% flags]
  crsp_december <- crsp_december[abs(PRC) >= 5]
  
  crsp_december[, size := abs(PRC)*SHROUT]
  m <- match(crsp_december$permno_year, paste(compustat$LPERMNO, compustat$year))
  crsp_december$BE <- compustat$be[m]
  crsp_december$BM <- 10^3*crsp_december$BE/crsp_december$size
 
  for(i in ind)
  {
    permno <- match_BM(i, crsp_december)
    crsp_december <- crsp_december[!PERMNO %in% permno]
    ipo$match_size_BM_permno1[i] <- permno
  }
  
  for(i in ind)
  {
    permno <- match_BM(i, crsp_december)
    crsp_december <- crsp_december[!PERMNO %in% permno]
    ipo$match_size_BM_permno2[i] <- permno
  }
  
  for(i in ind)
  {
    permno <- match_BM(i, crsp_december)
    crsp_december <- crsp_december[!PERMNO %in% permno]
    ipo$match_size_BM_permno3[i] <- permno
  }
  rm(crsp_december)
}


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
ipo[, `:=` (match1_BH3 = NA, match2_BH3 = NA, match3_BH3 = NA)]

for(cyear in 1973:2013)
{
  print(cyear)
  print(Sys.time())
  ind <- which(ipo$Year == cyear)

  crsp_year <- crsp[PERMNO %in% ipo$Permno[ind]]
  setkey(crsp_year, PERMNO, date)
  crsp_year[, dif := 1:.N, by = PERMNO]
  crsp_year <- crsp_year[dif <= 252*3]
  crsp_year[, `:=`(end3 = as.character(max(date, na.rm = T))), by = PERMNO]
  
  m <- match(ipo$Permno[ind], crsp_year$PERMNO)
  ipo$end3[ind] <- crsp_year$end3[m]
  
  ipo$match1_BH3[ind] <- get_BH(ipo$match_size_BM_permno1[ind], ipo$Issue_date[ind], ipo$end3[ind], crsp)
  ipo$match2_BH3[ind] <- get_BH(ipo$match_size_BM_permno2[ind], ipo$Issue_date[ind], ipo$end3[ind], crsp)
  ipo$match3_BH3[ind] <- get_BH(ipo$match_size_BM_permno3[ind], ipo$Issue_date[ind], ipo$end3[ind], crsp)
}

###########################################
###!!!!!!! Table 10, Panel C !!!!!!!#######
###########################################
years3 <- NULL
years3[[1]] <- 1973:2013
years3[[2]] <- 1973:1979
years3[[3]] <- 1980:1989
years3[[4]] <- 1990:1999
years3[[5]] <- 2000:2013

table10c <- NULL
for(i in 1:5)
{
  tmp <- ipo[Year %in% years3[[i]], list(n = length(Issuer), BH3 = mean(BH3, na.rm = T), match1_BH3 = mean(match1_BH3, na.rm = T),
                                         match2_BH3 = mean(match2_BH3, na.rm = T), match3_BH3 = mean(match3_BH3, na.rm = T))]
  table10c <- rbind(table10c, tmp)
}

###########################################
###!!!!!!! Figure 15, Panel A,B !!!!!!!####
###########################################
figure15a <- ipo[Year %in% 1973:2013, list(mean_ret3 = mean(BH3, na.rm = T), half_df = 0.5*sd(BH3, na.rm = T)), by = Year]
figure15b <- ipo[Year %in% 1973:2013, list(mean_ret3 = mean(match1_BH3, na.rm = T), half_df = 0.5*sd(match1_BH3, na.rm = T)), by = Year]

###########################################
###!!!!!!! Figure 15, Panel C !!!!!!!######
###########################################

for(cyear in 1973:2013)
{
  print(cyear)
  crsp_year <- crsp[year == cyear & month == 12]
  setkey(crsp_year, PERMNO, day_til_yearend)
  crsp_year <- crsp_year[!duplicated(PERMNO)]
  crsp_year <- crsp_year[!paste(PERMNO, year) %in% flags]
  crsp_year[,size := abs(PRC)*SHROUT]
  crsp_year <- crsp_year[order( - size)]
  large_permno <- crsp_year$PERMNO[1:min(1000, length(crsp_year$PERMNO))]
  
  ind <- which(ipo$Year == cyear)
  ipo$match_snp1500[ind] <- sample(large_permno, length(ind))
  ipo$snp1500_BH3[ind] <- get_BH(ipo$match_snp1500[ind], ipo$Issue_date[ind], ipo$end3[ind], crsp)
}

figure15c <- ipo[Year %in% 1973:2013, list(mean_ret3 = mean(snp1500_BH3, na.rm = T), half_df = 0.5*sd(snp1500_BH3, na.rm = T)), by = Year]

write.csv(ipo, ipo.datafile)
export <- function(x)
{
  clip <- pipe("pbcopy", "w") 
  write.table(x, file=clip, sep = "\t", row.names = F) 
  close(clip)
}
export(table10c)
export(figure15a)
export(figure15b)
export(figure15c)

