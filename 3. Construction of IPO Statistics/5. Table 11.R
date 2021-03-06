#############################################
###!!!!!!!!!!!! README START !!!!!!!!!!!!####
#############################################
# This code uses clean IPO datafile to contruct 
# table 11, Panel A and Panel B
# in Lowry, Michaely, Volkova, 
# "Initial Public Offerings: A synthesis of the literature and directions for future research",(2017)
# output tables are located in Tables.xlsx github file 
# output figures are located in Figures.xlsx github file
# for this file you need
# - ipo data file
# - CRSP monthly file between 01-01-1973 and 12-31-2016 
#   with varibles: date, PERMNO, PRC, SHROUT, RET
# - Fama-French file includes daily value of variables:
#   date, mktrf, smb, hml, umd, rf, year, dateff
#############################################
###!!!!!!!!!!!! README END !!!!!!!!!!!!######
#############################################
ipo.datafile <- "ipo_all_variables.csv"
ff.datafile <- "ffdata.rds"
crsp_month.datafile <- "crsp_monthly.rds"

### loading packages:
require(data.table)
require(bit64)
require(stringr)
require(lubridate)
require(lfe)
require(stargazer)

### loading IPO data:
ipo <-  as.data.table(read.csv(ipo.datafile))
setkey(ipo, Issue_date)

### Loading CRSP data:
crsp_month <- readRDS(crsp_month.datafile)
crsp_month <- crsp_month[PERMNO %in% ipo$Permno]

setkey(crsp_month, PERMNO, date)
crsp_month <- crsp_month[!duplicated(paste(crsp_month$PERMNO, crsp_month$date))]
crsp_month[, marcap := abs(PRC)*SHROUT]
crsp_month[, L.marcap := shift(marcap, 1), by = PERMNO]
crsp_month[, RET := 100*as.numeric(as.character(RET))]
crsp_month <- crsp_month[!is.na(RET) & !is.na(L.marcap)]
crsp_month[, dif := 1:.N, by = PERMNO]
crsp_month <- crsp_month[dif <= 5*12]

### Loading Fama-French four-factor data:
ffdata <- readRDS(ff.datafile)
ffdata[, dateff := ymd(dateff)]
ffdata[, `:=`(mktrf = 100*mktrf, smb = 100*smb, hml = 100*hml, umd = 100*umd, rf = 100*rf)]
years3 <- NULL
years3[[1]] <- 1973:2013
years3[[2]] <- 1973:1979
years3[[3]] <- 1980:1989
years3[[4]] <- 1990:1999
years3[[5]] <- 2000:2013

model_ew3 <- as.formula("I(ew_port3 - rf) ~ mktrf + smb + hml + umd|0|0")
model_vw3 <- as.formula("I(vw_port3 - rf) ~ mktrf + smb + hml + umd|0|0")

reg_ew3 <- NULL
reg_vw3 <- NULL
for(i in 1:5)
{
  years <- years3[[i]]
  permnos <- ipo[Year %in% years]$Permno
  crsp_tmp <- crsp_month[PERMNO %in% permnos]
  crsp_tmp <- crsp_tmp[dif <= 3*12]
  setkey(crsp_tmp, PERMNO, date)
  crsp_tmp[, `:=` (equal_ret3 = mean(RET, na.rm = T), value_ret3 = weighted.mean(RET, L.marcap, na.rm = T)), by = date]
  
  ff_reg <- ffdata
  m <- match(ff_reg$dateff, ymd(crsp_tmp$date))
  ff_reg[, `:=` (ew_port3 = crsp_tmp$equal_ret3[m], vw_port3 = crsp_tmp$value_ret3[m])]
  ff_reg <- ff_reg[!is.na(ew_port3)]
  
  reg_ew3[[i]] <- felm(model_ew3, ff_reg)
  reg_vw3[[i]] <- felm(model_vw3, ff_reg)
}

years5 <- years3
years5[[1]] <- 1973:2011
years3[[5]] <- 2000:2011

model_ew5 <- as.formula("I(ew_port5 - rf) ~ mktrf + smb + hml + umd|0|0")
model_vw5 <- as.formula("I(vw_port5 - rf) ~ mktrf + smb + hml + umd|0|0")

reg_ew5 <- NULL
reg_vw5 <- NULL
for(i in 1:5)
{
  years <- years5[[i]]
  permnos <- ipo[Year %in% years]$Permno
  crsp_tmp <- crsp_month[PERMNO %in% permnos]
  setkey(crsp_tmp, PERMNO, date)
  crsp_tmp[, `:=` (equal_ret5 = mean(RET, na.rm = T), value_ret5 = weighted.mean(RET, L.marcap, na.rm = T)), by = date]
  
  ff_reg <- ffdata
  m <- match(ff_reg$dateff, ymd(crsp_tmp$date))
  ff_reg[, `:=` (ew_port5 = crsp_tmp$equal_ret5[m], vw_port5 = crsp_tmp$value_ret5[m])]
  ff_reg <- ff_reg[!is.na(ew_port5)]
  
  reg_ew5[[i]] <- felm(model_ew5, ff_reg)
  reg_vw5[[i]] <- felm(model_vw5, ff_reg)
}

stargazer(reg_ew3, type = "text")
stargazer(reg_vw3, type = "text")

stargazer(reg_ew5, type = "text")
stargazer(reg_vw5, type = "text")

