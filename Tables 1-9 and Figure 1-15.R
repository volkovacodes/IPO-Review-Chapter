require(data.table)
require(bit64)
require(stringr)
ipo <- read.csv("./Projects/IPO review chapter/Chapter write up/Data 20170315/ipo.csv")

ipo <- as.data.table(ipo)
setkey(ipo, Issue_date)
ipo[, Year := year(Issue_date)]
ipo[, IR := Close_price1/Offer_Price - 1]
ipo[is.na(IR), IR := Close_price2/Offer_Price -1]
ipo[, Proceeds := as.numeric(as.character(gsub(",","", Proceeds)))]

### get data from https://fred.stlouisfed.org/series/GDPDEF/downloaddata
gdp <- read.csv("./Projects/IPO review chapter/Chapter write up/Data 20170315/GDPDEF.csv")
gdp <- as.data.table(gdp)
gdp[, year := year(DATE)]
gdp <- gdp[!duplicated(year)]
gdp[, base := GDPDEF[year == 2015]]
gdp[, factor := base/GDPDEF]
gdp[, inf := GDPDEF/shift(GDPDEF, 1) - 1]
match <- match(ipo$Year, gdp$year)
ipo$factor_2015 <- gdp$factor[match]
ipo$Proceeds_2015 <- ipo$Proceeds*ipo$factor_2015

ipo$size <- "N"
ipo[Proceeds_2015 <= 30, size := "S"]
ipo[Proceeds_2015 > 30 & Proceeds_2015 <= 120, size := "M"]
ipo[Proceeds_2015 > 120, size := "L"]

export <- function(x)
{
  clip <- pipe("pbcopy", "w") 
  write.table(x, file=clip, sep = "\t", row.names = F) 
  close(clip)
}

### table 1 
table1 <- ipo[, list(n = length(Deal_number), IR = mean(IR),
                     ave_proceeds = mean(Proceeds_2015), 
                     agg_proceeds = sum(Proceeds_2015)), by = Year]

table1 <- rbind(table1, ipo[, list(n = length(Deal_number), IR = mean(IR),
                           ave_proceeds = mean(Proceeds_2015), 
                           agg_proceeds = sum(Proceeds_2015), Year = "Total")])
export(table1)
### table2
table2 <- ipo[, list(n = length(Deal_number),
                     n_s = length(Deal_number[size == "S"]),
                     n_m = length(Deal_number[size == "M"]),
                     n_l = length(Deal_number[size == "L"]),
                     IR_s = mean(IR[size == "S"], na.rm = T),
                     IR_m = mean(IR[size == "M"], na.rm = T),
                     IR_l = mean(IR[size == "L"], na.rm = T)), by = Year]

table2 <- rbind(table2, ipo[, list(n = length(Deal_number),
           n_s = length(Deal_number[size == "S"]),
           n_m = length(Deal_number[size == "M"]),
           n_l = length(Deal_number[size == "L"]),
           IR_s = mean(IR[size == "S"], na.rm = T),
           IR_m = mean(IR[size == "M"], na.rm = T),
           IR_l = mean(IR[size == "L"], na.rm = T), Year = "Total")])
export(table2)
### table 3
table3 <- ipo[grep("range",Issue_priced_range), list(n = length(Deal_number),
                     n_b = length(Deal_number[grep("Below", Issue_priced_range)]),
                     n_w = length(Deal_number[grep("Within", Issue_priced_range)]),
                     n_a = length(Deal_number[grep("Above", Issue_priced_range)]),
                     IR_b = mean(IR[grep("Below", Issue_priced_range)], na.rm = T),
                     IR_w = mean(IR[grep("Within", Issue_priced_range)], na.rm = T),
                     IR_a = mean(IR[grep("Above", Issue_priced_range)], na.rm = T)), by = Year]

table3 <- rbind(table3, ipo[grep("range",Issue_priced_range), list(n = length(Deal_number),
                                                                   n_b = length(Deal_number[grep("Below", Issue_priced_range)]),
                                                                   n_w = length(Deal_number[grep("Within", Issue_priced_range)]),
                                                                   n_a = length(Deal_number[grep("Above", Issue_priced_range)]),
                                                                   IR_b = mean(IR[grep("Below", Issue_priced_range)], na.rm = T),
                                                                   IR_w = mean(IR[grep("Within", Issue_priced_range)], na.rm = T),
                                                                   IR_a = mean(IR[grep("Above", Issue_priced_range)], na.rm = T), 
                                                                   Year = "Total")])

export(table3)
### table 4
require(xlsx)
### get data about IPO age from Jay Ritter website
age <- read.xlsx2("./Projects/IPO review chapter/Chapter write up/Data 20170315/age7515.xlsx",1)
ipo$found_year <- age$FOUNDYEAR[match(ipo$Permno, age$Permno)]
ipo$age <- ipo$Year - as.numeric(as.character(ipo$found_year))
ipo$tech <- 0
ipo[Tech_ind > 0, tech := 1]
table(ipo$tech)
table(ipo$VC)

table4 <- ipo[, list(n = length(Deal_number),
                     n_VC = length(Deal_number[VC == "Yes"]),
                     n_noVC = length(Deal_number[VC == "No"]),
                     IR_VC = mean(IR[VC == "Yes"], na.rm = T),
                     IR_noVC = mean(IR[VC == "No"], na.rm = T),
                     age_VC = mean(age[VC == "Yes"], na.rm = T),
                     age_noVC = mean(age[VC == "No"], na.rm = T),
                     tech_VC = mean(tech[VC == "Yes"], na.rm = T),
                     tech_noVC = mean(tech[VC == "No"], na.rm = T)), by = Year]

table4 <- rbind(table4, 
                ipo[, list(n = length(Deal_number),
                           n_VC = length(Deal_number[VC == "Yes"]),
                           n_noVC = length(Deal_number[VC == "No"]),
                           IR_VC = mean(IR[VC == "Yes"], na.rm = T),
                           IR_noVC = mean(IR[VC == "No"], na.rm = T),
                           age_VC = mean(age[VC == "Yes"], na.rm = T),
                           age_noVC = mean(age[VC == "No"], na.rm = T),
                           tech_VC = mean(tech[VC == "Yes"], na.rm = T),
                           tech_noVC = mean(tech[VC == "No"], na.rm = T), Year = "Total")])

export(table4)
### table 5
ipo[, Gross_spread := as.numeric(as.character(Gross_spread))]
table5 <- ipo[Year >= 1980, list(n = length(Deal_number),
                     gs_less7 = mean(Gross_spread < 7, na.rm = T),
                     gs_eq7 = mean(Gross_spread == 7, na.rm = T),
                     gs_more7 = mean(Gross_spread > 7, na.rm = T),
                     gs_s =  mean(Gross_spread[size == "S"]/100, na.rm = T),
                     gs_m =  mean(Gross_spread[size == "M"]/100, na.rm = T),
                     gs_l =  mean(Gross_spread[size == "L"]/100, na.rm = T)), by = Year]

table5 <- rbind(table5 ,ipo[Year >= 1980, list(n = length(Deal_number),
                                 gs_less7 = mean(Gross_spread < 7, na.rm = T),
                                 gs_eq7 = mean(Gross_spread == 7, na.rm = T),
                                 gs_more7 = mean(Gross_spread > 7, na.rm = T),
                                 gs_s =  mean(Gross_spread[size == "S"], na.rm = T)/100,
                                 gs_m =  mean(Gross_spread[size == "M"], na.rm = T)/100,
                                 gs_l =  mean(Gross_spread[size == "L"], na.rm = T)/100,
                                 Year = "Total")])

export(table5)
### table 6
ipo[, n_lead := str_count(Mgr_codes,"BM")+str_count(Mgr_codes,"JB")+str_count(Mgr_codes,"JL"),
    by = Deal_number]
ipo[, n_co := str_count(Mgr_codes,"CM"), by = Deal_number]

table6 <- ipo[, list(n = length(Deal_number),
                     n_lean = mean(n_lead, na.rm = T), n_co = mean(n_co, na.rm = T),
                     n_lean_s = mean(n_lead[size == "S"], na.rm = T), 
                     n_co_s = mean(n_co[size == "S"], na.rm = T),
                     n_lean_m = mean(n_lead[size == "M"], na.rm = T), 
                     n_co_m = mean(n_co[size == "M"], na.rm = T),
                     n_lean_l = mean(n_lead[size == "L"], na.rm = T), 
                     n_co_l = mean(n_co[size == "L"], na.rm = T)), by = Year]

table6 <- rbind(table6, ipo[, list(n = length(Deal_number),
                     n_lean = mean(n_lead, na.rm = T), n_co = mean(n_co, na.rm = T),
                     n_lean_s = mean(n_lead[size == "S"], na.rm = T), 
                     n_co_s = mean(n_co[size == "S"], na.rm = T),
                     n_lean_m = mean(n_lead[size == "M"], na.rm = T), 
                     n_co_m = mean(n_co[size == "M"], na.rm = T),
                     n_lean_l = mean(n_lead[size == "L"], na.rm = T), 
                     n_co_l = mean(n_co[size == "L"], na.rm = T), Year = "Total")])

export(table6)
### table 7
ipo[, Issue_date := as.Date(Issue_date)]
ipo[, Filing_date := as.Date(Filing_date)]
ipo[, rp := as.numeric(as.character(Issue_date - Filing_date))]
table7 <- ipo[!is.na(rp), list(n = length(Deal_number[!is.na(rp)]),
                          rp  = mean(rp[!is.na(rp)], na.rm = T),
                          rp_s  = mean(rp[!is.na(rp) & size == "S"], na.rm = T),
                          rp_m  = mean(rp[!is.na(rp) & size == "M"], na.rm = T),
                          rp_l  = mean(rp[!is.na(rp) & size == "L"], na.rm = T)), by = Year]

table7 <- rbind(table7, ipo[!is.na(rp), list(n = length(Deal_number[!is.na(rp)]),
                               rp  = mean(rp[!is.na(rp)], na.rm = T),
                               rp_s  = mean(rp[!is.na(rp) & size == "S"], na.rm = T),
                               rp_m  = mean(rp[!is.na(rp) & size == "M"], na.rm = T),
                               rp_l  = mean(rp[!is.na(rp) & size == "L"], na.rm = T),
                               Year = "Total")])

export(table7)
### table 8
table8 <- ipo[Year >= 1980, list(n = length(Deal_number),
                               op  = mean(Offer_Price, na.rm = T),
                               op_s  = mean(Offer_Price[size == "S"], na.rm = T),
                               op_m  = mean(Offer_Price[size == "M"], na.rm = T),
                               op_l  = mean(Offer_Price[size == "L"], na.rm = T)), by = Year]

table8 <- rbind(table8, ipo[Year >= 1980, list(n = length(Deal_number),
                                  op  = mean(Offer_Price, na.rm = T),
                                  op_s  = mean(Offer_Price[size == "S"], na.rm = T),
                                  op_m  = mean(Offer_Price[size == "M"], na.rm = T),
                                  op_l  = mean(Offer_Price[size == "L"], na.rm = T),
                                  Year = "Total")])
export(table8)
source("./R codes/wrds connect.R") ### program to connect to WRDS in R
sql <- "select permno, dlstcd, endprc from CRSP.DSFHDR"
res <- dbSendQuery(wrds, sql)
delist <- fetch(res, n = -1)

match <- match(ipo$Permno, delist$PERMNO)
ipo$Delist_code <- delist$DLSTCD[match]
ipo$Delist_date <- delist$ENDPRC[match]
ipo[, Delist_date := as.Date(Delist_date)]

ipo[, Poor_perf := 0]
ipo[, Acq := 0]
ipo[Delist_code >= 400 & Delist_code < 600, Poor_perf := 1]
ipo[Delist_code >= 200 & Delist_code < 400, Acq := 1]


ipo[, ':=' (Poor_perf3 = 0,Poor_perf5 = 0, Poor_perf10 = 0)]
ipo[Delist_date - Issue_date < 3*365 & Poor_perf == 1, Poor_perf3 := 1]
ipo[Delist_date - Issue_date < 5*365 & Poor_perf == 1, Poor_perf5 := 1]
ipo[Delist_date - Issue_date < 10*365 & Poor_perf == 1, Poor_perf10 := 1]
today <- as.Date("2015-12-31")
ipo[today - Issue_date < 3*365, Poor_perf3 := NA]
ipo[today - Issue_date < 5*365, Poor_perf5 := NA]
ipo[today- Issue_date < 10*365, Poor_perf10 := NA]

ipo[, ':=' (Acq3 = 0,Acq5 = 0, Acq10 = 0)]
ipo[Delist_date - Issue_date < 3*365 & Acq == 1, Acq3 := 1]
ipo[Delist_date - Issue_date < 5*365 & Acq == 1, Acq5 := 1]
ipo[Delist_date - Issue_date < 10*365 & Acq == 1, Acq10 := 1]
ipo[today - Issue_date < 3*365, Acq3 := NA]
ipo[today - Issue_date < 5*365, Acq5 := NA]
ipo[today- Issue_date < 10*365, Acq10 := NA]

table9 <- ipo[, list(n = length(Deal_number), 
                     del3 = mean(Poor_perf3, na.rm = T),
                     del5 = mean(Poor_perf5, na.rm = T),
                     del10 = mean(Poor_perf10, na.rm = T),
                     acq3 = mean(Acq3, na.rm = T),
                     acq5 = mean(Acq5, na.rm = T),
                     acq10 = mean(Acq10, na.rm = T)), by = Year]

table9 <- rbind(table9, ipo[, list(n = length(Deal_number), 
                     del3 = mean(Poor_perf3, na.rm = T),
                     del5 = mean(Poor_perf5, na.rm = T),
                     del10 = mean(Poor_perf10, na.rm = T),
                     acq3 = mean(Acq3, na.rm = T),
                     acq5 = mean(Acq5, na.rm = T),
                     acq10 = mean(Acq10, na.rm = T), Year = "Total")])

export(table9)
### figure 9b
ipo[, n_lead := str_count(Mgr_codes,"BM")+str_count(Mgr_codes,"JB")+str_count(Mgr_codes,"JL"),
    by = Deal_number]
ipo[, n_co := str_count(Mgr_codes,"CM"), by = Deal_number]
ipo[, n_syn := str_count(Mgr_codes,"SD"), by = Deal_number]

figure9b <- ipo[Year >= 1997, list(n = length(Deal_number),
                     n_lean = mean(n_lead, na.rm = T), n_co = mean(n_co, na.rm = T),
                     n_syn = mean(n_syn, na.rm = T)), by = Year]

export(figure9b)

### figure 12a
ipo[, First_CRSP_date := as.Date(First_CRSP_date)]
ipo[, month := month(First_CRSP_date)]
setkey(ipo, month)
figure12a <- ipo[, list(n = length(Deal_number)), by = month]
export(figure12a)

ipo[, weekday := weekdays(First_CRSP_date)]
setkey(ipo, weekday)
figure12b <- ipo[, list(n = length(Deal_number)), by = weekday]
export(figure12b)

### figure 15 Panel (A) 
crsp <- NULL
permnos <- unique(c(ipo$Permno))
permnos <- permnos[!is.na(permnos)]

for(yr in 1972:2015)
{
  print(yr)
  tmp <- fread(paste0("/Volumes/SD_card/Yandex.Disk.localized/CRSP/DSF/CRSP_DSF_", yr, ".csv"), 
               select = c("PERMNO", "date","SHRCD", "EXCHCD","PRC", "RET", "CUSIP", "SHROUT"))
  crsp <- rbind(crsp, tmp)
}
crsp[, RET := as.numeric(as.character(RET))]
crsp <- crsp[!is.na(RET)]
require(lubridate)
crsp[, date := ymd(date)]
setkey(crsp, PERMNO, date)

crsp_ipo <- crsp[PERMNO %in% permnos]
crsp_ipo[, dif:= 1:.N, by = PERMNO]
crsp_ipo <- crsp_ipo[dif <= 251*3]
crsp_ipo[, BH3 := prod(1+RET, na.rm = T) - 1, by = PERMNO]

match <- match(ipo$Permno, crsp_ipo$PERMNO)
ipo$BH3 <- crsp_ipo$BH3[match]

setkey(ipo, Issue_date)
figure15a <- ipo[Year %in% 1973:2012, list(n = length(Deal_number), mean_ret3 = mean(BH3, na.rm = T), 
                        half_sd = 0.5*sd(BH3, na.rm = T)), by = Year]
export(figure15a)

win <- function(x, eps = 0.025)
{
  up <- quantile(x, na.rm = T, 1 - eps)
  down <- quantile(x, na.rm = T, eps)
  x[x>up] <- up
  x[x<down] <- down
  return(x)
}

### figure 15 (B)

ipo[, BH3_win := win(BH3), by = Year]
setkey(ipo, Issue_date)
figure15b <- ipo[Year %in% 1973:2012, list(n = length(Deal_number), mean_ret3 = mean(BH3_win, na.rm = T), 
                                           half_sd = 0.5*sd(BH3_win, na.rm = T)), by = Year]
export(figure15b)

### figure 15 (C)
#### calcualting BM for IPOs and matching
### excluding IPOs 
ipo[,`:=`(Py1 = paste(Permno, Year), Py2 = paste(Permno, Year - 1), Py3 = paste(Permno, Year -2),
             Py4 = paste(Permno, Year - 4), Py5 = paste(Permno, Year - 5))]
flags <- c(ipo$Py1, ipo$Py2, ipo$Py3, ipo$Py4, ipo$Py5)
### functions
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

### at first match by size AND book-to-market
match_BM <- function(ipo_id, crsp_tmp = crsp_cut)
{
  ipopermno <- ipo$Permno[ipo_id]
  iposize <- as.numeric(as.character(ipo$size[ipo_id]))
  ipobm <- as.numeric(as.character(ipo$BM[ipo_id]))
  if(is.na(ipopermno) | is.na(iposize) | is.na(ipobm)) return(NA)
  crsp_foo <- crsp_tmp[size >= 0.7*iposize & size <= 1.3*iposize]
  i <- which.min(abs(crsp_foo$BM - ipobm))
  return(crsp_foo$PERMNO[i])
}
get_port_rank <- function(crsp, year)
{
  crsp_june <- crsp
  crsp_june <- crsp_june[!is.na(size) & !is.na(BE) & (BE) > 0]
  crsp_june <- crsp_june[!duplicated(PERMNO)]
  crsp_june[, BM := BE/size]
  
  return(crsp_june)
}
crsp[, year := year(date)]
links <- get_links()
for(cyear in 1973:2012)
{
  print(cyear)
  ipo[, Year := year(Issue_date)]
  ind <- which(ipo$Year == cyear)
  comp <- get_comp(cyear)
  crsp_cut <- crsp[year == cyear & EXCHCD %in% 1:3 & SHRCD %in% 10:19 & !is.na(PRC)]
  
  crsp_cut$gvkey <- links$gvkey[match(crsp_cut$PERMNO, links$lpermno)]
  crsp_cut$BE <- comp$be[match(crsp_cut$gvkey, comp$gvkey)]
  crsp_cut[, size := abs(PRC)*SHROUT]
  crsp_cut <- get_port_rank(crsp_cut)
  ipo$size[ind] <- crsp_cut$size[match(ipo$Permno[ind], crsp_cut$PERMNO)] 
  ipo$BM[ind] <- crsp_cut$BM[match(ipo$Permno[ind], crsp_cut$PERMNO)] 

  crsp_cut <- crsp_cut[!paste(PERMNO, year) %in% flags]
  for(i in ind)
  {
    permno <- match_BM(i)
    crsp_cut <- crsp_cut[!PERMNO %in% permno]
    ipo$match_size_BM_permno1[i] <- permno
  }
}

### then match residuals by size only
match_size <- function(ipo_id, crsp_tmp = crsp_cut)
{
  ipopermno <- ipo$Permno[ipo_id]
  iposize <- as.numeric(as.character(ipo$size[ipo_id]))
  #ipobm <- as.numeric(as.character(ipo$BM[ipo_id]))
  if(is.na(ipopermno) | is.na(iposize)) return(NA)
 # crsp_foo <- crsp_tmp[size >= 0.7*iposize & size <= 1.3*iposize]
  i <- which.min(abs(crsp_cut$size - iposize))
  return(crsp_cut$PERMNO[i])
}

for(cyear in 1973:2012)
{
  print(cyear)
  #ipo[, Year := year(Issue_date)]
  ind <- which(ipo$Year == cyear & is.na(ipo$match_size_BM_permno1))
  print(length(ind))
  comp <- get_comp(cyear)
  crsp_cut <- crsp[year == cyear & EXCHCD %in% 1:3 & SHRCD %in% 10:19 & !is.na(PRC)]
  
  crsp_cut[, size := abs(PRC)*SHROUT]
  ipo$size[ind] <- crsp_cut$size[match(ipo$Permno[ind], crsp_cut$PERMNO)] 

  crsp_cut <- crsp_cut[!paste(PERMNO, year) %in% flags]
  for(i in ind)
  {
    permno <- match_size(i)
    crsp_cut <- crsp_cut[!PERMNO %in% permno]
    ipo$match_size_BM_permno1[i] <- permno
  }
}


ipo[Year %in% 1973:2012, mean(is.na(match_size_BM_permno1))]

### calculating matched returns
for(cyear in 1973:2012)
{
  print(cyear)
  ind <- which(ipo$Year == cyear & !is.na(ipo$match_size_BM_permno1))
  
  ipo_year <- ipo[ind]
  matched_permnos <- unique(ipo_year$match_size_BM_permno1)
  crsp_matched <- crsp[PERMNO %in% matched_permnos]

  match <- match(crsp_matched$PERMNO, ipo_year$match_size_BM_permno1)
  crsp_matched$ipo_date <- ipo_year$Issue_date[match]
  crsp_matched <- crsp_matched[date >= ipo_date]
  
  setkey(crsp_matched, PERMNO, date)
  crsp_matched[, dif:= 1:.N, by =PERMNO]
  crsp_matched <- crsp_matched[dif <= 251*3]
  crsp_matched[, BH3_matched := prod(1+RET, na.rm = T)- 1, by = PERMNO]
  match <- match(ipo$match_size_BM_permno1[ind], crsp_matched$PERMNO)
  ipo$matched_BH3[ind] <- crsp_matched$BH3_matched[match]
}

setkey(ipo, Issue_date)
ipo[, n:= 1:.N]
figure15c <- ipo[Year %in% 1973:2012, list(n = length(Deal_number), mean_ret3 = mean(matched_BH3, na.rm = T), 
                                           half_sd = 0.5*sd(matched_BH3, na.rm = T)), by = Year]
export(figure15c)

ipo[, matched_BH3_win := win(matched_BH3), by = Year]
figure15d <- ipo[Year %in% 1973:2012, list(n = length(Deal_number), mean_ret3 = mean(matched_BH3_win, na.rm = T), 
                                           half_sd = 0.5*sd(matched_BH3_win, na.rm = T)), by = Year]
export(figure15d)

### calculated large firm returns
crsp[, month := month(date)]

for(cyear in 1973:2012)
{
  print(cyear)
  crsp_cut <- crsp[year == cyear & month == 1]
  crsp_cut <- crsp_cut[!duplicated(PERMNO)]
  crsp_cut <- crsp_cut[!paste(PERMNO, year) %in% flags]
  crsp_cut[,size := abs(PRC)*SHROUT]
  crsp_cut <- crsp_cut[order( - size)]
  large_permno <- crsp_cut$PERMNO[1:min(1500, length(crsp_cut$PERMNO))]
  
  ind <- which(ipo$Year == cyear)
  ipo$match_snp1500[ind] <- sample(large_permno, length(ind))
}


for(cyear in 1973:2012)
{
  print(cyear)
  ind <- which(ipo$Year == cyear & !is.na(ipo$match_snp1500))
  
  ipo_year <- ipo[ind]
  matched_permnos <- unique(ipo_year$match_snp1500)
  crsp_matched <- crsp[PERMNO %in% matched_permnos]
  
  match <- match(crsp_matched$PERMNO, ipo_year$match_snp1500)
  crsp_matched$ipo_date <- ipo_year$Issue_date[match]
  crsp_matched <- crsp_matched[date >= ipo_date]
  
  setkey(crsp_matched, PERMNO, date)
  crsp_matched[, dif:= 1:.N, by =PERMNO]
  crsp_matched <- crsp_matched[dif <= 251*3]
  crsp_matched[, BH3_matched := prod(1+RET, na.rm = T)- 1, by = PERMNO]
  match <- match(ipo$match_snp1500[ind], crsp_matched$PERMNO)
  ipo$matched_snp1500_BH3[ind] <- crsp_matched$BH3_matched[match]
}

setkey(ipo, Issue_date)
ipo[, n:= 1:.N]
figure15e <- ipo[Year %in% 1973:2012, list(n = length(Deal_number), mean_ret3 = mean(matched_snp1500_BH3, na.rm = T), 
                                           half_sd = 0.5*sd(matched_snp1500_BH3, na.rm = T)), by = Year]
export(figure15e)

ipo[, matched_snp1500_BH3_win := win(matched_snp1500_BH3), by = Year]
figure15f <- ipo[Year %in% 1973:2012, list(n = length(Deal_number), mean_ret3 = mean(matched_snp1500_BH3_win, na.rm = T), 
                                           half_sd = 0.5*sd(matched_snp1500_BH3_win, na.rm = T)), by = Year]
export(figure15f)
#write.csv(ipo, "./IPO review chapter/Chapter write up/Data 20170315/ipo_all_variables.csv", row.names = F)

