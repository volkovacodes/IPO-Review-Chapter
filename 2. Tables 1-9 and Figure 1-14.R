#############################################
###!!!!!!!!!!!! README START !!!!!!!!!!!!####
#############################################
# This code uses clean IPO datafile to contruct 
# tables 1 though 9 and figures 1 though 15
# in Lowry, Michaely, Volkova, 
# "Initial Public Offerings: A synthesis of the literature and directions for future research",(2017)
# output tables are located in Tables.xlsx github file 
# output figures are located in Figures.xlsx github file
#############################################
###!!!!!!!!!!!! README END !!!!!!!!!!!!######
#############################################

### I used the file created by the previous code:
ipo.clean.datafile <- "./Projects/IPO review chapter/Chapter write up/Data 20170315/ipo.csv"
gdp.deflator.data <- "./Projects/IPO review chapter/Chapter write up/Data 20170315/GDPDEF.csv" # taken from https://fred.stlouisfed.org/series/GDPDEF
fitter.founding.year <- "./Projects/IPO review chapter/Chapter write up/Data 20170315/age7516.csv" # taken from https://site.warrington.ufl.edu/ritter/ipo-data/

### I will save ipo with with all variables to this file:
ipo.datafile <- "./Projects/IPO review chapter/Chapter write up/Data 20170315/ipo_all_variables.csv"
### loading packages:
require(data.table)
require(bit64)
require(stringr)
require(lubridate)

### loading IPO data:
ipo <-  as.data.table(read.csv(ipo.clean.datafile))
setkey(ipo, Issue_date)

ipo[, IR := Close_price1/Offer_Price - 1]
ipo[is.na(IR), IR := Close_price2/Offer_Price - 1]
n <- function(x) return(as.numeric(as.character(x)))
ipo[, Proceeds := n(gsub(",","", Proceeds))]

### price deflator data:
### get data from https://fred.stlouisfed.org/series/GDPDEF/downloaddata
gdp <- as.data.table(read.csv(gdp.deflator.data))
gdp[, year := year(DATE)]
gdp <- gdp[!duplicated(year)]
gdp[, base := GDPDEF[year == 2016]]
gdp[, factor := base/GDPDEF]
gdp[, inf := GDPDEF/shift(GDPDEF, 1) - 1]
match <- match(ipo$Year, gdp$year)
ipo$factor_2016 <- gdp$factor[match]
ipo$Proceeds_2016 <- ipo$Proceeds*ipo$factor_2016

ipo$size <- "N"
ipo[Proceeds_2016 <= 30, size := "S"]
ipo[Proceeds_2016 > 30 & Proceeds_2016 <= 120, size := "M"]
ipo[Proceeds_2016 > 120, size := "L"]


#############################################
###!!!!!!!!!!!! TABLE 1 !!!!!!!!!!!!########
#############################################
table1 <- ipo[, list(n = length(Deal_number), IR = mean(IR),
                     ave_proceeds = mean(Proceeds_2016), 
                     agg_proceeds = sum(Proceeds_2016)), by = Year]

figure1 <- data.frame(table1$Year, table1$agg_proceeds/1000, table1$n)
figure2 <- data.frame(table1$Year, table1$IR, table1$n)
figure3 <- data.frame(table1$Year, table1$IR, table1$agg_proceeds/1000)
table1 <- rbind(table1, ipo[, list(n = length(Deal_number), IR = mean(IR),
                           ave_proceeds = mean(Proceeds_2016), 
                           agg_proceeds = sum(Proceeds_2016), Year = "Total")])


#############################################
###!!!!!!!!!!!! TABLE 2 !!!!!!!!!!!!########
#############################################
table2 <- ipo[, list(n = length(Deal_number),
                     n_s = length(Deal_number[size == "S"]),
                     n_m = length(Deal_number[size == "M"]),
                     n_l = length(Deal_number[size == "L"]),
                     IR_s = mean(IR[size == "S"], na.rm = T),
                     IR_m = mean(IR[size == "M"], na.rm = T),
                     IR_l = mean(IR[size == "L"], na.rm = T)), by = Year]

figure4 <- table2[Year >= 1980]

table2 <- rbind(table2, ipo[, list(n = length(Deal_number),
           n_s = length(Deal_number[size == "S"]),
           n_m = length(Deal_number[size == "M"]),
           n_l = length(Deal_number[size == "L"]),
           IR_s = mean(IR[size == "S"], na.rm = T),
           IR_m = mean(IR[size == "M"], na.rm = T),
           IR_l = mean(IR[size == "L"], na.rm = T), Year = "Total")])

#############################################
###!!!!!!!!!!!! TABLE 3 !!!!!!!!!!!!#########
#############################################
# checking whether IPO is priced below/within/above the *initial* price range
# SDC does not have this information prior to March-1983
# - between 1983 and 1996 we use only LFILE (HFILE) SDC variables 
#   for low (high) original filing price
# - after 1996 LFILE (HFILE) variables first and 
#   then if the information in the variable is missing we use first 
#   non-missing value in AH_LFILE (AH_HFILE) varible

ipo[, Issue_priced_range := "NA"]
# info from LFILE/HFILE variables
ipo[Year >= 1983 & Low_Price  != "", constructed_low  := n(gsub(",", "", Low_Price))]
ipo[Year >= 1983 & High_Price != "", constructed_high := n(gsub(",", "", High_Price))]
# info from AH_LFILE/AH_HFILE (ammendment price history list) variables
first_price <- function(a)
{
  prices <- strsplit(as.character(a), split = "\n")
  prices <- lapply(prices, function(x) return(as.numeric(as.character(x))))
  prices <- lapply(prices, function(x) return(x[!is.na(x)]))
  first_element_or_na <- function(x)
  {
    if(length(x) >= 1) return(x[1])
    if(length(x) == 0) return(NA)
  }
  prices <- lapply(prices, first_element_or_na)
  return(unlist(prices))
}

a <- ipo[Year >= 1996 & is.na(constructed_low)]$Low_Price_History
ipo[Year >= 1996 & is.na(constructed_low)]$constructed_low <- first_price(a)
b <- ipo[Year >= 1996 & is.na(constructed_high)]$High_Price_History
ipo[Year >= 1996 & is.na(constructed_high)]$constructed_high <- first_price(b)

not_missing <- (!is.na(ipo$constructed_low)) & (!is.na(ipo$constructed_high))
min_range <- 0.5*(ipo$constructed_low + ipo$constructed_high) - 0.5*abs(ipo$constructed_low - ipo$constructed_high)
max_range <- 0.5*(ipo$constructed_low + ipo$constructed_high) + 0.5*abs(ipo$constructed_low - ipo$constructed_high)

ipo[not_missing & Offer_Price < min_range, Issue_priced_range := "Below range"]
ipo[not_missing & Offer_Price >= min_range & Offer_Price <= max_range, Issue_priced_range := "Within range"]
ipo[not_missing & Offer_Price > max_range, Issue_priced_range := "Above range"]


table3 <- ipo[grep("range",Issue_priced_range), list(n = length(Deal_number),
                     n_b = length(Deal_number[grep("Below", Issue_priced_range)]),
                     n_w = length(Deal_number[grep("Within", Issue_priced_range)]),
                     n_a = length(Deal_number[grep("Above", Issue_priced_range)]),
                     IR_b = mean(IR[grep("Below", Issue_priced_range)], na.rm = T),
                     IR_w = mean(IR[grep("Within", Issue_priced_range)], na.rm = T),
                     IR_a = mean(IR[grep("Above", Issue_priced_range)], na.rm = T)), by = Year]

figure5 <- data.frame(table3$Year, table3$n_b, table3$n_w, table3$n_a, table3$IR_b, table3$IR_w, table3$IR_a)

table3 <- rbind(table3, ipo[grep("range",Issue_priced_range), list(n = length(Deal_number),
                                                                   n_b = length(Deal_number[grep("Below", Issue_priced_range)]),
                                                                   n_w = length(Deal_number[grep("Within", Issue_priced_range)]),
                                                                   n_a = length(Deal_number[grep("Above", Issue_priced_range)]),
                                                                   IR_b = mean(IR[grep("Below", Issue_priced_range)], na.rm = T),
                                                                   IR_w = mean(IR[grep("Within", Issue_priced_range)], na.rm = T),
                                                                   IR_a = mean(IR[grep("Above", Issue_priced_range)], na.rm = T), 
                                                                   Year = "Total")])


#############################################
###!!!!!!!!!!!! TABLE 4 !!!!!!!!!!!!#########
#############################################
### get data about IPO age from Jay Ritter website
age <- read.csv(fitter.founding.year)
age <- age[age$Founding != -99,]
ipo$found_year <- age$Founding[match(ipo$Permno, age$CRSP.perm)]

### for 2016 I will match my cusip
age$Year <- year(ymd(age$Offer.date))
age <- age[age$Year == 2016 & str_length(age$CUSIP) == 9,]
ipo[Year==2016]$found_year <- age$Founding[match(ipo[Year == 2016]$CUSIP8, substr(age$CUSIP,1,8))]
ipo$age <- ipo$Year - as.numeric(as.character(ipo$found_year))
ipo$tech <- 0
ipo[Tech_ind > 0, tech := 1]

table4 <- ipo[, list(n = length(Deal_number),
                     n_VC = length(Deal_number[VC == "Yes"]),
                     n_noVC = length(Deal_number[VC == "No"]),
                     IR_VC = mean(IR[VC == "Yes"], na.rm = T),
                     IR_noVC = mean(IR[VC == "No"], na.rm = T),
                     age_VC = mean(age[VC == "Yes"], na.rm = T),
                     age_noVC = mean(age[VC == "No"], na.rm = T),
                     tech_VC = mean(tech[VC == "Yes"], na.rm = T),
                     tech_noVC = mean(tech[VC == "No"], na.rm = T)), by = Year]

figure6 <- data.frame(table4$Year, table4$n_VC, table4$n_noVC, table4$IR_VC, table4$IR_noVC)
figure6 <- figure6[figure6$table4.Year >= 1980,]
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

#############################################
###!!!!!!!!!!!! TABLE 5 !!!!!!!!!!!!#########
#############################################
ipo[, Gross_spread := as.numeric(as.character(Gross_spread))]
table5 <- ipo[Year >= 1980, list(n = length(Deal_number),
                     gs_less7 = mean(Gross_spread < 7, na.rm = T),
                     gs_eq7 = mean(Gross_spread == 7, na.rm = T),
                     gs_more7 = mean(Gross_spread > 7, na.rm = T),
                     gs_s =  mean(Gross_spread[size == "S"]/100, na.rm = T),
                     gs_m =  mean(Gross_spread[size == "M"]/100, na.rm = T),
                     gs_l =  mean(Gross_spread[size == "L"]/100, na.rm = T)), by = Year]

figure7 <- data.frame(table5$Year, table5$gs_less7, table5$gs_eq7, table5$gs_more7)
figure8 <- data.frame(table5$Year, table5$gs_s, table5$gs_m, table5$gs_l)
table5 <- rbind(table5 ,ipo[Year >= 1980, list(n = length(Deal_number),
                                 gs_less7 = mean(Gross_spread < 7, na.rm = T),
                                 gs_eq7 = mean(Gross_spread == 7, na.rm = T),
                                 gs_more7 = mean(Gross_spread > 7, na.rm = T),
                                 gs_s =  mean(Gross_spread[size == "S"], na.rm = T)/100,
                                 gs_m =  mean(Gross_spread[size == "M"], na.rm = T)/100,
                                 gs_l =  mean(Gross_spread[size == "L"], na.rm = T)/100,
                                 Year = "Total")])


#############################################
###!!!!!!!!!!!! TABLE 6 !!!!!!!!!!!!#########
#############################################
ipo[, n_lead := str_count(Mgr_codes,"BM")+str_count(Mgr_codes,"JB")+str_count(Mgr_codes,"JL"), by = Deal_number]
ipo[, n_co := str_count(Mgr_codes,"CM"), by = Deal_number]
ipo[, n_syn := str_count(Mgr_codes,"SD"), by = Deal_number]

table6 <- ipo[, list(n = length(Deal_number),
                     n_lean = mean(n_lead, na.rm = T), n_co = mean(n_co, na.rm = T),
                     n_lean_s = mean(n_lead[size == "S"], na.rm = T), 
                     n_co_s = mean(n_co[size == "S"], na.rm = T),
                     n_lean_m = mean(n_lead[size == "M"], na.rm = T), 
                     n_co_m = mean(n_co[size == "M"], na.rm = T),
                     n_lean_l = mean(n_lead[size == "L"], na.rm = T), 
                     n_co_l = mean(n_co[size == "L"], na.rm = T)), by = Year]

figure9a <- data.frame(table6$Year, table6$n_lean, table6$n_co)

table6 <- rbind(table6, ipo[, list(n = length(Deal_number),
                     n_lean = mean(n_lead, na.rm = T), n_co = mean(n_co, na.rm = T),
                     n_lean_s = mean(n_lead[size == "S"], na.rm = T), 
                     n_co_s = mean(n_co[size == "S"], na.rm = T),
                     n_lean_m = mean(n_lead[size == "M"], na.rm = T), 
                     n_co_m = mean(n_co[size == "M"], na.rm = T),
                     n_lean_l = mean(n_lead[size == "L"], na.rm = T), 
                     n_co_l = mean(n_co[size == "L"], na.rm = T), Year = "Total")])


figure9b <- ipo[Year >= 1997, list(n_lean = mean(n_lead, na.rm = T), n_co = mean(n_co, na.rm = T),
                                   n_syn = mean(n_syn, na.rm = T)), by = Year]


#############################################
###!!!!!!!!!!!! TABLE 7 !!!!!!!!!!!!#########
#############################################
ipo[, Issue_date := as.Date(Issue_date)]
ipo[, Filing_date := as.Date(Filing_date)]
ipo[, rp := as.numeric(as.character(Issue_date - Filing_date))]
table7 <- ipo[!is.na(rp), list(n = length(Deal_number[!is.na(rp)]),
                          rp  = mean(rp[!is.na(rp)], na.rm = T),
                          rp_s  = mean(rp[!is.na(rp) & size == "S"], na.rm = T),
                          rp_m  = mean(rp[!is.na(rp) & size == "M"], na.rm = T),
                          rp_l  = mean(rp[!is.na(rp) & size == "L"], na.rm = T)), by = Year]

figure10 <- data.frame(table7$Year, table7$n, table7$rp)
table7 <- rbind(table7, ipo[!is.na(rp), list(n = length(Deal_number[!is.na(rp)]),
                               rp  = mean(rp[!is.na(rp)], na.rm = T),
                               rp_s  = mean(rp[!is.na(rp) & size == "S"], na.rm = T),
                               rp_m  = mean(rp[!is.na(rp) & size == "M"], na.rm = T),
                               rp_l  = mean(rp[!is.na(rp) & size == "L"], na.rm = T),
                               Year = "Total")])
#############################################
###!!!!!!!!!!!! FIGURE 12 !!!!!!!!!!!!#######
#############################################

ipo[, First_CRSP_date := as.Date(First_CRSP_date)]
ipo[, month := month(First_CRSP_date)]
setkey(ipo, month)
figure12a <- ipo[, list(n = length(Deal_number)), by = month]

ipo[, weekday := weekdays(First_CRSP_date)]
setkey(ipo, weekday)
figure12b <- ipo[, list(n = length(Deal_number)), by = weekday]

#############################################
###!!!!!!!!!!!! TABLE 8 !!!!!!!!!!!!#########
#############################################
setkey(ipo, Issue_date)
table8 <- ipo[Year >= 1980, list(n = length(Deal_number),
                               op  = mean(Offer_Price, na.rm = T),
                               op_s  = mean(Offer_Price[size == "S"], na.rm = T),
                               op_m  = mean(Offer_Price[size == "M"], na.rm = T),
                               op_l  = mean(Offer_Price[size == "L"], na.rm = T)), by = Year]

figure13 <- data.frame(table8$Year, table8$op_s, table8$op_m, table8$op_l)

table8 <- rbind(table8, ipo[Year >= 1980, list(n = length(Deal_number),
                                  op  = mean(Offer_Price, na.rm = T),
                                  op_s  = mean(Offer_Price[size == "S"], na.rm = T),
                                  op_m  = mean(Offer_Price[size == "M"], na.rm = T),
                                  op_l  = mean(Offer_Price[size == "L"], na.rm = T),
                                  Year = "Total")])

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
today <- as.Date("2016-12-31")
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

table9 <- ipo[Year %in% 1973:2013, list(n = length(Deal_number), 
                     del3 = mean(Poor_perf3, na.rm = T),
                     del5 = mean(Poor_perf5, na.rm = T),
                     del10 = mean(Poor_perf10, na.rm = T),
                     acq3 = mean(Acq3, na.rm = T),
                     acq5 = mean(Acq5, na.rm = T),
                     acq10 = mean(Acq10, na.rm = T)), by = Year]

figure14a <- data.frame(table9$Year, table9$n, table9$del3, table9$del5, table9$del10)
figure14b <- data.frame(table9$Year, table9$n, table9$acq3, table9$acq5, table9$acq10)

table9 <- rbind(table9, ipo[Year %in% 1973:2013, list(n = length(Deal_number), 
                     del3 = mean(Poor_perf3, na.rm = T),
                     del5 = mean(Poor_perf5, na.rm = T),
                     del10 = mean(Poor_perf10, na.rm = T),
                     acq3 = mean(Acq3, na.rm = T),
                     acq5 = mean(Acq5, na.rm = T),
                     acq10 = mean(Acq10, na.rm = T), Year = "Total")])

write.csv(ipo, ipo.datafile, row.names = F)
export <- function(x)
{
  clip <- pipe("pbcopy", "w") 
  write.table(x, file=clip, sep = "\t", row.names = F) 
  close(clip)
}