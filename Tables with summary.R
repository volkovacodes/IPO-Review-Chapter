require(data.table)
require(bit64)
require(stringr)
ipo <- read.csv("./IPO review chapter/Chapter write up/Data 20170315/ipo.csv")

ipo <- as.data.table(ipo)
setkey(ipo, Issue_date)
ipo[, Year := year(Issue_date)]
ipo[, IR := Close_price1/Offer_Price - 1]
ipo[is.na(IR), IR := Close_price2/Offer_Price -1]
ipo[, Proceeds := as.numeric(as.character(gsub(",","", Proceeds)))]

### get data from https://fred.stlouisfed.org/series/GDPDEF/downloaddata
gdp <- read.csv("./IPO review chapter/Chapter write up/Data 20170315/GDPDEF.csv")
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

### table 1 
table1 <- ipo[, list(n = length(Deal_number), IR = mean(IR),
                     ave_proceeds = mean(Proceeds_2015), 
                     agg_proceeds = sum(Proceeds_2015)), by = Year]

table1 <- rbind(table1, ipo[, list(n = length(Deal_number), IR = mean(IR),
                           ave_proceeds = mean(Proceeds_2015), 
                           agg_proceeds = sum(Proceeds_2015), Year = "Total")])
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

### table 4
require(xlsx)
### get data about IPO age from Jay Ritter website
age <- read.xlsx2("./IPO review chapter/Chapter write up/Data 20170315/age7515.xlsx",1)
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

### figure 9b
ipo[, n_lead := str_count(Mgr_codes,"BM")+str_count(Mgr_codes,"JB")+str_count(Mgr_codes,"JL"),
    by = Deal_number]
ipo[, n_co := str_count(Mgr_codes,"CM"), by = Deal_number]
ipo[, n_syn := str_count(Mgr_codes,"SD"), by = Deal_number]

figure9b <- ipo[Year >= 1997, list(n = length(Deal_number),
                     n_lean = mean(n_lead, na.rm = T), n_co = mean(n_co, na.rm = T),
                     n_syn = mean(n_syn, na.rm = T)), by = Year]



### figure 12a
ipo[, First_CRSP_date := as.Date(First_CRSP_date)]
ipo[, month := month(First_CRSP_date)]
setkey(ipo, month)
figure12a <- ipo[, list(n = length(Deal_number)), by = month]

ipo[, weekday := weekdays(First_CRSP_date)]
setkey(ipo, weekday)
figure12b <- ipo[, list(n = length(Deal_number)), by = weekday]

clip <- pipe("pbcopy", "w") 
write.table(figure12b, file=clip, sep = "\t", row.names = F) 
close(clip)
write.csv(ipo, "./IPO review chapter/Chapter write up/Data 20170315/ipo_all_variables.csv", row.names = F)

