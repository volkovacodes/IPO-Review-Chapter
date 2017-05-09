require(data.table)
require(Hmisc)
require(lfe)
require(psych)
require(stargazer)
require(bit64)

ipo <- fread("./IPO review chapter/Chapter write up/SDC-IPO-data/US_IPOs_1970_2015_clean.csv")

permnos <- unique(ipo$Permno)
load(,file = "./CRSP_COMP/crsp_monthly_1970_2015.rda")

crsp_monthly <- crsp_monthly[PERMNO %in% permnos]
match <- match(crsp_monthly$PERMNO, ipo$Permno)
crsp_monthly$ipo_date <- as.Date(as.character(ipo$Issue_date[match]))
crsp_monthly[, date := as.Date(as.character(date), format = "%Y%m%d")]
crsp_monthly[, dif := date - ipo_date]

### we do not care after 5 years
crsp_monthly <- crsp_monthly[dif <= 5*365]
crsp_monthly[, RET := as.numeric(as.character(RET))]
crsp_monthly <- crsp_monthly[!is.na(RET)]
crsp_monthly[, eq := mean(RET, na.rm = T), by = date]
crsp_monthly[, marcap := abs(PRC)*SHROUT]
crsp_monthly[, L.marcap := shift(marcap, 1), by = PERMNO]
crsp <- crsp_monthly[!is.na(L.marcap)]
crsp[, value := weighted.mean(RET, L.marcap, na.rm = T), by = date]

source("./R codes/wrds connect.R")
sql <-  "select date, mktrf,smb,hml,umd,rf,year, dateff from FF.FACTORS_MONTHLY where year between 1970 and 2016"
res <- dbSendQuery(wrds, sql)
fdata <- fetch(res, n = -1)
fdata <- as.data.table(fdata)

match <- match(as.Date(as.character(fdata$dateff)), crsp$date)
fdata$e_port <- crsp$eq[match]
fdata$v_port <- crsp$value[match]
fdata[, year := year(as.Date(as.character(dateff)))]

model <- as.formula("I(v_port - rf) ~ mktrf + smb + hml + umd|0|0")
reg <- NULL
reg[[1]] <- felm(model, data = fdata)
reg[[2]] <- felm(model, data = fdata[year %in% 1973:1979])
reg[[3]] <- felm(model, data = fdata[year %in% 1980:1989])
reg[[4]] <- felm(model, data = fdata[year %in% 1990:1999])
reg[[5]] <- felm(model, data = fdata[year %in% 2000:2012])
#reg[[6]] <- felm(model, data = fdata[year %in% 1977:1994])


#summary(reg[[1]], robust = T)
#summary(reg[[6]], robust = F)
require(foreign)
#write.dta(fdata, "fdata.dta")
stargazer(reg, type = "text", report = "cst*")

out <- stargazer(reg, type = "html",keep.stat = c("n", "adj.rsq"),intercept.bottom = F, 
                 rownames = F)

write(out, "test.htm")
