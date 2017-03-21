require(data.table)
require(bit64)
### reading first SDC request
ipo <- read.csv("./IPO review chapter/Chapter write up/Data 20170315/IPO data all.csv")
### changing names of the variables
colnames(ipo) <- c("Filing_date", "Issue_date", "Issuer", "State", "Nation", "Offer_Price",
                   "Type", "Description", "REIT", "Unit", "Depositary", "Deal_number", "CEF", 
                   "CUSIP", "CUSIP9", "Proceeds", "Issue_priced_range", "VC", "Gross_spread", 
                   "Mgr_codes","Tech_ind", "X")
ipo$X <- NULL
ipo <- as.data.table(ipo)

### format dates
ipo[, Filing_date := as.Date(Filing_date, format = "%m/%d/%y")]
ipo[, Issue_date := as.Date(Issue_date, format = "%m/%d/%y")]
ipo[, Offer_Price := gsub(",","", Offer_Price)]
ipo[, Offer_Price := as.numeric(as.character(Offer_Price))]

### types of securities we will keep in the data (common/ordinary shares)
### 40,525 obs before ---> 37,692 after
### Comment: sometimes Ritter codes "MLP-Common Shs" as units =1
ex_types <- c("Units", "Ltd Prtnr Int", "MLP-Common Shs", "Shs Benficl Int",
              "Ltd Liab Int", "Stock Unit", "Trust Units", "Beneficial Ints")
ipo <- ipo[!Type %in% ex_types,]

### creating 8-digit CUSIP to match with CRSP
ipo[, CUSIP8 := paste0(CUSIP, 10)]
require(stringr)
ipo[str_length(CUSIP9) > 1, CUSIP8 := substr(CUSIP9, 1,8)]

### keeping US obs only 27,357 obs before ---> 27,356
ipo <- ipo[Nation == "United States"]

### loading CRSP monthly data to extract PERMNO
load("./CRSP_COMP/crsp_monthly_1970_2015.rda")
match <- match(ipo$CUSIP8, crsp_monthly$CUSIP)
ipo$Permno_cusip <- crsp_monthly$PERMNO[match]
match <- match(ipo$CUSIP8, crsp_monthly$NCUSIP)
ipo$Permno_ncusip <- crsp_monthly$PERMNO[match]

crsp_monthly[, cusip6 := substr(CUSIP,1,6)]
match <- match(ipo$CUSIP, crsp_monthly$cusip6)
ipo$Permno_ncusip6 <- crsp_monthly$PERMNO[match]
rm(crsp_monthly)

df <- NULL
ipo[, Year := as.numeric(substr(Issue_date,1,4))]
df <- data.frame(Year = ipo$Year, Permno = ipo$Permno_ncusip)
write.csv(df, "./IPO review chapter/Chapter write up/SDC-PULL-IPO/ipo_seo_permno.csv", row.names = F)
