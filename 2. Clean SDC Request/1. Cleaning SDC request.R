#############################################
###!!!!!!!!!!!! README START !!!!!!!!!!!!####
#############################################
# This code cleans IPO data extracted from SDC
# Word file in github golder explain the details of the data extraction from SDC
# for this code you would need:
# - extracted file with IPO data in .csv format
# - file with daily CRSP data that includes: date, CUSIP, NCUSIP and PERMNO for
#   for all US companies between 01-01-1973 and 12-31-2016
#############################################
###!!!!!!!!!!!! README END !!!!!!!!!!!!######
#############################################

### path to the file file "ditry" with IPO data
ipo.datafile <- "./Projects/IPO review chapter/Chapter write up/Data 20170315/IPO data all.csv"
### path to the file with CRSP data (I use .rds format for faster reading)
crsp.datafile <- "./Projects/IPO review chapter/Chapter write up/Data 20170315/crsp.rds"

### I will write results to this file:
ipo.clean.datafile <- "./Projects/IPO review chapter/Chapter write up/Data 20170315/ipo.csv"

### loading packages:
require(data.table)
require(bit64)
require(lubridate)
require(stringr)

### reading first SDC request
ipo <- read.csv(ipo.datafile)
### changing names of the variables
colnames(ipo) <- c("Filing_date", "Issue_date", "Issuer", "State", "Nation", "Offer_Price",
                   "Type", "Description", "REIT", "Unit", "Depositary", "Deal_number", "CEF", 
                   "CUSIP", "CUSIP9", "Proceeds",  "VC", "Gross_spread", 
                   "Mgr_codes","Tech_ind", "Low_Price", "High_Price", "Low_Price_History", "High_Price_History")
ipo <- as.data.table(ipo)

### format dates
ipo[, `:=`(Filing_date = mdy(Filing_date), Issue_date = mdy(Issue_date))]
ipo[, Year := year(Issue_date)]
### delete commas from the Offer_Price and convert in to numeric format
ipo[, Offer_Price := gsub(",","", Offer_Price)]
ipo[, Offer_Price := as.numeric(as.character(Offer_Price))]

### types of securities we will keep in the data (common/ordinary shares)
### 38459 obs before ---> 35,516 after
### Comment: sometimes Ritter codes "MLP-Common Shs" as units =1
ex_types <- c("Units", "Ltd Prtnr Int", "MLP-Common Shs", "Shs Benficl Int",
             "Ltd Liab Int", "Stock Unit", "Trust Units", "Beneficial Ints")
ipo <- ipo[!Type %in% ex_types,]

### drop REIT, Units, ADR, penny stocks and CEF
### 35,516 obs before ---> 26,426 obs after
logic <- ipo$REIT == "" & (ipo$Unit == "No" | ipo$Unit == "") &  ipo$Depositary == "No" & 
  !is.na(ipo$Offer_Price) & ipo$Offer_Price >= 5 & 
  ipo$CEF == "No" 
ipo <- ipo[logic,]

### creating 8-digit CUSIP to match with CRSP
ipo[, CUSIP8 := paste0(CUSIP, 10)]
ipo[str_length(CUSIP9) > 1, CUSIP8 := substr(CUSIP9, 1,8)]

### loading CRSP data
print(Sys.time())
crsp <- readRDS(crsp.datafile)
crsp[, `:=` (CUSIP6 = substr(CUSIP, 1, 6), NCUSIP6 = substr(NCUSIP, 1, 6))]
crsp <- crsp[!is.na(PRC)]
print(Sys.time())

### matching IPO and CRSP
ipo$Permno_cusip <- crsp$PERMNO[match(ipo$CUSIP8, crsp$CUSIP)]
ipo$Permno_ncusip <- crsp$PERMNO[match(ipo$CUSIP8, crsp$NCUSIP)]
ipo$Permno_cusip6 <- crsp$PERMNO[match(substr(ipo$CUSIP8,1,6), crsp$CUSIP6)]
ipo$Permno_ncusip6 <- crsp$PERMNO[match(substr(ipo$CUSIP8,1,6), crsp$NCUSIP6)]

### here I do matching by both CUSIP and NCUSIP
### and keep the match that appears to CRSP within 7 days
match <- match(ipo$Permno_cusip, crsp$PERMNO)
ipo$First_CRSP_date_cusip <- ymd(crsp$date[match])

match <- match(ipo$Permno_ncusip, crsp$PERMNO)
ipo$First_CRSP_date_ncusip <- ymd(crsp$date[match])

match <- match(ipo$Permno_ncusip6, crsp$PERMNO)
ipo$First_CRSP_date_ncusip6 <- ymd(crsp$date[match])

match <- match(ipo$Permno_cusip6, crsp$PERMNO)
ipo$First_CRSP_date_cusip6 <- ymd(crsp$date[match])


### here I decide which match to select
n <- function(x) return(as.numeric(as.character(x)))
ipo[, Permno := -999]
ipo[, dif := n(First_CRSP_date_ncusip - Issue_date)]
ipo[(dif >= -1 & dif <= 7) & (Permno == - 999), Permno := Permno_ncusip] 

ipo[, dif := n(First_CRSP_date_ncusip - Issue_date)]
ipo[(dif >= -1  & dif <= 7) &  (Permno == - 999), Permno := Permno_cusip] 

ipo[, dif := n(First_CRSP_date_ncusip6 - Issue_date)]
ipo[ (dif >= -1 & dif <= 7) & (Permno == - 999), Permno := Permno_ncusip6]

ipo[, dif := n(First_CRSP_date_cusip6 - Issue_date)]
ipo[ (dif >= -1 & dif <= 7) & (Permno == - 999), Permno := Permno_cusip6]

m <- match(ipo$Permno, crsp$PERMNO)
ipo[, `:=` (First_CRSP_date = ymd(crsp$date[m]), Close_price1 = abs(crsp$PRC[m]), Close_price2 = abs(crsp$PRC[m + 1]))]

### dropping IPOs that do not appear on CRSP (and SEOs that were on CRSP before)
ipo <- ipo[!is.na(Permno) & Permno != -999]

ipo[,`:=`(First_CRSP_date_cusip = NULL, First_CRSP_date_ncusip = NULL, First_CRSP_date_cusip6 = NULL, First_CRSP_date_ncusip6 = NULL)]
ipo[, `:=` (Permno_cusip = NULL, Permno_ncusip = NULL, Permno_cusip6 = NULL, Permno_ncusip6 = NULL, dif = NULL)]

unique <- !duplicated(ipo$Permno)
ipo <- ipo[unique]
write.csv(ipo, ipo.clean.datafile, row.names = F)
