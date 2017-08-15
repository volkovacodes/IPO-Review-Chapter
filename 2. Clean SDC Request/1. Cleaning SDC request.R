#############################################
###!!!!!!!!!!!! README START !!!!!!!!!!!!####
#############################################
# This code cleans IPO data extracted from SDC
# Word file in github golder explain the details of the data extraction from SDC
# for this code you would need:
# - extracted file with IPO data in .csv format
# - file with daily CRSP data that includes: date, NCUSIP, PERMNO, PRC, RET, SHROUT for
#   for all US companies between 01-01-1973 and 12-31-2016
#############################################
###!!!!!!!!!!!! README END !!!!!!!!!!!!######
#############################################

###  file "ditry" with IPO data
ipo.datafile <- "IPO data all.csv"
### path to the file with CRSP data (I use .rds format for faster reading)
crsp.datafile <- "crsp.rds"
crsp.info.datafile <- "crsp_info.csv"
### I will write results to this file:
ipo.clean.datafile <- "ipo.csv"

### loading packages:
require(data.table)
require(bit64)
require(lubridate)
require(stringr)

### reading first SDC request
ipo <- read.csv(ipo.datafile)
### changing names of the variables

colnames(ipo) <- c("Filing_date", "Issue_date", "Issuer", "State", "Nation","IPO_Flag", "Original_IPO_Flag",
                   "Offer_Price", "Type", "Description", "REIT", "Unit", "Depositary", "Deal_number", "CEF", 
                   "CUSIP", "CUSIP9", "Proceeds", "VC", "Gross_spread", 
                  "Mgr_codes","Tech_ind", "Low_Price", "High_Price", "Low_Price_History", "High_Price_History")
ipo <- as.data.table(ipo)

### The exact number of downloaded observation depends on SDC version
### In our sample SDC report consisnt of 43817 obs

### keep only IPOs
<<<<<<< HEAD
### 43,817 obs ---> 16,454
=======
### 43.817 obs -> 16,454
>>>>>>> origin/master
ipo <- ipo[IPO_Flag != "No" & Original_IPO_Flag != "No"]
### format dates
ipo[, `:=`(Filing_date = mdy(Filing_date), Issue_date = mdy(Issue_date))]
ipo[, Year := year(Issue_date)]
### delete commas from the Offer_Price and convert in to numeric format
ipo[, Offer_Price := gsub(",","", Offer_Price)]
ipo[, Offer_Price := as.numeric(as.character(Offer_Price))]

### types of securities we will keep in the data (common/ordinary shares)
<<<<<<< HEAD
### 16,454 obs before ---> 15,107 obs after
=======
### 15,454 obs before ---> 15,107 obs after
>>>>>>> origin/master
### Comment: sometimes Ritter codes "MLP-Common Shs" as units =1
ex_types <- c("Units", "Ltd Prtnr Int", "MLP-Common Shs", "Shs Benficl Int",
             "Ltd Liab Int", "Stock Unit", "Trust Units", "Beneficial Ints")
ipo <- ipo[!Type %in% ex_types,]

### drop REIT, Units, ADR, penny stocks and CEF
### 15,107 obs before ---> 11,103 obs after
logic <- ipo$REIT == "" & (ipo$Unit == "No" | ipo$Unit == "") &  ipo$Depositary == "No" & 
  !is.na(ipo$Offer_Price) & ipo$Offer_Price >= 5 & 
  ipo$CEF == "No" 
ipo <- ipo[logic,]

### creating 8-digit CUSIP to match with CRSP
ipo[, CUSIP8 := paste0(CUSIP, 10)]
ipo[str_length(CUSIP9) > 1, CUSIP8 := substr(CUSIP9, 1,8)]

### loading CRSP daily data
print(Sys.time())
crsp <- readRDS(crsp.datafile)
crsp <- crsp[!is.na(PRC)]
crsp[, `:=` (NCUSIP6 = substr(NCUSIP, 1, 6))]
print(Sys.time())

### matching IPO and CRSP
ipo$Permno_ncusip <- crsp$PERMNO[match(ipo$CUSIP8, crsp$NCUSIP)]
ipo$Permno_ncusip6 <- crsp$PERMNO[match(substr(ipo$CUSIP8,1,6), crsp$NCUSIP6)]

### here I do matching by both CUSIP and NCUSIP
### and keep the match that appears to CRSP within 7 days
match <- match(ipo$Permno_ncusip, crsp$PERMNO)
ipo$First_CRSP_date_ncusip <- ymd(crsp$date[match])

match <- match(ipo$Permno_ncusip6, crsp$PERMNO)
ipo$First_CRSP_date_ncusip6 <- ymd(crsp$date[match])

### here I decide which match to select
n <- function(x) return(as.numeric(as.character(x)))
ipo[, Permno := -999]
ipo[, dif := n(First_CRSP_date_ncusip - Issue_date)]
ipo[(dif >= -1 & dif <= 7) & (Permno == - 999) & !is.na(dif), Permno := Permno_ncusip] 

ipo[, dif := n(First_CRSP_date_ncusip6 - Issue_date)]
ipo[ (dif >= -1 & dif <= 7) & (Permno == - 999) & !is.na(dif), Permno := Permno_ncusip6]

<<<<<<< HEAD
### Dropping non-match companies
=======
### Dropping non-match companies and duplicated
### (Duplicates usually happen when Units and Common Stocks are offered at the same time)
>>>>>>> origin/master
### 11,103 obs ---> 8,995 obs
ipo <- ipo[!(Permno == -999)]
ipo <- ipo[!duplicated(ipo$Permno)]

### matching CRSP infor
m <- match(ipo$Permno, crsp$PERMNO)
ipo[, `:=` (First_CRSP_date = ymd(crsp$date[m]), Close_price1 = abs(crsp$PRC[m]), Close_price2 = abs(crsp$PRC[m + 1]))]

### removing extra variables
ipo[,`:=`(First_CRSP_date_ncusip = NULL,First_CRSP_date_ncusip6 = NULL, Permno_ncusip = NULL, Permno_ncusip6 = NULL, dif = NULL)]
ipo[, `:=`(REIT = NULL, Unit = NULL, Depositary = NULL, CEF = NULL, CUSIP = NULL, CUSIP9 = NULL)]
ipo[, `:=`(IPO_Flag = NULL, Original_IPO_Flag = NULL)]


### Dropping wrong share clases and shares traded on other exchanges
### Loading CRSP Stock Header Information file
crsp.info <- fread(crsp.info.datafile, select = c("PERMNO", "HSHRCD", "HEXCD"))
m <- match(ipo$Permno, crsp.info$PERMNO)
ipo[, `:=` (exch = crsp.info$HEXCD[m], shrcd = crsp.info$HSHRCD[m])]

### 8,821 obs ---> 8,770
ipo <- ipo[exch %in% 1:3 & shrcd %in% 10:19]
write.csv(ipo, ipo.clean.datafile, row.names = F)
