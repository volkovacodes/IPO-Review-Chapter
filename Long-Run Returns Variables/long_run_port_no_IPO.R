require(data.table)
dir <- "./IPO review chapter/Chapter write up/SDC-PULL-IPO/Long-Run Returns Variables/"
### get CRSP info
get_crsp <- function(year)
{
  source("./R codes/wrds connect.R")
  sql_crsp <- paste0( "select date, permno, ret, shrout, prc, cusip from CRSP.DSF where date between '01jan", year, 
                      "'d and '31dec", year,"'d")
  
  res <- dbSendQuery(wrds, sql_crsp)
  start <- Sys.time()
  data <- fetch(res, n = -1)
  end <- Sys.time()
  
  data <- as.data.table(data)
  data[, size := abs(PRC)*SHROUT]
  print(end - start)
  return(data)
}

### get compustat data for book-to-market
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
  sql <- "select gvkey, lpermno from CRSPA.CCMXPF_LINKTABLE"
  res <- dbSendQuery(wrds, sql)
  link <- fetch(res, n = -1)
  link <- as.data.table(link)
  link <- link[!is.na(link$lpermno)]
  return(link)
}
### calculating thresholds for portfolios:
get_port_rank <- function(crsp)
{
  crsp[, month := as.numeric(substr(DATE, 6,7))]
  crsp_june <- crsp[month == 6]
  crsp_june <- crsp_june[!is.na(size) & !is.na(BE) & (BE) > 0]
  crsp_june <- crsp_june[!duplicated(PERMNO)]
  crsp_june[, BM := BE/size]
  
  crsp_june <- setDT(crsp_june)[, size_port := cut(size, quantile(size, probs=0:5/5), 
                                                   include.lowest=TRUE, labels=FALSE)]
  crsp_june <- setDT(crsp_june)[, BM5 := cut(BM, quantile(BM, probs=0:5/5), 
                                             include.lowest=TRUE, labels=FALSE)]
  crsp_june[, size_BM_port := (size_port - 1)*5 + BM5]
  return(crsp_june)
}

for(year in 1970:2015)
{
  ### getting data
  print(Sys.time())
  crsp <- get_crsp(year)
  print(Sys.time())
  comp <- get_comp(year)
  print(Sys.time())
  links <- get_links()
  
  ### explude IPOs
  require(bit64)
  ipo <- fread("./IPO review chapter/Chapter write up/SDC-PULL-IPO/ipo_seo_permno.csv")
  exclude_permnos <- ipo$Permno[year - ipo$Year <= 5]
  exclude_permnos <- exclude_permnos[!is.na(exclude_permnos)]
  crsp <- crsp[!PERMNO %in% exclude_permnos]
  
  ### matching gvkey to CRSP
  crsp$GVKEY <- links$gvkey[match(crsp$PERMNO, links$lpermno)]
  crsp <- crsp[!is.na(GVKEY)]
  
  ### adding book information to CRSP
  crsp$BE <- comp$be[match(crsp$GVKEY, comp$gvkey)]
  
  ### getting the firle with june quantiles
  crsp_ranks <- get_port_rank(crsp)
  crsp$size_port <- crsp_ranks$size_port[match(crsp$PERMNO, crsp_ranks$PERMNO)]
  crsp$size_BM_port <- crsp_ranks$size_BM_port[match(crsp$PERMNO, crsp_ranks$PERMNO)]
  crsp$wsize <- crsp_ranks$size[match(crsp$PERMNO, crsp_ranks$PERMNO)]
  
  require(stats)
  crsp <- crsp[, RET := as.numeric(as.character(RET))]
  crsp <- crsp[!is.na(RET)]
  crsp[, size_ret := weighted.mean(RET,wsize), by = c("DATE", "size_port")]
  crsp[, size_BM_ret := weighted.mean(RET,wsize), by = c("DATE", "size_BM_port")]
  
  head(crsp)
  crsp[, size_date := paste(size_port, DATE)]
  size_ret <- crsp[!duplicated(size_date) & !is.na(size_port), size_ret, by = size_date]
  
  crsp[, size_BM_date := paste(size_BM_port, DATE)]
  size_BM_ret <- crsp[!duplicated(size_BM_date) & !is.na(size_BM_port), size_BM_ret, by = size_BM_date]
  
  write.csv(size_ret, paste0(dir, "Size_Ret_EX_IPO_SEO_", year, ".csv"), row.names = F)
  write.csv(size_BM_ret, paste0(dir, "Size_BM_Ret_EX_IPO_SEO_", year, ".csv"), row.names = F)
  
  quantile_size <- crsp_ranks[, list(min_size = min(size)), by = size_port]
  quantile_size$year <- year
  quantile_BM <- crsp_ranks[, list(min_BM = min(BM)), by = BM5]
  quantile_BM$year <- year
  
  write.csv(quantile_size, paste0(dir,"Size_quant_EX_IPO_SEO_", year, ".csv"), row.names = F)
  write.csv(quantile_BM, paste0(dir, "BM_quant_EX_IPO_SEO_", year, ".csv"), row.names = F)
}

files <- list.files(dir)
files <- paste0(dir, files)

files_size_ret <- files[grepl("Size_Ret_EX_IPO_SEO_", files)]
df <- NULL
for(fl in files_size_ret) df <- rbind(df, fread(fl))

write.csv(df, paste0(dir, "Size_Ret_EX_IPO_SEO.csv"), row.names = F)
file.remove(files_size_ret)

files_size_BM_ret <- files[grep("Size_BM_Ret_EX_IPO_SEO_", files)]
df <- NULL
for(fl in files_size_BM_ret) df <- rbind(df, fread(fl))

write.csv(df, paste0(dir,"Size_BM_Ret_EX_IPO_SEO_.csv"), row.names = F)
file.remove(files_size_BM_ret)

qt_size <- files[grep("Size_quant_EX_IPO_SEO_", files)]
df <- NULL
for(fl in qt_size) df <- rbind(df, fread(fl))

write.csv(df, paste0(dir,"Size_Quant_EX_IPO_SEO.csv"), row.names = F)
file.remove(qt_size)

qt_BM <- files[grep("BM_quant_EX_IPO_SEO_", files)]
df <- NULL
for(fl in qt_BM) df <- rbind(df, fread(fl))

write.csv(df, paste0("BM_Quant_EX_IPO_SEO.csv"), row.names = F)
file.remove(qt_BM)
