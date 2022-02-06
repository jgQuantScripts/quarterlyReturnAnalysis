require("quantmod");require("PerformanceAnalytics")
# ********************************************************************************************
#                             quarterly returns performance analysis
# ********************************************************************************************
# assign symbol
# idx = "^IXIC"
# idx = "BTC-USD"
# idx = "IWM"
idx = "^GSPC"
# get data from yahoo finance
IDX = getSymbols(idx, from="1920-01-01",auto.assign = FALSE)
# convert Adjusted Closed to quarterly returns
qRET = quarterlyReturn(Ad(IDX))
# ********************************************************************************************
# Function: table with quarterly returns and returns 1-quarter after & 1-year after
# ********************************************************************************************
getStats = function(qRET, threshold){
  # subsets quarterly returns below threshold (th): i.e. th is 0.10 -> returns quarters below negative th
  if(threshold < 0){
    tmp <- round(subset(qRET, qRET < threshold),4)  
  }else{
    tmp <- round(subset(qRET, qRET > threshold),4)  
  }
  # now get the return 1 quarter after 
  indx = index(tmp)
  oneQ = do.call(c, lapply(as.list(1:length(indx)), function(ii) which(indx[ii] == index(qRET))+1))
  oneQ[length(oneQ)] = ifelse(last(oneQ) > length(index(qRET)), length(index(qRET)), last(oneQ)) 
  oneQafter = index(qRET)[oneQ]
  oneQafter = round(qRET[oneQafter],4)
  # now get the return 1 year after 
  oneYafter = do.call(c, lapply(as.list(1:length(indx)), function(ii) which(indx[ii] == index(qRET))+4))
  oneYafter[length(oneYafter)] = ifelse(last(oneYafter) > length(index(qRET)), length(index(qRET)), last(oneYafter)) 
  oneYafter = paste0(index(qRET)[oneQ],"/",index(qRET)[oneYafter])
  oneYafter = do.call(rbind, lapply(as.list(oneYafter), function(x){
    round(xts(sum(qRET[x]), order.by = last(index(qRET[x]))),4)
  }))
  # format as a data.frame
  tmp = cbind(as.data.frame(index(tmp), row.names = NULL),as.data.frame(tmp, row.names = NULL))
  oneQafter = cbind(as.data.frame(index(oneQafter), row.names = NULL),
                    as.data.frame(oneQafter, row.names = NULL))
  oneYafter = cbind(as.data.frame(index(oneYafter), row.names = NULL),
                    as.data.frame(oneYafter, row.names = NULL))
  tmp = cbind(tmp,oneQafter,oneYafter)
  colnames(tmp) = c("indx","qRet","indx1Q","oneQret","indx1Y","oneYret")
  # return data.frame
  tmp
}
# ********************************************************************************************
# test function
# threshold (TH): negative to get drops below TH and positive to get rallies above TH
qStat = getStats(qRET = qRET, threshold = -0.10)

# positive 1-quarter after
cat("Win % after 1-quarter: ",round(length(qStat$oneQret[qStat$oneQret > 0])/nrow(qStat),4)*100,
    "% \nAverage Return       :  ",round(mean(qStat$oneQret),4)*100, "%")
# positive 1-year after
cat("\nWin % after 1-year   :  ",round(length(qStat$oneYret[qStat$oneYret > 0])/nrow(qStat),4)*100,
    "% \nAverage Return       :  ",round(mean(qStat$oneYret),4)*100, "%")

# convert results into XTS objects
oneQ = xts(qStat$oneQret, order.by = as.Date(qStat$indx1Q))
oneY = xts(qStat$oneYret, order.by = as.Date(qStat$indx1Y))
# merge quarterly results + fill NAs with 0s
comps = merge(oneQ, oneY,qRET)
comps[is.na(comps)] <-0
# plot results
charts.PerformanceSummary(comps, geometric = FALSE)


