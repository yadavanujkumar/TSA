sales<-c(100,120,90,150,110,130,100,160)
sales

#convert ts
ts_sales <- ts(sales, start = c(2020,1), frequency = 4)
ts_sales

#step 2 calculate link relatives

#lr = (current/previous) *100
LR <- (sales[-1] / sales[-length(sales)]) * 100
LR
quarters <- cycle(ts_sales)[-1]
sales[-1]
sales[-length(sales)]
quarters

#show link relative
link_relativs <- data.frame(Quarter = quarters,LR=round(LR,2))
link_relativs

#step 3: average link relative for each quarte
avg_lr<-tapply(LR, quarters, mean, na.rm=TRUE)
print("average link relative by quarters")
print(round(avg_lr))
#step 4 convert to chain relative
chain_rel <-numeric((length(avg_lr)))
chain_rel[1] <-100
for(i in 2:length(avg_lr)){
  chain_rel[i]<-chain_rel[i-1]*(avg_lr[i]/100)
}
print("chain relative")
print(round(chain_rel,2))

#step 5:L normalise to get seasconal indices

mean_chain<-mean(chain_rel)
mean_chain
seasonal_index <-chain_rel/mean_chain*100
seasonal_index
