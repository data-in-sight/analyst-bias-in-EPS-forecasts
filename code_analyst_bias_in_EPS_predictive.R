###########################
# Step 7. Please Run the following code in r 
# As model was developed in R
###########################
# filename : only_spy.r #this only takes into condideration SPY companies
# Many other methods were tried to link the databases: They gave more or less similar results
# Filename: using_linkmethod_wrds.r
# Filename: using_class_method.r


#Filename: only_spy.r
#this only takes into condideration SPY companies
rm(list=ls())
library(readr)
#import dataset
sue <- read_csv("sue.csv")

options(warn = -1)

library(RPostgres)
library(tidyverse)
library(datasets)
library(psych)
library(ExPanDaR)
library(data.table)
library(sqldf)

#create variables to be usd in the model
crsp_comp_ini2 <- sue%>%
  filter(prcc_f > 5,at>25.0)%>%
  mutate(
    pose = ifelse(actual1>0,actual1,0),
    nege_ind = ifelse(actual1<0,1,0),
    at_csho = at/csho,
    div = dvc/csho,
    dd_ind = ifelse(div==0.0,1,0),
    btm = ceq/(csho*prcc_f),
    year = year(datadate),
    mean = mean/ajex)%>%
  rename(act=act_x)

# define the accrual and the growth variable
crsp_comp_final <- crsp_comp_ini2%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(acc = ((act - lag(act)) - (che - lag(che))) - ((lct - lag(lct)) - (dlc - lag(dlc)) - (txp - lag(txp))) -dp,
         ag = 100*((at/lag(at)) - 1) )%>%
  ungroup()%>%
  mutate(accneg = ifelse(acc<0,acc,0),
         accpos = ifelse(acc>0,acc,0))

# create lag of each variable to be used in the model
crsp_comp_final<- crsp_comp_final%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(pose_lag = lag(pose),
         nege_ind_lag = lag(nege_ind),
         div_lag = lag(div),
         dd_ind_lag = lag(dd_ind),
         btm_lag = lag(btm),
         accpos_lag = lag(accpos),
         accneg_lag = lag(accneg),
         ag_lag = lag(ag),
         price_lag = lag(prcc_f))

# write_csv(crsp_comp_final,"eda_spy_v2.csv")

#filter out companies with  absolute actual eps greater than 100
temp <- crsp_comp_final%>%
  select(c(gvkey,year,conm,numest,actual1,actual2, at_csho,pose_lag,nege_ind_lag,div_lag,dd_ind_lag,
           btm_lag,accpos_lag,ag_lag,accneg_lag,price_lag,mean))%>%
  filter(abs(actual1)<=100)

# remove na variables
temp <- na.omit(temp)

temp2 <- do.call(data.frame,lapply(temp, function(x) replace(x, is.infinite(x),NA)))
temp2 <- na.omit(temp2)

#linear regression model
earn_lm <- lm(actual1 ~ pose_lag + nege_ind_lag + 
                accneg_lag + accpos_lag + ag_lag + 
                dd_ind_lag + div_lag + btm_lag + price_lag, data=temp2)
summary(earn_lm)
#earn_lm_lag <- lm(actual1 ~ actual1_lag, data=temp2)

m1m1 <- lm(cbind(actual1,actual2)~ pose_lag + nege_ind_lag + 
             accneg_lag + accpos_lag + ag_lag + 
             dd_ind_lag + div_lag + btm_lag + price_lag, data=temp2)

summary(m1m1)
head(resid(m1m1))
coef(m1m1)
sigma(m1m1)

vcov(m1m1)

library(car)
Anova(m1m1)

m1m2 <- update(m1m1,.~. -dd_ind_lag -accpos_lag)
anova(m1m1,m1m2)

lh.out <- linearHypothesis(m1m1,hypothesis.matrix = c("dd_ind_lag=0","accpos_lag=0"))
lh.out

summary(m1m2)

library(robustbase)
#robust regression
earn_lm2 <- lmrob(actual1 ~ pose_lag + nege_ind_lag + 
                    accneg_lag + accpos_lag + ag_lag + 
                    dd_ind_lag + div_lag + btm_lag + price_lag, data=temp2)
summary(earn_lm2,setting="KS2011")


require(MASS)

# MM Estimation method
earn_rlm <- rlm(actual1 ~ +pose_lag + nege_ind_lag + 
                  accneg_lag + accpos_lag + ag_lag + 
                  dd_ind_lag + div_lag + btm_lag + price_lag, data=temp2,psi=psi.bisquare,method = "MM")


summary(earn_rlm)

cbind(temp2$actual1,temp2$mean,earn_rlm %>% predict(temp2))
detach(package:MASS,unload=TRUE)


mean(abs(earn_lm$fitted.values - temp2$actual1))
mean(abs(temp2$mean - temp2$actual1))
sqrt(mean((earn_lm$fitted.values - temp2$actual1)^2))
sqrt(mean((temp2$mean - temp2$actual1)^2))

finish <- temp2
finish$predicted <- predict(earn_lm,temp2)
finish$predictedMM <- predict(earn_rlm,temp2)
finish$predicted2 <- predict(earn_lm2,temp2)

finish <-finish%>%
  mutate(CO = predicted - mean,
         AFE = actual1 -mean)

cor(finish$CO,finish$actual1)
cor(finish$AFE,finish$actual1)
cor(finish$CO,finish$AFE)

to_graph<-finish%>%group_by(year)%>%
  summarise(
    median_analyst_forecast = median(mean),
    median_actual_forecast = median(actual1),
    median_predicted_forecast = median(predicted),
    median_predictedMM_forecast = median(predictedMM),
    median_predicted2_forecast = median(predicted2),
    number_of_firms = n())%>%ungroup()%>%filter(year>=1983,year<=2020)

#write.csv(to_graph,"mm_estimation.csv")

library(ggplot2)

ggplot(to_graph) + 
  geom_bar(aes(x=year,y=number_of_firms),stat="identity",fill="grey",colour="#006000") +
  geom_line(aes(x=year,y=100*median_analyst_forecast), stat="identity",color="red",size=1,
            linetype="dashed") +
  geom_line(aes(x=year,y=100*median_predicted_forecast), stat="identity",color="black",size=1) +
  geom_line(aes(x=year,y=100*median_predicted2_forecast), stat="identity",color="brown",size=1) +
  geom_line(aes(x=year,y=100*median_predictedMM_forecast), stat="identity",color="blue",size=1) +
  geom_line(aes(x=year,y=100*median_actual_forecast), stat="identity",color="black",size=1,
            linetype = "dashed") +
  labs(title= "Median Forecasts",
       x="Year",y="Number of Firms")+
  scale_y_continuous(breaks = seq(0,1000,100),sec.axis=sec_axis(~.*0.01,name="Median  EPS",
                                                                breaks =  seq(0,6,0.5))) 



final_pred <- data.frame(year = temp2$year,
                         name = temp2$conm,
                         num_analyst = temp2$numest,
                         actual = temp2$actual1,
                         analyst_pred = temp2$mean,
                         mm_model_pred = earn_rlm %>% predict(temp2),
                         so_model_pred = earn_lm %>%predict(temp2),
                         so_model2_pred = earn_lm2 %>%predict(temp2),
                         at_csho = temp2$at_csho
)



fnn <- final_pred%>%
  mutate(AFE = (actual - analyst_pred)/at_csho ,
         CO1 = (so_model_pred - analyst_pred)/at_csho,
         CO2 = (mm_model_pred - analyst_pred)/at_csho,
         CO3 = (so_model2_pred - analyst_pred)/at_csho,
         Analyst_Predictions = ifelse(AFE > 0 ,"Pessimistic",ifelse(AFE==0,"HIT","Optimistic")),
         Signal_So_model=ifelse((AFE > 0 & CO1 > 0)|(AFE<0 & CO1 <0),"Signal","False Signal"),
         Signal_So2_model=ifelse((AFE > 0 & CO3 > 0)|(AFE<0 & CO3 <0),"Signal","False Signal"),
         Signal_mm_model=ifelse((AFE > 0 & CO2 > 0)|(AFE<0 & CO2 <0),"Signal","False Signal"))


## final graph that compares the median analyst and the prediction performance of the signal
merge(
  fnn%>%group_by(year,Signal_mm_model)%>%
    summarise(Count=n())%>%ungroup()%>%group_by(year)%>%
    mutate(Percentage_Correct_mm_model= 100*Count/sum(Count))%>%
    ungroup()%>%filter(Signal_mm_model=="Signal",year>1982)%>%
    select(year,Percentage_Correct_mm_model)%>%
    rename(`MM Model` = Percentage_Correct_mm_model),
  
  fnn%>%group_by(year,Signal_So_model)%>%
    summarise(Count=n())%>%ungroup()%>%group_by(year)%>%
    mutate(Percentage_Correct_So_model= 100*Count/sum(Count))%>%
    ungroup()%>%filter(Signal_So_model=="Signal",year>1982)%>%
    select(year,Percentage_Correct_So_model)%>%
    rename(`So Model` = Percentage_Correct_So_model),
  by="year"
)%>%
  gather(key = "Model", value = "Correct Signal Percentage",-year)%>%
  ggplot(aes(x=year,y = `Correct Signal Percentage`))+
  geom_line(aes(color = Model,linetype = Model))+
  geom_line(aes(y=50))+
  scale_y_continuous(breaks = seq(0,100,25),limits = c(0,100)) +
  scale_x_continuous(breaks = seq(1985,2020,5),limits = c(1983,2020)) +
  ylab("Correct Signal Percentage")+
  xlab("Year")+
  ggtitle("Signal detection using Model Predictions") +
  scale_color_manual(values = c("darkred", "blue"))+
  theme_minimal()

ggplot(aes(x=year))+
  geom_line(aes(y = Percentage_Correct_mm_model,color = "black")) +
  geom_line(aes(y = Percentage_Correct_So_model,color = "pink")) +
  scale_y_continuous(breaks = seq(0,100,25),limits = c(0,100)) +
  scale_x_continuous(breaks = seq(1985,2020,5),limits = c(1983,2020)) +
  ylab("Correct Signal Percentage")+
  xlab("Year")+
  ggtitle("Signal detection using Model Predictions") +
  theme_economist()

ggplot(fnn%>%group_by(year,Analyst_Predictions)%>%
         summarize(Count= n())%>%ungroup()%>%filter(year>1982),
       aes(fill = Analyst_Predictions,y=Count,x=year))+
  geom_bar(position="fill",stat="identity")+
  labs(title = "Analyst Performance in predicting EPS")+
  ylab("Ratio")


# Filename: using_linkmethod_wrds.r
# The following R code fetches data from Compustat and CRSP at WRDS
rm(list=ls())
options(warn = -1)

#import libraries that are needed
library(RPostgres)
library(tidyverse)
library(datasets)
library(psych)
library(ExPanDaR)
library(data.table)
library(sqldf)

#initialize WRDS data server connection
wrds<-dbConnect(Postgres(),  
                host='wrds-pgdata.wharton.upenn.edu',
                port=9737,
                user='$$$',	            # replace $$$$ with your WRDS user name			
                password='###',   # reokace $$$$ with your WRDS password
                sslmode='require',
                dbname='wrds')


#Fetch COMPUSTAT data and merge with he linktable above
res <- dbSendQuery(wrds, "
      select gvkey, datadate, cusip,conm, fyear, wcap, at,
             lt, ebit, re, mkvalt, sale, ni, act, lct, csho, che,
             dlc, txp, dp, ceq, prcc_f, ib, spi, dvc
      
      from compa.funda

      where datadate between '1962-01-01' and '2020-12-31' and consol='C'
         and popsrc='D' and indfmt='INDL' and datafmt='STD';")

comp <- dbFetch(res, n=-1)

dbClearResult(res)

crsp_comp <- comp
# As there is duplicate data for firm year, keep rows with the least NA values
crsp_comp$na_count <- apply(is.na(crsp_comp), 1, sum)

crsp_comp <- crsp_comp[order(crsp_comp$gvkey,crsp_comp$datadate, crsp_comp$na_count),]

crsp_comp <- crsp_comp %>% distinct(gvkey, datadate, .keep_all = TRUE)  #There can be more than one line of data for a firm-year, leave only the one with least NAs 

res <- dbSendQuery(wrds, "
      select * from Crsp.Ccmxpf_Linktable;")

linktable <- dbFetch(res, n=-1)

dbClearResult(res)

command0 <- "select a.*, b.lpermno as permno, b.lpermco as permco
  from crsp_comp a left join linktable b
  on a.gvkey=b.gvkey and (b.linkdt<= a.datadate and a.datadate<=b.linkenddt
     or (b.linkdt<=a.datadate and b.linkenddt is null ))
    and b.usedflag=1 and b.linkprim in ('P','C')
  group by a.gvkey, datadate, permno
  having fyear=min(fyear)"

Crsp_IDs <- sqldf(command0, stringsAsFactors = FALSE)

#If firm has duplicate gvkey-datadate observations due the fiscal year change 
#keep the record with the latest fiscal quarter for a given gvkey-datadate pair

res <- dbSendQuery(wrds, "
      select distinct gvkey, ibtic from Compa.Security
where ibtic is not null and iid='01';")

security <- dbFetch(res, n=-1)

dbClearResult(res)

command1 <- "select a.*, b.ibtic
  from Crsp_IDs a left join 
  security b
  on a.gvkey=b.gvkey
  group by a.gvkey, a.datadate
  having fyear=max(fyear)
  order by a.gvkey, a.datadate"

Comp_Earnings <- sqldf(command1, stringsAsFactors = FALSE)

#Sanity check: are there duplicate gvkey-datadate observations
#should be zero duplicates   
nrow(Comp_Earnings) == length(unique(paste(Comp_Earnings$gvkey,Comp_Earnings$datadate)))

#sort data
Comp_Earnings <- Comp_Earnings[order(Comp_Earnings$gvkey,Comp_Earnings$datadate),]

command2<- "select * from Comp_Earnings
where permno is not null and ibtic is null"

Noticker <- sqldf(command2,stringsAsFactors = FALSE)

Noticker <- Noticker[!names(Noticker) %in% c("ibtic")]

library(readr)
iclink1 <- read_csv("iclink1.csv", col_types = cols(sdate = col_date(format = "%Y/%m/%d"), 
                                                    edate = col_date(format = "%Y/%m/%d")))

iclink1 <- as.tibble(iclink1)
ibislink <- iclink1%>%filter(SCORE %in% c(1,0))%>%arrange(PERMNO,TICKER,SCORE)

# select first permno
ibeslink <- ibislink%>%distinct(PERMNO, .keep_all =TRUE)

command3<- "select a.*, b.ticker as ibtic
from Noticker a left join ibeslink b
on a.permno=b.permno
order by gvkey, datadate"

Noticker1 <- sqldf(command3,stringsAsFactors = FALSE)

#append the additional GVKEY-IBES Ticker links
command4 <- "select a.*,
case when a.ibtic is null then b.ibtic
else a.ibtic end as ibtic_new
from Comp_Earnings as a left join
Noticker1 as b on a.permno = b.permno and a.datadate = b.datadate"

Comp_Earnings_final <- sqldf(command4, stringsAsFactors = FALSE)    

#check if new ibtic were attached
sum(is.na(Comp_Earnings_final$ibtic_new))
sum(is.na(Comp_Earnings_final$ibtic))   

#drop and rename column
Comp_Earnings_final <- Comp_Earnings_final[!names(Comp_Earnings_final) %in% c("ibtic")]
Comp_Earnings_final <- Comp_Earnings_final%>%rename(ibtic = ibtic_new)    

#Bring in closest available analyst consensus estimate
#for future EPS before earnings announcement          

Comp_Earnings_final <- Comp_Earnings_final%>%
  mutate(port_date = datadate +150)


# get ibes data
res <- dbSendQuery(wrds, "
      select ticker,statpers,numest,medest,meanest,actual, anndats_act, fpedats, fpi from ibes.statsum_epsus 
                   where fiscalp = 'ANN' and fpi = '1';")

ibes_stat <- dbFetch(res, n=-1)

dbClearResult(res)

ibes_stat <- ibes_stat%>%filter(statpers < anndats_act)



command5 <- "select a.*, b.statpers, b.numest, b.medest, b.meanest,b.actual, b.fpedats, b.anndats_act
from Comp_Earnings_final a left join
ibes_stat as b 
on a.ibtic = b.ticker and a.datadate < b.statpers and b.statpers <= a.port_date
group by gvkey,datadate
having statpers = max(statpers)
order by gvkey, datadate"

final_data <- sqldf(command5, stringsAsFactors = FALSE) 


crsp_comp_ini2 <- final_data%>%
  mutate(year = year(datadate))%>%
  filter(prcc_f > 5, year >=1980,year<=2009,at>25.0)%>%
  mutate(eps = (ib - 0.65*ifelse(is.na(spi),0,spi)) /csho ,
         pose = ifelse(eps>0,eps,0),
         nege_ind = ifelse(eps<0,1,0),
         at_csho = at/csho,
         div = dvc/csho,
         dd_ind = ifelse(div==0.0,1,0),
         btm = ceq/(csho*prcc_f))

crsp_comp_ini2_full <- final_data%>%
  mutate(year = year(datadate))%>%
  filter(prcc_f > 5, year >=1990,year<=2020,at>25.0)%>%
  mutate(eps = (ib - 0.65*ifelse(is.na(spi),0,spi)) /csho ,
         pose = ifelse(eps>0,eps,0),
         nege_ind = ifelse(eps<0,1,0),
         at_csho = at/csho,
         div = dvc/csho,
         dd_ind = ifelse(div==0.0,1,0),
         btm = ceq/(csho*prcc_f))

crsp_comp_final <- crsp_comp_ini2%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(acc = ((act - lag(act)) - (che - lag(che))) - ((lct - lag(lct)) - (dlc - lag(dlc)) - (txp - lag(txp))) -dp,
         ag = 100*((at/lag(at)) - 1) )%>%
  ungroup()%>%
  mutate(accneg = ifelse(acc<0,acc,0),
         accpos = ifelse(acc>0,acc,0))


crsp_comp_final_full <- crsp_comp_ini2_full%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(acc = ((act - lag(act)) - (che - lag(che))) - ((lct - lag(lct)) - (dlc - lag(dlc)) - (txp - lag(txp))) -dp,
         ag = 100*((at/lag(at)) - 1) )%>%
  ungroup()%>%
  mutate(accneg = ifelse(acc<0,acc,0),
         accpos = ifelse(acc>0,acc,0))


crsp_comp_final<- crsp_comp_final%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(pose_lag = lag(pose),
         nege_ind_lag = lag(nege_ind),
         div_lag = lag(div),
         dd_ind_lag = lag(dd_ind),
         btm_lag = lag(btm),
         accpos_lag = lag(accpos),
         accneg_lag = lag(accneg),
         ag_lag = lag(ag),
         price_lag = lag(prcc_f) )

crsp_comp_final_full<- crsp_comp_final_full%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(pose_lag = lag(pose),
         nege_ind_lag = lag(nege_ind),
         div_lag = lag(div),
         dd_ind_lag = lag(dd_ind),
         btm_lag = lag(btm),
         accpos_lag = lag(accpos),
         accneg_lag = lag(accneg),
         ag_lag = lag(ag),
         price_lag = lag(prcc_f) )


temp <- crsp_comp_final%>%
  select(c(gvkey,year,eps, pose_lag,nege_ind_lag,div_lag,dd_ind_lag,
           btm_lag,accpos_lag,ag_lag,accneg_lag,price_lag,meanest))

temp <- na.omit(temp)

temp2 <- do.call(data.frame,lapply(temp, function(x) replace(x, is.infinite(x),NA)))
temp2 <- na.omit(temp2)


earn_lm <- lm(eps ~ pose_lag + nege_ind_lag + 
                accneg_lag + accpos_lag + ag_lag + 
                dd_ind_lag + div_lag + btm_lag + price_lag, data=temp2)

summary(earn_lm)

temp_full <- crsp_comp_final_full%>%
  select(c(gvkey,year,eps, pose_lag,nege_ind_lag,div_lag,dd_ind_lag,
           btm_lag,accpos_lag,ag_lag,accneg_lag,price_lag,meanest))

temp_full <- na.omit(temp_full)

temp2_full <- do.call(data.frame,lapply(temp_full, function(x) replace(x, is.infinite(x),NA)))
temp2_full <- na.omit(temp2_full)


earn_lm_full <- lm(eps ~ pose_lag + nege_ind_lag + 
                     accneg_lag + accpos_lag + ag_lag + 
                     dd_ind_lag + div_lag + btm_lag + price_lag, data=temp2_full)

summary(earn_lm_full)


library(texreg)

screenreg(list(earn_lm,earn_lm_full),digits=4)
save(earn_lm, file = 'using_linkmethod_wrds_method.Rdata')
save(earn_lm_full, file = 'using_linkmethod_wrds_method_all.Rdata')

finish <- temp2_full%>%rename(actual1=eps,mean=meanest)
finish$predicted <- predict(earn_lm,temp2_full)

finish <-finish%>%
  mutate(CO = predicted - mean,
         AFE = actual1 -mean)

cor(finish$CO,finish$actual1)
cor(finish$AFE,finish$actual1)
cor(finish$CO,finish$AFE)

to_graph<-finish%>%group_by(year)%>%
  summarise(
    median_analyst_forecast = median(mean),
    median_actual_forecast = median(actual1),
    median_predicted_forecast = median(predicted),
    number_of_firms = n())%>%ungroup()%>%filter(year>=1983,year<=2020)

library(ggplot2)
ggplot(to_graph) + 
  geom_bar(aes(x=year,y=number_of_firms),stat="identity",fill="grey",colour="#006000") +
  geom_line(aes(x=year,y=1000*median_analyst_forecast), stat="identity",color="red",size=1,
            linetype="dashed") +
  geom_line(aes(x=year,y=1000*median_predicted_forecast), stat="identity",color="black",size=1) +
  geom_line(aes(x=year,y=1000*median_actual_forecast), stat="identity",color="black",size=1,
            linetype = "dashed") +
  labs(title= "Median Forecasts",
       x="Year",y="Number of Firms")+
  scale_y_continuous(breaks = seq(0,3000,200),sec.axis=sec_axis(~.*0.001,name="Median  EPS",
                                                                breaks =  seq(0,6,0.5))) 

# Filename: using_class_method.r
# The following R code fetches data from Compustat and CRSP at WRDS
rm(list=ls())
options(warn = -1)

library(RPostgres)
library(tidyverse)
library(datasets)
library(psych)
library(ExPanDaR)
library(data.table)


#initialize WRDS data server connection
wrds<-dbConnect(Postgres(),  
                host='wrds-pgdata.wharton.upenn.edu',
                port=9737,
                user='$$$',	            # replace $$$$ with your WRDS user name			
                password='###',            # reokace $$$$ with your WRDS password
                sslmode='require',
                dbname='wrds')

# Get IBES Data 
res <- dbSendQuery(wrds, "select * 
                   from ibes.statsumu_epsus
                   where fpi in ('1','0')") #fpi 1 is annual eps Fpi 0 is for long term growth
ibes <- dbFetch(res, n = -1)
dbClearResult(res)


#unique(ibes$fpi)
# Crete year and month for analyst estimates
ibes<- data.table(ibes)
ibes[,year:= year(statpers)]
ibes[,month:= month(statpers)]

ibes <- as.tibble(ibes)

# Analyst forecasts before the portfolio formation date
ibes1 <- ibes%>%filter(month <=5)%>%
  group_by(cusip,fpi,year)%>%
  mutate(max_month = max(month))%>%
  ungroup()%>%filter(month == max_month, year <= 2020)%>%arrange(cusip,year)%>%
  filter(max_month >= 3)

unique(ibes1$fpi)
#unique(ibes1$month)

#### Get gvkey and link it to IBES data as gvkey will serve as an identifier
res <- dbSendQuery(wrds, "select ticker, cusip, cname 
                   from ibes.idsum
                   ")
ibeslink <- dbFetch(res, n = -1)
dbClearResult(res)

#Drop duplicates
ibeslink <- ibeslink[!duplicated(ibeslink),]

#get crsp data
res <- dbSendQuery(wrds, "select permno, ncusip
                   from crsp.stocknames
                   ")
crsplink <- dbFetch(res, n = -1)
dbClearResult(res)

#Drop duplicates
crsplink <- crsplink[!duplicated(crsplink),]

## merge ibes and crsp
link1 <- merge(ibeslink, crsplink, by.x ="cusip", by.y = 'ncusip' )

# get Compustat- CRSP link
res <- dbSendQuery(wrds, "select gvkey, lpermno,lpermco,linktype,linkprim
                   from crsp.ccmxpf_lnkhist
                   ")
link2 <- dbFetch(res, n = -1)
dbClearResult(res)

link2 <- link2[link2$linktype %in% c("LC","LU"),]
link2 <- link2[link2$linkprim %in% c("C","P"),]

drops <- c('linktype','linkprim')

link2<- link2[,!(names(link2) %in% drops)]

# Merge the two link tables
outlink <- merge(link2,link1, by.x="lpermno",by.y = "permno")
# Keep only relevant columns
outlink <- outlink[,1:4]
outlink <- outlink[!duplicated(outlink),]

#test1 <- ibes1[ibes1$ticker=="GFGC",]
#Merge the link file to get gvkey for IBES data
ibes_gvkey <- merge(ibes1,outlink,by="cusip")

##gvkey 012994 lpermno 10001
#test2<-crsp_comp_final_full[crsp_comp_final_full$gvkey=="012994",]


# read the crsp permno and compustat gvkey
crsp_daily_comp_link <- read.table(file="crsp_daily_comp_link.csv",sep=",",colClasses=c("integer","character"),header=TRUE)

#Fetch COMPUSTAT data and merge with he linktable above
res <- dbSendQuery(wrds, "
      select gvkey, datadate, cusip, fyear, wcap, at,
             lt, ebit, re, mkvalt, sale, ni, act, lct, csho, che,
             dlc, txp, dp, ceq, prcc_f, ib, spi, dvc
      
      from compa.funda

      where datadate between '1962-01-01' and '2020-12-31';")

comp <- dbFetch(res, n=-1)

dbClearResult(res)

#Merge the two files to get permno
crsp_comp<- merge(crsp_daily_comp_link, comp, by.x="gvkey", by.y="gvkey", sort = TRUE)

# As there is duplicate data for firm year, keep rows with the least NA values
crsp_comp$na_count <- apply(is.na(crsp_comp), 1, sum)

crsp_comp <- crsp_comp[order(crsp_comp$gvkey,crsp_comp$datadate, crsp_comp$na_count),]

crsp_comp <- crsp_comp %>% distinct(gvkey, datadate, .keep_all = TRUE)  #There can be more than one line of data for a firm-year, leave only the one with least NAs 


# Fetch monthly CRSP data to get returns if needed
res <- dbSendQuery(wrds, "
      select permno, date, prc, ret, shrout
      
      from crsp_a_stock.msf

      where date between '1961-01-01' and '2020-12-31';")

crsp_ms <- dbFetch(res, n=-1)
dbClearResult(res)

# Fetch monthly index data
res <- dbSendQuery(wrds, "
      select caldt, vwretd, vwretx, totval
      
      from crsp_a_indexes.msic

      where caldt between '1961-01-01' and '2020-12-31';")

crsp_msi <- dbFetch(res, n=-1)
dbClearResult(res)
names(crsp_msi)[names(crsp_msi) == "caldt"] <- "date"

# Fetch begin trading date to drop those begin trading outside 1962-2020 and construct age var.
res <- dbSendQuery(wrds, "
      select distinct permno, begexchdate
      
      from crsp_a_stock.mseexchdates;")

crsp_beg <- dbFetch(res, n=-1)
dbClearResult(res)

crsp_beg <- crsp_beg[order(crsp_beg$permno),]

crsp_comp <- merge(crsp_comp, crsp_beg, by.x="permno", by.y="permno", sort = TRUE)

crsp_comp<- crsp_comp[ which(crsp_comp$begexchdate<='2020-12-31'
                             & crsp_comp$begexchdate>='1962-01-01'), ] #drop those begin trading before 1962 and after 2020

crsp_comp$begyear<- format(crsp_comp$begexchdate, "%Y") #extract the year a firm begin trading to compute age of a firm

crsp_ms <- merge(crsp_ms, crsp_msi, by.x = "date", by.y = "date", sort = TRUE)


crsp_ms$year <- format(crsp_ms$date,"%Y")



sdreg <- function(x,y) {
  df <- data.frame(x=x, y=y)
  return( sd(summary(lm(y~x,data=df))$residuals) )
}
system.time({
  tmp <- data.table(crsp_ms)
  setorderv(tmp,c("permno","year"))
  tmp <- tmp[!is.na(tmp$ret),]                                     # remove NAs
  tmp1 <- tmp[, .N, by=c("permno","year")]                         # count number of monthly returns in each firm-year
  tmp1 <- tmp1[N==12,]                                             # keep only the firm-years that have 12 monthly returns
  tmp2 <- tmp[tmp1,,on=c("permno","year")]                         # extract monthly returns for firm-years with 12 monthly returns
  tmp3 <- tmp2[, .(sd=sdreg(vwretd,ret)), by=c("permno","year")]   # run regression of monthly return on market return by firm-year and keep the std dev of the residuals -- this is the sigma variable
  tmp4 <- tmp3[tmp, , on=c("permno","year")]                       # put the resulting sigma variable back into the full data set
})

#   user  system elapsed 
# 287.84    4.43  292.55 


crsp_sd <- tmp4[,c("permno","year","sd")]%>% distinct(permno, year, .keep_all = TRUE)
crsp_ms<-merge(crsp_ms, crsp_sd, by = c("permno","year"), sort = TRUE)
rm(tmp, tmp1, tmp2, tmp3, tmp4)

crsp_ms$ret[is.na(crsp_ms$ret)] <- crsp_ms$vwretd[is.na(crsp_ms$ret)] #replace missing firm monthly returns with the market return of that month

crsp_msi$year <- format(crsp_msi$date,"%Y")
crsp_msi$fyear <- as.numeric(crsp_msi$year)-1  #one year lag to merge with fyear in comp
crsp_msi$month<- format(crsp_msi$date,"%m")
crsp_msi$vwretd <- log(1+crsp_msi$vwretd) #convert to log monthly return
crsp_msi <- crsp_msi[order(crsp_msi$fyear,crsp_msi$month),]
crsp_msi$vwretda <- ave(crsp_msi$vwretd, crsp_msi$fyear, FUN=cumsum) #cumulate monthly return to annual return
crsp_msi$vwretda <- exp(crsp_msi$vwretda)-1
crsp_msi <- crsp_msi[ which(crsp_msi$month == 12),] #keep only end of year market return and cap.
crsp_msi <- crsp_msi[ c("fyear", "vwretda", "totval")] #keep only year, return and market cap.

crsp_comp <- merge(crsp_comp, crsp_msi, by.x="fyear", by.y="fyear", sort = TRUE)

crsp_ms$fyear <- as.numeric(crsp_ms$year)-1  #one year lag to merge with fyear in comp
crsp_ms$month<- format(crsp_ms$date,"%m")
crsp_ms <- crsp_ms[order(crsp_ms$permno, crsp_ms$fyear,crsp_ms$month),]
crsp_ms$reta <- ave(crsp_ms$ret, crsp_ms$permno, crsp_ms$fyear, FUN=cumsum) #cumulate monthly return of firm into annual return
crsp_ms$reta <- exp(crsp_ms$reta)-1
crsp_ms <- crsp_ms[ which(crsp_ms$month == 12),] #only end of year firm level market cap.
crsp_ms_formerge <- crsp_ms[ c("fyear","permno", "reta", "sd")]

crsp_comp <- merge(crsp_comp, crsp_ms_formerge, by=c("fyear", "permno"), sort = TRUE)


crsp_me  <- crsp_ms["permno"]
crsp_me$me <- abs(crsp_ms$prc)*crsp_ms$shrout #compute firm market value 
crsp_me$fyear <- format(crsp_ms$date,"%Y")
crsp_comp <- merge(crsp_comp, crsp_me, by=c("fyear", "permno"), sort = TRUE)

# create combinations of fundamental variables
#create Altman variables
crsp_comp$wc_ta <- crsp_comp$wcap/crsp_comp$at

crsp_comp$re_ta <- crsp_comp$re/crsp_comp$at

crsp_comp$ebit_ta <- crsp_comp$ebit/crsp_comp$at

#crsp_comp$me_tl <- crsp_comp$me/crsp_comp$lt/1000 #Solve .x and .y issue

crsp_comp$s_ta <- crsp_comp$sale/crsp_comp$at

# create Zmijewski's variables

crsp_comp$ni_ta <- crsp_comp$ni/crsp_comp$at

crsp_comp$tl_ta <- crsp_comp$lt/crsp_comp$at

crsp_comp$ca_cl <- crsp_comp$act/crsp_comp$lct

# create Shumway's variables

crsp_comp$ri_rm <- crsp_comp$reta - crsp_comp$vwretda

#crsp_comp$size <- log(crsp_comp$me/crsp_comp$totval) #same issue as above

crsp_comp$age <- log(as.numeric(format(crsp_comp$datadate,"%Y"))-as.numeric(crsp_comp$begyear)+1)


crsp_comp_ini <- as.tibble(crsp_comp)



merge_all <- merge(ibes_gvkey,crsp_comp_ini,by.x = c("gvkey","year"),by.y = c("gvkey","fyear"))


unique(merge_all$fpi)

crsp_comp_ini2 <- merge_all%>%
  filter(prcc_f > 5, year >=1980,year<=2009,at>25.0,fpi=="1")%>%
  mutate(eps = (ib - 0.65*ifelse(is.na(spi),0,spi)) /csho ,
         pose = ifelse(eps>0,eps,0),
         nege_ind = ifelse(eps<0,1,0),
         at_csho = at/csho,
         div = dvc/csho,
         dd_ind = ifelse(div==0.0,1,0),
         btm = ceq/(csho*prcc_f))

crsp_comp_ini2_full <- merge_all%>%
  filter(prcc_f > 5, year >=1990,year<=2020,at>25.0,fpi=="1")%>%
  mutate(eps = (ib - 0.65*ifelse(is.na(spi),0,spi)) /csho ,
         pose = ifelse(eps>0,eps,0),
         nege_ind = ifelse(eps<0,1,0),
         at_csho = at/csho,
         div = dvc/csho,
         dd_ind = ifelse(div==0.0,1,0),
         btm = ceq/(csho*prcc_f))

crsp_comp_final <- crsp_comp_ini2%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(acc = ((act - lag(act)) - (che - lag(che))) - ((lct - lag(lct)) - (dlc - lag(dlc)) - (txp - lag(txp))) -dp,
         ag = 100*((at/lag(at)) - 1) )%>%
  ungroup()%>%
  mutate(accneg = ifelse(acc<0,acc,0),
         accpos = ifelse(acc>0,acc,0))


crsp_comp_final_full <- crsp_comp_ini2_full%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(acc = ((act - lag(act)) - (che - lag(che))) - ((lct - lag(lct)) - (dlc - lag(dlc)) - (txp - lag(txp))) -dp,
         ag = 100*((at/lag(at)) - 1) )%>%
  ungroup()%>%
  mutate(accneg = ifelse(acc<0,acc,0),
         accpos = ifelse(acc>0,acc,0))


crsp_comp_final<- crsp_comp_final%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(pose_lag = lag(pose),
         nege_ind_lag = lag(nege_ind),
         div_lag = lag(div),
         dd_ind_lag = lag(dd_ind),
         btm_lag = lag(btm),
         accpos_lag = lag(accpos),
         accneg_lag = lag(accneg),
         ag_lag = lag(ag),
         price_lag = lag(prcc_f) )

crsp_comp_final_full<- crsp_comp_final_full%>%arrange(gvkey,year)%>%group_by(gvkey)%>%
  mutate(pose_lag = lag(pose),
         nege_ind_lag = lag(nege_ind),
         div_lag = lag(div),
         dd_ind_lag = lag(dd_ind),
         btm_lag = lag(btm),
         accpos_lag = lag(accpos),
         accneg_lag = lag(accneg),
         ag_lag = lag(ag),
         price_lag = lag(prcc_f) )


temp <- crsp_comp_final%>%
  select(c(gvkey,year,eps, pose_lag,nege_ind_lag,div_lag,dd_ind_lag,
           btm_lag,accpos_lag,ag_lag,accneg_lag,price_lag,meanest))

temp <- na.omit(temp)

temp2 <- do.call(data.frame,lapply(temp, function(x) replace(x, is.infinite(x),NA)))
temp2 <- na.omit(temp2)


earn_lm <- lm(eps ~ pose_lag + nege_ind_lag + 
                accneg_lag + accpos_lag + ag_lag + 
                dd_ind_lag + div_lag + btm_lag + price_lag, data=temp2)

summary(earn_lm)

temp_full <- crsp_comp_final_full%>%
  select(c(gvkey,year,eps, pose_lag,nege_ind_lag,div_lag,dd_ind_lag,
           btm_lag,accpos_lag,ag_lag,accneg_lag,price_lag,meanest))

temp_full <- na.omit(temp_full)

temp2_full <- do.call(data.frame,lapply(temp_full, function(x) replace(x, is.infinite(x),NA)))
temp2_full <- na.omit(temp2_full)


earn_lm_full <- lm(eps ~ pose_lag + nege_ind_lag + 
                     accneg_lag + accpos_lag + ag_lag + 
                     dd_ind_lag + div_lag + btm_lag + price_lag, data=temp2_full)

summary(earn_lm_full)
library(texreg)

screenreg(list(earn_lm,earn_lm_full),digits=4)
save(earn_lm, file = 'using_class_method.Rdata')
save(earn_lm_full, file = 'using_class_method_all.Rdata')



finish <- temp2_full%>%rename(actual1=eps,mean=meanest)
finish$predicted <- predict(earn_lm,temp2_full)

finish <-finish%>%
  mutate(CO = predicted - mean,
         AFE = actual1 -mean)

cor(finish$CO,finish$actual1)
cor(finish$AFE,finish$actual1)
cor(finish$CO,finish$AFE)

to_graph<-finish%>%group_by(year)%>%
  summarise(
    median_analyst_forecast = median(mean),
    median_actual_forecast = median(actual1),
    median_predicted_forecast = median(predicted),
    number_of_firms = n())%>%ungroup()%>%filter(year>=1983,year<=2020)

library(ggplot2)

ggplot(to_graph) + 
  geom_bar(aes(x=year,y=number_of_firms),stat="identity",fill="grey",colour="#006000") +
  geom_line(aes(x=year,y=1000*median_analyst_forecast), stat="identity",color="red",size=1,
            linetype="dashed") +
  geom_line(aes(x=year,y=1000*median_predicted_forecast), stat="identity",color="black",size=1) +
  geom_line(aes(x=year,y=1000*median_actual_forecast), stat="identity",color="black",size=1,
            linetype = "dashed") +
  labs(title= "Median Forecasts",
       x="Year",y="Number of Firms")+
  scale_y_continuous(breaks = seq(0,3000,200),sec.axis=sec_axis(~.*0.001,name="Median  EPS",
                                                                breaks =  seq(0,6,0.5))) +
  annotate(
    geom = "curve", x = 1993, y = 210, xend = 1995, yend = 1009, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 1990, y = 200, label = "Prediction Model", hjust = "left") +
  
  annotate(
    geom = "curve", x = 2000, y = 210, xend = 2005, yend = 1100, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 1998, y = 200, label = "Analyst Forecast", hjust = "left") +
  
  annotate(
    geom = "curve", x = 2010, y = 210, xend = 2012, yend = 1580, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 2008, y = 200, label = "Actual", hjust = "left")


  
