#This script replicates tables 1-4 of Gottlieb (2016, AJPS)
#LaTeX documents created: Table1,Table2,Table3a,Table3b,Table4


##Set your working directory appropriately
setwd("C:/Users/Sebastian/Google Drive/Education/Replication/Gottlieb 2016")

##Download data (if you have not already) 

dl.file1<-"https://dataverse.harvard.edu/api/access/datafile/3004606"
dl.file2<-"https://dataverse.harvard.edu/api/access/datafile/2533316"
dl.file3<-"https://dataverse.harvard.edu/api/access/datafile/2533287"

if (!file.exists("./commune.dta")){ 
  download.file(url = dl.file1, destfile = "./commune.dta", method="curl")
    dldate1<-date()
    }

if (!file.exists("./restitutions.dta")){ 
    download.file(url = dl.file2, destfile = "./restitutions.dta", method="curl")
    dldate2<-date()
}

if (!file.exists("./survey.dta")){ 
    download.file(url = dl.file3, destfile = "./survey.dta", method="curl")
    dldate3<-date()
}

##Load dataset #1 (survey) and merge #2 (restitutions) with #3 (commune)

library(readstata13) #to read .dta file
survey.data<-read.dta13("./survey.dta", nonint.factors = TRUE, missing.type = T)

rest.data<-read.dta13("./restitutions.dta", nonint.factors = TRUE, missing.type = T)
rest.data$secondrestitution<-NA
rest.data$secondrestitution[368:381]<-1
library(dplyr)
rest.data<-arrange(rest.data, cid)
commune.data<-read.dta13("./commune.dta", nonint.factors = TRUE, missing.type = T)
merge.data<-merge(rest.data, commune.data, by = c("cid"), all.x = T)
merge.data$cid[merge.data$secondrestitution==1]<-1


##Prepare variables
survey.data$h0m1<-ifelse(survey.data$responsibleschools=="", 0,
                         ifelse(survey.data$responsibleschools=="1"|
                                    survey.data$responsibleschools=="3"|
                                    survey.data$responsibleschools=="4"|
                                    survey.data$responsibleschools=="5", 0, 1))
survey.data$h0m2<-ifelse(survey.data$responsibleclinics=="", 0,
                         ifelse(survey.data$responsibleclinics==1|
                                    survey.data$responsibleclinics==3|
                                    survey.data$responsibleclinics==4|
                                    survey.data$responsibleclinics==5, 0, 1))
survey.data$h0m3<-ifelse(survey.data$responsibletaxes=="", 0,
                         ifelse(survey.data$responsibletaxes==1|survey.data$responsibletaxes==3|
                                    survey.data$responsibletaxes==4|
                                    survey.data$responsibletaxes==5, 0, 1))
survey.data$h0m4<-ifelse(survey.data$responsiblewater=="", 0,
                         ifelse(survey.data$responsiblewater==1|
                                    survey.data$responsiblewater==3|
                                    survey.data$responsiblewater==4|
                                    survey.data$responsiblewater==5, 0, 1))
survey.data$h0m5<-ifelse(survey.data$responsibleconflict=="", 0,
                         ifelse(survey.data$responsibleconflict==1|
                                    survey.data$responsibleconflict==3|
                                    survey.data$responsibleconflict==4|
                                    survey.data$responsibleconflict==5, 0, 1))
survey.data$h0m6<-ifelse(survey.data$budget=="", 0,ifelse(survey.data$budget==1, 0, 1))
survey.data$h0m7<-ifelse(survey.data$budgetbig==2, 0, 1)
survey.data$h0m8<-survey.data$notsecret
survey.data$h0m8[survey.data$h0m8==88|survey.data$h0m8==""]<-NA
survey.data$h0m8<-as.numeric((survey.data$h0m8))*(-1)
survey.data$h0m9<-ifelse(survey.data$retrovoting==2, 0, 1)
survey.data$h0m10<-survey.data$nprojectsfuture
survey.data$h0m20<-ifelse(survey.data$paybirthcert==""|survey.data$paybirthcert==88, 0,
                          ifelse(survey.data$paybirthcert==88, NA,
                                 ifelse(survey.data$paybirthcert=="",NA,
                                        ifelse(survey.data$paybirthcert=="0",1,0))))
survey.data$h0m21<- survey.data$agreemultiparty
survey.data$h0m22<- 7-survey.data$agreerespectauth
survey.data$h0m23<- 7-survey.data$agreestrongchief
survey.data$h0m24<- survey.data$agreegenderequal
survey.data$h0m25<- 7-survey.data$agreestrongopposition
survey.data$h0m26<- survey.data$infoperform

##Standardize

survey.data$h0m20<-(survey.data$h0m20-mean(survey.data$h0m20[survey.data$t==0],
                                           na.rm=T))/sd(survey.data$h0m20[survey.data$t==0],
                                                        na.rm=T)
survey.data$h0m21<-(survey.data$h0m21-mean(survey.data$h0m21[survey.data$t==0],
                                           na.rm=T))/sd(survey.data$h0m21[survey.data$t==0],
                                                        na.rm=T)
survey.data$h0m22<-(survey.data$h0m22-mean(survey.data$h0m22[survey.data$t==0],
                                           na.rm=T))/sd(survey.data$h0m22[survey.data$t==0],
                                                        na.rm=T)
survey.data$h0m23<-(survey.data$h0m23-mean(survey.data$h0m23[survey.data$t==0],
                                           na.rm=T))/sd(survey.data$h0m23[survey.data$t==0],
                                                        na.rm=T)
survey.data$h0m24<-(survey.data$h0m24-mean(survey.data$h0m24[survey.data$t==0],
                                           na.rm=T))/sd(survey.data$h0m24[survey.data$t==0],
                                                        na.rm=T)
survey.data$h0m25<-(survey.data$h0m25-mean(survey.data$h0m25[survey.data$t==0],
                                           na.rm=T))/sd(survey.data$h0m25[survey.data$t==0],
                                                        na.rm=T)
survey.data$h0m26<-(survey.data$h0m26-mean(survey.data$h0m26[survey.data$t==0],
                                           na.rm=T))/sd(survey.data$h0m26[survey.data$t==0],
                                                        na.rm=T)

index<-c("h0m1","h0m2","h0m3","h0m4","h0m5","h0m6","h0m7","h0m8","h0m9","h0m10","h0m20","h0m21",
         "h0m22","h0m23","h0m24","h0m25")
survey.data$indexexpectnew<-rowMeans(survey.data[,index], na.rm = T)

survey.data$t2hi<-0 
survey.data$t2low<-0
survey.data$t2hi[(survey.data$t=="2") & (survey.data$indexi>=mean(survey.data$indexi, na.rm = T) |
                                             (is.na(survey.data$indexi)))]<-1
survey.data$t2low[(survey.data$t=="2") & (survey.data$indexi<mean(survey.data$indexi,
                                                                  na.rm = T))]<-1

survey.data$wtanew<-NA
survey.data$wtanew[survey.data$a10000=="1"]<-10000
survey.data$wtanew[survey.data$a5000=="1"]<-5000
survey.data$wtanew[survey.data$a1000=="1"]<-1000
survey.data$wtanew[survey.data$a500=="1"]<-500
survey.data$wtanew[survey.data$avote=="2"]<-NA
survey.data$wtanewusd<-survey.data$wtanew/500

##############
#ANALYSIS
#################

library(nlme)
library(stargazer)

##Table1
fm.table1_1<-lme(indexexpectnew~t1 + t2 + as.factor(block) + as.factor(enumerator),
                 data = survey.data, method = "ML", random = ~1|cid + 1|village, na.action =na.omit)
fm.table1_2<-lme(indexexpectnew~t1 + t2hi + t2low + as.factor(block) + as.factor(enumerator),
                 data = survey.data, method = "ML", random = ~1|cid + 1|village, na.action =na.omit)
stargazer(fm.table1_1,fm.table1_2, align=T, title = "Treatment Effect on Expectations Index",
          omit=5:55, omit.stat = c("ll", "bic", "aic"), out="./Table1.tex", out.header = T,
          covariate.labels = c("T1:Capacity","T2:Capacity + Performance","T2 High", "T2 Low",
                               "Intercept"), dep.var.caption = "", dep.var.labels = "",
          star.cutoffs = c(0.05, 0.01, 0.001))


#Table 2
survey.data$switch<-NA
survey.data$switch<-ifelse((survey.data$vignette=="A") & (survey.data$avote==1)
                           & (is.na(survey.data$wtanewusd)), 0,
                           ifelse((survey.data$vignette=="A")&
                                      (survey.data$avote==1) &
                                      (!is.na(survey.data$wtanewusd)),1,NA))
table(survey.data$switch)
fm.table2<-lme(switch ~ t1 + t2 + as.factor(block) + as.factor(enumerator),
               data = survey.data, method = "ML", random = ~1|cid + 1|village,
               na.action =na.omit)
stargazer(fm.table2, align=T, title = "Treatment Effect on Switching One's Vote",
          keep=1:3, omit.stat = c("ll", "bic", "aic"), out="./Table2.tex",
          out.header = T,
          covariate.labels = c("T1:Capacity","T2:Capacity + Performance", "Intercept"),
          dep.var.caption = "", dep.var.labels = "", star.cutoffs = c(0.05, 0.01, 0.001))

#Code for Table 3
survey.data$bvote0<-ifelse(survey.data$btype==1, survey.data$bvote, NA)
survey.data$bvote1<-ifelse(survey.data$btype==2, survey.data$bvote, NA)
survey.data$bvote2<-ifelse(survey.data$btype==3, survey.data$bvote, NA)
library(plyr)
bvote0.mean<-ddply(.data = survey.data, .(block, cid, t, t1,t2), summarise, 
                     bvote0MEAN = mean(c(bvote0), na.rm = T))
bvote1.mean<-ddply(.data = survey.data, .(block, cid, t, t1,t2), summarise, 
      bvote1MEAN = mean(c(bvote1), na.rm = T))
bvote2.mean<-ddply(.data = survey.data, .(block, cid, t, t1,t2), summarise, 
      bvote2MEAN = mean(c(bvote2), na.rm = T))
collapsed<-merge(bvote0.mean, bvote1.mean, by = c("block", "cid", "t", "t1", "t2"))
collapsed<-merge(collapsed, bvote2.mean, by = c("block", "cid", "t", "t1", "t2"))

library(schoRsch)
library(xtable)
controlkb<-with(collapsed[collapsed$t==0,], t.test(bvote1MEAN,bvote0MEAN,
                                                   var.equal = F, paired = T)$estimate)
t1kb<-with(collapsed[collapsed$t==1,], t.test(bvote1MEAN,bvote0MEAN,
                                              var.equal = F, paired = T)$estimate)
t2kb<-with(collapsed[collapsed$t==2,], t.test(bvote1MEAN,bvote0MEAN,
                                              var.equal = F, paired = T)$estimate)
controlcb<-with(collapsed[collapsed$t==0,], t.test(bvote2MEAN,bvote0MEAN,
                                                   var.equal = F, paired = T)$estimate)
t1cb<-with(collapsed[collapsed$t==1,], t.test(bvote2MEAN,bvote0MEAN,
                                              var.equal = F, paired = T)$estimate)
t2cb<-with(collapsed[collapsed$t==2,], t.test(bvote2MEAN,bvote0MEAN,
                                              var.equal = F, paired = T)$estimate)

collapsed$diff1<-collapsed$bvote1MEAN-collapsed$bvote0MEAN
collapsed$diff2<-collapsed$bvote2MEAN-collapsed$bvote0MEAN
collapsed$diff3<-collapsed$bvote2MEAN-collapsed$bvote1MEAN

collapsed<-reshape(collapsed, timevar = "t", idvar = "block", direction = "wide")
t1c.kc<-with(collapsed, t.test(diff1.1,diff1.0, var.equal = F, paired = T)$estimate)
t2c.kc<-with(collapsed, t.test(diff1.2,diff1.0, var.equal = F, paired = T)$estimate)
t2t1.kc<-with(collapsed, t.test(diff1.2,diff1.1, var.equal = F, paired = T)$estimate)
t1c.cc<-with(collapsed, t.test(diff2.1,diff2.0, var.equal = F, paired = T)$estimate)
t2c.cc<-with(collapsed, t.test(diff2.2,diff2.0, var.equal = F, paired = T)$estimate)
t2t1.cc<-with(collapsed, t.test(diff2.2,diff2.1, var.equal = F, paired = T)$estimate)

#Table 3
df3a<-data.frame(c("Difference (Kin-Baseline)","Difference (Chief-Baseline)"),
               Control=c(controlkb, controlcb), T1=c(t1kb, t1cb), T2=c(t2kb, t2cb),
               fix.empty.names = F)
table3a<-xtable(caption="A: First Differences: Mean Vote Share for Candidate A",df, digits = 3 )
df3b<-data.frame(c("Kin Condition","Chief Condition"), "T1-C"=c(t1c.kc, t1c.cc),
                "T2-C"=c(t2c.kc, t2c.cc), "T2-T1"=c(t2t1.kc, t2t1.cc),
                fix.empty.names = F, check.names = F)
table3b<-xtable(caption="B: Difference-in-Difference",df2, digits = 3)

print(table3a, caption.placement="top", file = "./Table3a.tex")
print(table3b, caption.placement="top", file = "./Table3b.tex")

##Code for Table 4


collapse2<-ddply(.data = merge.data[is.na(merge.data$prefet),],
                 .(cid, t, nvillages), summarise, 
                   challengeSUM = sum(c(challengecomplaint), na.rm = T))
collapse2$tc<-NA
collapse2$tc<-ifelse(collapse2$t==0, 0, ifelse(collapse2$t==1,1,
                                               ifelse(collapse2$t==2,1,NA)))

t1c<-with(collapse2, t.test(challengeSUM[collapse2$t==0],challengeSUM[collapse2$t==1],
                            var.equal = T, paired = F))
t2c<-with(collapse2, t.test(challengeSUM[collapse2$t==0],challengeSUM[collapse2$t==2],
                            var.equal = T, paired = F))
tc<-with(collapse2, t.test(challengeSUM[collapse2$tc==0],challengeSUM[collapse2$tc==1],
                           var.equal = T, paired = F))


#Conduct randomization inference to extract "Exact p value"

library(ri2)

tempdata<-collapse2[(!is.na(collapse2$t)) & ((collapse2$t==1)| (collapse2$t==0)),]
set.seed(1018)
ri1_out <- conduct_ri(
    formula = challengeSUM ~ t,
    declaration= declare_ra(N=21, m=13),
    assignment= "t",
    sharp_hypothesis = 0,
    data = tempdata
)
ri1_out #T1-C p value


tempdata<-collapse2[(!is.na(collapse2$t)) & ((collapse2$t==2)| (collapse2$t==0)),]
set.seed(1018)
ri2_out <- conduct_ri(
    formula = challengeSUM ~ tc,
    declaration= declare_ra(N=20, m=12),
    assignment= "tc",
    sharp_hypothesis = 0,
    data = tempdata
)
ri2_out #T2-C p value

tempdata<-collapse2[(!is.na(collapse2$tc)),]
set.seed(1018)
ri3_out <- conduct_ri(
    formula = challengeSUM ~ tc,
    declaration= declare_ra(N=33, m=25),
    assignment= "tc",
    sharp_hypothesis = 0,
    data = tempdata
)
ri3_out #T-C p value

df4.2<-data.frame(c("T1-C", "T2-C", "T-C"), 
                  "Difference"=c(t1c$estimate[[2]]-t1c$estimate[[1]],
                                 t2c$estimate[[2]]-t2c$estimate[[1]], 
                                 tc$estimate[[2]]-tc$estimate[[1]]), 
                  "p-Value (Two-Sided)"=c(t1c$p.value, t2c$p.value,tc$p.value),
                  "Exact p"=c(summary(ri1_out)[[3]], summary(ri2_out)[[3]],
                              summary(ri3_out)[[3]]),
                  fix.empty.names = F, check.names = F)


#Table 4
table4<-xtable(caption="Mean Differences",df4.2, digits = c(0,0,2,3,3), booktabs=T)
print(table4, caption.placement="top", file = "./Table4.tex")