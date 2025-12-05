library(tidyverse)
library(haven)
library(plyr)
library(missRanger)
library(mltools)
library(data.table)
library(dbarts)
library(Rcpp)

#Make sure to change xxx to your country iso3n code and your_directory to directory that stores the data
load("your_directory/bcgxxxm8.rdata")
load("your_directory/bsgxxxm8.rdata")
load("your_directory/bstxxxm8.rdata")
load("your_directory/btmxxxm8.rdata")
load("your_directory/btsxxxm8.rdata")

BCG <- BCGXXXM8 #School context data files
BSG <- BSGXXXM8 #Student context data files
BST <- BSTXXXM8 #Student-teacher linkage files
BTM <- BTMXXXM8 #Mathematics teacher context data files
BTS <- BTSXXXM8 #Science teacher context data files 

#Merging Student-teacher linkage and Mathematics teacher context data
BST_BTM <- merge(BST, BTM, by = "IDTEALIN")

#Ordering BST_BTM by Student ID (IDSTUD) and time spent on math homework (BTBM14) for tie breaker
order1 <- as.numeric(as.character(BST_BTM$IDSTUD)) 
order2 <- as.numeric(as.character(BST_BTM$BTBM14))
order2 <- ifelse(is.na(order2), 0, order2)*-1

BST_BTM <- BST_BTM[order(order1, order2), ]

#Removing repititive IDSTUD
support_teachers<-c()
for(i in 2:(length(BST_BTM$IDSTUD)))
{
  if(BST_BTM$IDSTUD[i] == BST_BTM$IDSTUD[i-1])
  {
    support_teachers<-c(support_teachers, i)
  }
}
BST_BTM<-BST_BTM[-support_teachers,]

#Each row now has student context data 
BSG_BST_BTM<-merge(BSG, BST_BTM, by="IDSTUD", all.x=T)
#Each row now has school context data 
BSG_BST_BTM_BCG<-merge(BSG_BST_BTM, BCG, by="IDSCHOOL")

#Merging Student-teacher linkage and Science teacher context data
BST_BTS <- merge(BST, BTS, by = "IDTEALIN")

#Ordering BST_BTM by Student ID (IDSTUD) and time spent on science homework (BTBS14) for tie breaker
order1<-as.numeric(as.character(BST_BTS$IDSTUD))
order2<-as.numeric(as.character(BST_BTS$BTBS14))
order2<-ifelse(is.na(order2), 0, order2)*-1

BST_BTS<-BST_BTS[order(order1, order2),]

#Removing repititive IDSTUD
support_teachers<-c()

for(i in 2:(length(BST_BTS$IDSTUD)))
{
  if(BST_BTS$IDSTUD[i] == BST_BTS$IDSTUD[i-1])
  {
    support_teachers<-c(support_teachers, i)
  }
}

BST_BTS<-BST_BTS[-support_teachers,]

#Each row now has student context data 
BSG_BST_BTS<-merge(BSG, BST_BTS, by="IDSTUD", all.x=T)
#Each row now has the school context data 
BSG_BST_BTS_BCG<-merge(BSG_BST_BTS, BCG, by="IDSCHOOL")

maths_vars<-c("BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
              "BSBG08A", "BSBG08B",
              "BSBG09A", "BSDGEDUP", "BSBGHER",
              "BSBGSSB", "BSBGSB", "BSBGICM",
              "BSBGSCM", "BSBGSVM",
              
              "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E", "BSBG05F", "BSBG05G", 
              "BSBG11A", "BSBG11B", "BSBG10",
              
              "BTBG01", "BTBG02", "BTBG03", "BTBG10",
              "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDMMME",
              
              "BCBGDAS", "BCBGEAS", "BCBGMRS",
              "BCDGSBC",
              
              "BTBM19CA", "BTBM19CB", "BTBM19CC", "BTBM19CD", "BTBM19CE", "BTBM14")


maths_other<-c("BSMMAT01.x", "BSMMAT02.x", "BSMMAT03.x", 
               "BSMMAT04.x", "BSMMAT05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")


maths_treatment<-c("BSBM30A") #Replacing BSBM26AA with BSBM30A

science_vars<-c("BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
                "BSBG08A", "BSBG08B",
                "BSBG09A", "BSDGEDUP", "BSBGHER",
                "BSBGSSB", "BSBGSB", "BSBGICS",
                "BSBGSCS", "BSBGSVS",
                
                "BSBG05A", "BSBG05B", "BSBG05C", "BSBG05D", "BSBG05E", "BSBG05F", "BSBG05G", 
                "BSBG11A", "BSBG11B", "BSBG10",
                
                "BTBG01", "BTBG02", "BTBG03", "BTBG10",
                "BTBGTJS", "BTBGSOS", "BTBGLSN", "BTBGEAS", "BTDSMSE",
                
                "BCBGDAS", "BCBGEAS", "BCBGSRS",
                "BCDGSBC",
                
                "BTBS23BA", "BTBS23BB", "BTBS23BC", "BTBS23BD", "BTBS23BE", "BTBS14")

science_other<-c("BSSSCI01.x", "BSSSCI02.x", "BSSSCI03.x",
                 "BSSSCI04.x", "BSSSCI05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")

science_treatment<-c("BSBS30B")

XYM<-BSG_BST_BTM_BCG[,c(maths_vars, maths_other, maths_treatment)]
XYS<-BSG_BST_BTS_BCG[,c(science_vars, science_other, science_treatment)]
#------------------------------------------------------------------------------
label <- c()
filtered_XY <- cbind(XYM[, c(2:9, 16:25, 27:28, 34, 38:43, 53)], XYS$BTDSMSE)
for (i in 1:(ncol(filtered_XY)))
{
  label <- c(label, attr(filtered_XY[, i], "labels"))
}
from <- unique(names(label))
to <- c(0, 1, 2, NA,
        3, 2, 1, 0,
        5, 20, 50, 150, 200,
        2, 3, 4, 5, 6, 8,
        1, 0, NA, NA, 
        5, 4, 3, 2, 1, 0,
        5, 2,
        4, 3, 2, 1, 0,
        0, 1,
        25, 29, 39, 49, 59, 66,
        2, 1, 1, 0, 0,
        3, 2, 1,
        2, 1, 0,
        3.5, 1.5, 0.5,
        2, 1, 1)

mapna <- function(df)
{
  for (i in 1:53)
  {
    df_NA <- attr(df[, i], "na_values")
    df[, i] <- replace(df[, i], df[, i] %in% df_NA, NA)
    if (i %in% c(2:9, 16:25, 27:28, 34, 38:43, 53))
    {
      df_label <- attr(df[, i], "labels")
      df[, i] <- names(df_label)[match(df[, i], df_label)]
    }
  }
  return(df)
}

mymap<-function(x)
{
  x<-as.numeric(as.character(x))
  
  return(x)
}

mymap2<-function(x)
{
  x<-mapvalues(x,
               from=from,
               to=to)
  
  x<-as.numeric(as.character(x))
  
  return(x)
}

XYM <- mapna(XYM)
XYS <- mapna(XYS)

XYM<-mutate_at(XYM, c(2:9, 16:25, 27:28, 34, 38:43, 53), mymap2)
XYS<-mutate_at(XYS, c(2:9, 16:25, 27:28, 34, 38:43, 53), mymap2)

XYM<-mutate_at(XYM, c(1, 10:15, 26, 29:33, 35:37, 44:52), mymap)
XYS<-mutate_at(XYS, c(1, 10:15, 26, 29:33, 35:37, 44:52), mymap)

XY <- merge(XYM, XYS, by = "IDSTUD")
view(XY)
NA_percentage <- sum(is.na(XY))/prod(dim(XY))

complete_XY <- XY[complete.cases(XY),]
impute_XY <- missRanger(XY, num.threads=6, num.trees=50, pmm.k = 3)


