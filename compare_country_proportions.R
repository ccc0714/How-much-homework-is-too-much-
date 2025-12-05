library(foreign)

#Make sure to change the path to your Directory for storing the dataset 
load_timss_country <- function(code, path = "YourDirectory") {
  
  code <- tolower(code)   # ensure filenames match pattern
  
  prefixes <- c("bcg", "bsg", "bst", "btm", "bts")
  
  data_list <- list()
  
  for (p in prefixes) {
    file_path <- file.path(path, paste0(p, code, "m7.sav"))
    object_name <- paste0(toupper(code), "_", toupper(p))
    
    data_list[[object_name]] <- as.data.frame(read.spss(file_path))
  }
  
  return(data_list)
}

df.clean <- function(BCG, BSG, BST, BTM, BTS){
  BST_BTM<-merge(BST, BTM, by="IDTEALIN")
  
  order1<-as.numeric(as.character(BST_BTM$IDSTUD))
  order2<-as.numeric(as.character(BST_BTM$BTBM14))
  order2<-ifelse(is.na(order2), 0, order2)*-1
  
  BST_BTM<-BST_BTM[order(order1, order2),]
  
  support_teachers<-c()
  
  for(i in 2:(length(BST_BTM$IDSTUD)))
  {
    if(BST_BTM$IDSTUD[i] == BST_BTM$IDSTUD[i-1])
    {
      support_teachers<-c(support_teachers, i)
    }
  }
  
  BST_BTM<-BST_BTM[-support_teachers,]
  
  #Each row now has the context data added to it.
  BSG_BST_BTM<-merge(BSG, BST_BTM, by="IDSTUD", all.x=T)
  
  #Each row now has school data too.
  BSG_BST_BTM_BCG<-merge(BSG_BST_BTM, BCG, by="IDSCHOOL")
  
  BST_BTS<-merge(BST, BTS, by="IDTEALIN")
  
  order1<-as.numeric(as.character(BST_BTS$IDSTUD))
  order2<-as.numeric(as.character(BST_BTS$BTBS14))
  order2<-ifelse(is.na(order2), 0, order2)*-1
  
  BST_BTS<-BST_BTS[order(order1, order2),]
  
  support_teachers<-c()
  
  for(i in 2:(length(BST_BTS$IDSTUD)))
  {
    if(BST_BTS$IDSTUD[i] == BST_BTS$IDSTUD[i-1])
    {
      support_teachers<-c(support_teachers, i)
    }
  }
  
  BST_BTS<-BST_BTS[-support_teachers,]
  
  BSG_BST_BTS<-merge(BSG, BST_BTS, by="IDSTUD", all.x=T)
  
  BSG_BST_BTS_BCG<-merge(BSG_BST_BTS, BCG, by="IDSCHOOL")
  
  #Select Columns
  maths_vars<-c("IDCNTRY.x", "BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
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
  
  
  maths_treatment<-c("BSBM26AA", "BSBM26BA")
  
  
  science_vars<-c("IDCNTRY.x", "BSDAGE", "BSBG01", "BSBG03", "BSBG04", "BSBG07",
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
                  
                  "BTBS18CA", "BTBS18CB", "BTBS18CC", "BTBS18CD", "BTBS18CE", "BTBS14")
  
  science_other<-c("BSSSCI01.x", "BSSSCI02.x", "BSSSCI03.x",
                   "BSSSCI04.x", "BSSSCI05.x", "IDCLASS.x", "TOTWGT", "IDSTUD")
  
  science_treatment<-c("BSBS26AB", "BSBS26BB")
  
  school_factor <- c("BCBG05B", "BTBG04", "BTDGEAS", "BTDGLSN", "BTDGSOS", "BTDGTJS")
  
  XYM <-BSG_BST_BTM_BCG[,c(maths_vars, maths_other, maths_treatment, school_factor)]
  XYS <-BSG_BST_BTS_BCG[,c(science_vars, science_other, science_treatment, school_factor)]
  factor_XYM <- XYM[c(1:10, 17:26, 28:29, 35, 39:44, 53:61)]
  factor_XYS <- XYS[c(1:10, 17:26, 28:29, 35, 39:44, 53:61)]
  XY<-merge(factor_XYM, factor_XYS, by="IDSTUD")
  return(XY)
}

df.clean.list <- function(data_list) {
  BCG <- data_list[[grep("_BCG$", names(data_list))]]
  BSG <- data_list[[grep("_BSG$", names(data_list))]]
  BST <- data_list[[grep("_BST$", names(data_list))]]
  BTM <- data_list[[grep("_BTM$", names(data_list))]]
  BTS <- data_list[[grep("_BTS$", names(data_list))]]
  
  df.clean(BCG, BSG, BST, BTM, BTS)
}

#Functions for viewing, plotting and creating proportions dataframe 
view_prop <- function(index){
  print(prop_df$variable[index])
  prop.table(table(XY$IDCNTRY.x.x, XY[, prop_df$variable[index]]), 1)
}

plot_prop <- function(index){
  target <- prop_df$variable[index]
  plot_data <- XY %>%
    group_by(IDCNTRY.x.x, .data[[target]]) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    group_by(IDCNTRY.x.x) %>%
    mutate(prop = n / sum(n))
  
  ggplot(plot_data, aes(x = .data[[target]], y = prop, fill = IDCNTRY.x.x)) +
    geom_col(position = position_dodge()) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
}

classify_cramers_v <- function(v, df_v) {
  
  if (df_v == 1) { 
    if (v < 0.30) return("Weak")
    else if (v < 0.50) return("Moderate")
    else return("Strong")
    
  } else if (df_v == 2) { 
    if (v < 0.21) return("Weak")
    else if (v < 0.35) return("Moderate")
    else return("Strong")
    
  } else if (df_v == 3) { 
    if (v < 0.17) return("Weak")
    else if (v < 0.29) return("Moderate")
    else return("Strong")
    
  } else if (df_v == 4) { 
    if (v < 0.15) return("Weak")
    else if (v < 0.25) return("Moderate")
    else return("Strong")
    
  } else { 
    if (v < 0.13) return("Weak")
    else if (v < 0.22) return("Moderate")
    else return("Strong")
  }
}

#Make sure to change xxx and yyy to your target countries in iso3n form 
countries <- c("xxx", "yyy")

# Load files for both countries
data_country1 <- load_timss_country(countries[1])
data_country2 <- load_timss_country(countries[2])

# Clean to create XY datasets
XY1 <- df.clean.list(data_country1)
XY2 <- df.clean.list(data_country2)

# Combine
XY <- rbind(XY1, XY2)

# Fix country code & convert to names
XY$IDCNTRY.x.x <- droplevels(XY$IDCNTRY.x.x)
XY$IDCNTRY.x.x <- countrycode(XY$IDCNTRY.x.x, "iso3n", "country.name")

#Identifying V value for categorical variables against countries 
n <- 38
prop_df <- data.frame(variable = character(n), v_df = numeric(n), v_value = numeric(n), strength = character(n))
index <- 1
for (i in c(4:33, 61, 63:69)){
  variable <- colnames(XY)[i]
  data <- table(XY$IDCNTRY.x.x, XY[, i])
  v_value <- cramerV(data)
  v_df <- min(nrow(data)-1, ncol(data)-1)
  level <- classify_cramers_v(v_value, v_df)
  prop_df$variable[index] <- variable
  prop_df$v_value[index] <- v_value
  prop_df$v_df[index] <- v_df
  prop_df$strength[index] <- level
  index <- index + 1
}

