

# bg_fisher #####
fisher_table <- function(df, group, omit){
  df %>% 
    dplyr::select(where(is.factor)) %>% 
    mutate_all(droplevels) -> df1
  
  df1[, group] %>% 
    as.matrix %>%
    as.factor %>%
    levels -> levels
  
  df1[, !names(df1) %in% c(omit, group)] -> df2
  lapply(1:ncol(df2),
         function(i){
           df2[, colnames(df2)[i]] %>%
             as.matrix %>%
             as.factor %>%
             levels -> factor
           
           df2[, colnames(df2)[i]] %>% 
             bind_cols(df1[, group]) %>% 
             table -> table
           
           fisher.test(table) %>% 
             .$p.value %>% 
             data.frame(
               Variable = colnames(df2)[i],
               Factor = factor[1],
               Number1 = table[1, 1],
               Ratio1 =  table[1, 1]/sum(table[,1]),
               Number2 = table[1, 2],
               Ratio2 =  table[1, 2]/sum(table[,2]),
               P.value = .)
         }) %>% 
    do.call(bind_rows, .) %>% 
    mutate(SD = ifelse(P.value < 0.001, paste("***"),
                       ifelse(P.value < 0.01,paste("**"),
                              ifelse(P.value < 0.05,paste("*"),
                                     paste(""))))) -> result
  
  c(paste("Number", levels[1], sep = "_"),
    paste("Ratio", levels[1], sep = "_"),
    paste("Number", levels[2], sep = "_"),
    paste("Ratio", levels[2], sep = "_")) -> colnames(result)[3:6] 
  
  result %>% tbl_df} 

# bg_fisher_table #####
fisher_table2 <- function(df, group, omit){
  df %>% 
    dplyr::select(where(is.factor)) %>% 
    mutate_all(droplevels) -> df1
  
  df1[, group] %>% 
    as.matrix %>%
    as.factor %>%
    levels -> levels
  
  df1[, !names(df1) %in% c(omit, group)] -> df2
  
  lapply(1:ncol(df2), function(i){
           df2[, colnames(df2)[i]] %>%
             as.matrix %>%
             as.factor %>%
             levels -> factor
           
           df2[, colnames(df2)[i]] %>% 
             bind_cols(df1[, group]) %>% 
             table -> table
           
           fisher.test(table) %>% 
             .$p.value %>% 
             data.frame(
               Variable = colnames(df2)[i],
               Factor = factor[1],
               Number1 = table[1, 1],
               Ratio1 =  table[1, 1]/sum(table[,1]),
               Number2 = table[1, 2],
               Ratio2 =  table[1, 2]/sum(table[,2]),
               P.value = .)
         }) %>% 
    do.call(bind_rows, .) %>% 
    mutate(SD = ifelse(P.value < 0.001, paste("***"),
                       ifelse(P.value < 0.01,paste("**"),
                              ifelse(P.value < 0.05,paste("*"),
                                     paste(""))))) %>% 
    mutate(Variable1 = paste0(Number1, " (",
                              sprintf(Ratio1*100, fmt = '%#.1f'), "%)" )) %>% 
    mutate(Variable2 = paste0(Number2, " (",
                              sprintf(Ratio2*100, fmt = '%#.1f'), "%)" )) %>% 
    mutate(Variable = paste0(Variable, " [", Factor, "]")) %>% 
    dplyr::select(Variable, Variable1, Variable2, P.value, SD) -> result
  
  colnames(result)[2] <- paste(levels[1])
  colnames(result)[3] <- paste(levels[2])
  
  result %>% tbl_df
}


# bg_wilcox #####
wilcox_table <- function(df, group, omit, digits){
  
  df[, group] -> df_group
  colnames(df_group) <- "Group"
  
  fmt = paste0("%#.", digits, "f")

    df %>%  
    dplyr::select(where(is.numeric)) %>% 
    bind_cols(df_group, .) %>% 
    gather(Variable, Value, -Group) -> df1 
  
  df1 %>% 
    group_by(Variable, Group) %>% 
    get_summary_stats(Value,
                      show = c("median", "q1", "q3")) %>% 
    mutate(Value = paste0(format(median, digits = digits),
                          " (",
                          sprintf(q1, fmt = fmt),
                          " - ",
                          sprintf(q3, fmt = fmt),
                          ")")) %>% 
    dplyr::select(Variable, Value, Group) %>% 
    spread(Group, Value) -> mq1q3
      
  df1 %>% 
    group_by(Variable) %>% 
    mutate(Variable = as.factor(Variable)) %>% 
    rstatix::wilcox_test(Value ~ Group) %>% 
    tbl_df %>% 
    dplyr::rename(P.value = p) %>% 
    dplyr::select(Variable, P.value) %>% 
    inner_join(mq1q3, .) %>% 
    mutate(SD = ifelse(P.value < 0.001, paste("***"),
                       ifelse(P.value < 0.01,paste("**"),
                              ifelse(P.value < 0.05,paste("*"),
                                     paste("")))))
} 

# bg_tableone #####
wilcox_fisher_table <- function(df, group, omit, digits){
  fisher_tables(df,
            group = group,
            omit = omit) -> result_fisher
  
  wilcox_table(df,
            group = group,
            omit = omit,
            digits = digits) -> result_wilcox
  
  result_fisher %>% 
    bind_rows(result_wilcox) %>% 
    arrange(P.value)
  }
