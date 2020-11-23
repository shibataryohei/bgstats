require(rstatix)

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
                                     paste(""))))) %>% 
    mutate_if(is.character, as.factor) -> result
  
  c(paste("Number", levels[1], sep = "_"),
    paste("Ratio", levels[1], sep = "_"),
    paste("Number", levels[2], sep = "_"),
    paste("Ratio", levels[2], sep = "_")) -> colnames(result)[3:6] 
  
  result %>% tibble::as_tibble()} 

# fisher_table2 #####
fisher_table2 <- function(df, group, omit){
  df %>% 
    dplyr::select(where(is.factor)) %>% 
    mutate_all(droplevels) -> df1
  
  df1[[group]] %>% 
    levels -> levels
  
  df1[[group]] %>% summary -> number_groups
  
  df1[, !names(df1) %in% c(omit, group)] -> df2
  
  lapply(1:ncol(df2), function(i){
    df1[[colnames(df2)[i]]] %>% 
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
    dplyr::select(Variable, Variable1, Variable2, P.value, SD) %>% 
    mutate_if(is.character, as.factor) -> result
  
  colnames(result)[2] <- paste0(levels[1], " (N = ", number_groups[1], ")")
  colnames(result)[3] <- paste0(levels[2], " (N = ", number_groups[2], ")")
  
  result %>% tibble::as_tibble()
}

# wilcox_table #####
wilcox_table <- function(df, group, omit, digits){
  
  df[, group] -> df_group
  colnames(df_group) <- "group"
  
  fmt = paste0("%#.", digits, "f")
  
  df %>%  
    dplyr::select(where(is.numeric)) %>% 
    bind_cols(df_group, .) %>% 
    gather(Variable, Value, -group) -> df1 
  
  df[[group]] %>% 
    summary -> number_groups
  
  df1 %>% 
    group_by(Variable, group) %>% 
    get_summary_stats(Value,
                      show = c("median", "q1", "q3")) %>% 
    mutate(Value = paste0(sprintf(median, fmt = fmt),
                          " (",
                          sprintf(q1, fmt = fmt),
                          " – ",
                          sprintf(q3, fmt = fmt),
                          ")")) %>% 
    dplyr::select(Variable, Value, group) %>% 
    spread(group, Value) -> mq1q3
      
  df1 %>% 
    group_by(Variable) %>% 
    mutate(Variable = as.factor(Variable)) %>% 
    rstatix::wilcox_test(Value ~ group) %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(P.value = p) %>% 
    dplyr::select(Variable, P.value) %>% 
    inner_join(mq1q3, .) %>% 
    mutate(SD = ifelse(P.value < 0.001, paste("***"),
                       ifelse(P.value < 0.01,paste("**"),
                              ifelse(P.value < 0.05,paste("*"),
                                     paste(""))))) %>% 
    mutate_if(is.character, as.factor) %>% 
    arrange(P.value) -> result
  
  colnames(result)[2] <- paste0(colnames(result)[2], " (N = ", number_groups[1], ")")
  colnames(result)[3] <- paste0(colnames(result)[3], " (N = ", number_groups[2], ")")
  
  result
} 

# wilcox_fisher_table #####
wilcox_fisher_table <- function(df, group, omit, digits){
  df[, !names(df) %in% c(omit)] -> df
  fisher_table2(df,
            group = group,
            omit = NA) -> result_fisher
  
  wilcox_table(df,
            group = group,
            omit = NA,
            digits = digits) -> result_wilcox
  
  result_fisher %>% 
    bind_rows(result_wilcox) %>% 
    arrange(P.value) 
  }


# Case control ######
# Wilcoxon CI ######

wilcox_CI_table <- function(df, group, omit, digits){
  
  df[, group] -> df_group
  colnames(df_group) <- "group"
  df_group$group %>% levels %>% .[1] -> group1
  df_group$group %>% levels %>% .[2] -> group2
  
  
  
  fmt = paste0("%#.", digits, "f")
  
  df %>%  
    dplyr::select_if(is.numeric) %>% 
    bind_cols(df_group, .) %>% 
    gather(Variable, Value, -group) -> df1 
  
  df[[group]] %>% 
    summary -> number_groups
  
  df1 %>% 
    group_by(Variable, group) %>% 
    get_summary_stats(Value,
                      show = c("median")) %>% 
    mutate(median = format(median, digits = digits)) %>% 
    unite(group, group, n, sep = " (N = ") %>% 
    mutate(group = paste0(group, ")")) %>% 
    dplyr::select(-variable) %>% 
    spread(group, median) -> df_median
  
  df1 %>% 
    mutate(Variable = as.factor(Variable)) %>% 
    group_by(Variable) %>% 
    dplyr::summarise_at("Value",
                        funs(P.value = wilcox.exact(.[group == group1],
                                                    .[group == group2],
                                                    paired = FALSE)$p.value,
                             CI_Low = wilcox.exact(.[group == group1],
                                                   .[group == group2],
                                                   paired = FALSE,
                                                   conf.int = T)$conf.int[1],
                             CI_High = wilcox.exact(.[group == group1],
                                                    .[group == group2],
                                                    paired = FALSE,
                                                    conf.int = T)$conf.int[2],
                             DIL = wilcox.exact(.[group == group1],
                                                .[group == group2],
                                                paired = FALSE,
                                                conf.int = T)$estimate)) %>% 
    mutate(`Difference in location [95% CI]` = paste0(sprintf(DIL, fmt = fmt),
                                                      " [",
                                                      sprintf(CI_Low, fmt = fmt),
                                                      " – ",
                                                      sprintf(CI_High, fmt = fmt),
                                                      "]")) %>% 
    dplyr::select(Variable, `Difference in location [95% CI]`, P.value) %>% 
    mutate(SD = ifelse(P.value < 0.001, paste("***"),
                       ifelse(P.value < 0.01,paste("**"),
                              ifelse(P.value < 0.05,paste("*"),
                                     paste(""))))) %>% 
    mutate_if(is.character, as.factor) %>% 
    inner_join(df_median, .)
} 


# fisher

fisher_CI_table <- function(df, group, omit, digits){
  fmt = paste0("%#.", digits, "f")
  
  df %>% 
    dplyr::select_if(is.factor) %>% 
    mutate_all(droplevels) -> df1
  
  df1[[group]] %>% 
    levels -> levels
  
  df1[[group]] %>% summary -> number_groups
  
  df1[, !names(df1) %in% c(omit, group)] -> df2
  
  lapply(1:ncol(df2), function(i){
    df1[[colnames(df2)[i]]] %>% 
      levels -> factor
    
    df2[, colnames(df2)[i]] %>% 
      bind_cols(df1[, group]) %>% 
      table -> table
    
    data.frame(
      Variable = colnames(df2)[i],
      Factor = factor[1],
      Number1 = table[1, 1],
      Ratio1 =  table[1, 1]/sum(table[,1]),
      Number2 = table[1, 2],
      Ratio2 =  table[1, 2]/sum(table[,2]),
      P.value = fisher.test(table)$p.value,
      OR = fisher.test(table)$estimate,
      CI_Low = fisher.test(table)$conf.int[1],
      CI_High = fisher.test(table)$conf.int[2])
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
    mutate(`Odds ratio [95% CI]` = paste0(sprintf(OR, fmt = fmt),
                                          " [",
                                          sprintf(CI_Low, fmt = fmt),
                                          " – ",
                                          sprintf(CI_High, fmt = fmt),
                                          "]")) %>% 
    dplyr::select(Variable, Variable1, Variable2,
                  `Odds ratio [95% CI]`, P.value, SD) %>% 
    mutate_if(is.character, as.factor) -> result
  
  colnames(result)[2] <- paste0(levels[1], " (N = ", number_groups[1], ")")
  colnames(result)[3] <- paste0(levels[2], " (N = ", number_groups[2], ")")
  
  result %>% tibble::as_tibble()
}
