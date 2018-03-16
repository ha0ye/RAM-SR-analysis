rm(list = ls())
source("my_functions.R")
library(rEDM)
library(parallel)

if(FALSE)
{
    extract_data() # pull data from RAM website
    process_data() # process SR data
    summarize_data()
    
    get_doc_info()
    load("stock_ids.Rdata")
    both <- intersect(andi_stock_ids, web_stock_ids)
    a_not_w <- setdiff(andi_stock_ids, web_stock_ids)
    w_not_a <- setdiff(web_stock_ids, andi_stock_ids)
    
    run_univariate_analysis()
    
}

