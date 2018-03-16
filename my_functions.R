extract_data <- function()
{
    my_db <- list()
    
    main_page <- readLines("http://ram.biology.dal.ca/~myers/data.html")
    
    idx <- grep("<h3>.+</h3>", main_page)
    family_names <- gsub("</*h3>", "", main_page[idx])
    
    for(i in idx)
    {
        family_name <- gsub("</*h3>", "", main_page[i])
        curr_line <- i+1
        
        while(!grepl("</ul>", main_page[curr_line]))
        {
            curr_line <- curr_line + 1
            if(grepl("</ul>", main_page[curr_line]))
                break
            species_name <- sub("<li><.+>([A-Z][a-z]+ [a-z]+).+", "\\1", main_page[curr_line])
            sub_link <- sub("<li><A HREF=\"(.+)\".+", "\\1", main_page[curr_line])
            sub_page <- readLines(sub_link)
            
            # for each location
            loc_idx <- grep("<h3>", sub_page)
            for(j in seq_along(loc_idx))
            {
                if(j < length(loc_idx))
                    line_span <- loc_idx[j]:(loc_idx[j+1]-1)
                else
                    line_span <- loc_idx[j]:length(sub_page)
                
                loc_text <- sub_page[line_span]
                
                # grab location
                loc_pattern <- ".*<li><h3>(.+): +.+documentation</A></li>"
                location <- sub(loc_pattern, "\\1", loc_text[grep(loc_pattern, loc_text)])
                
                # grab documentation
                doc_pattern <- ".+<A HREF=\"(.+)\">documentation<.+"
                doc_link <- sub(doc_pattern, "\\1", loc_text[grep(doc_pattern, loc_text)])
                doc <- tryCatch(readLines(doc_link), 
                                error = function(e) {
                                    message("encounted an error while reading ", doc_link)
                                    return(NA)}, 
                                finally = {})
                
                # grab data
                data_pattern <- ".+<A HREF=\"(.+)\">data<.+"
                data_link <- sub(data_pattern, "\\1", loc_text[grep(data_pattern, loc_text)])
                data <- tryCatch(readLines(data_link), 
                                 error = function(e) {
                                     message("encounted an error while reading ", data_link)
                                     return(NA)}, 
                                 finally = {})
                
                my_db[[length(my_db)+1]] <- list(family = family_name, 
                                                 species_name = species_name, 
                                                 location = location, 
                                                 documentation = doc, 
                                                 data = data)
            }
        }    
    }
    
    # fix extra columns
    my_db[[412]]$data[1] <- " 1970      . 122673     .      ."
    my_db[[412]]$data[2] <- " 1971      . 158172     .      ."
    my_db[[412]]$data[3] <- " 1972      . 159831     .      ."
    my_db[[412]]$data[4] <- " 1973      . 176089     .      ."
    my_db[[412]]$data[5] <- " 1974      . 175696     .      ."
    
    my_db <- my_db[-c(740:750, 756:759)] # missing freshwater brook trout data
    my_db <- my_db[-c(128, 415, 416, 417, 418, 419)] # duplicated data
    
    save(my_db, file = "web_data.Rdata")
    return()
}

process_data <- function()
{
    load("web_data.Rdata")
    for(i in 1:length(my_db))
    {
        sr_data <- my_db[[i]]
        
        # trim leading and trailing whitespace
        data <- gsub("^\\s+", "", sr_data$data)
        data <- gsub("\\s+$", "", data)
        
        # trim empty lines
        while(tail(data, n = 1) == "")
            data <- data[-length(data)]
        while(head(data, n = 1) == "")
            data <- data[-1]
        
        # convert string data into numeric
        data <- t(sapply(data, function(x) {unlist(strsplit(x, "\\s+"))}, USE.NAMES = FALSE))
        gsub("^.$", "NA", data)
        class(data) <- "numeric"
        my_db[[i]]$data <- data.frame(year = rep.int(NA, NROW(data)), 
                                      ssb = rep.int(NA, NROW(data)), 
                                      rec = rep.int(NA, NROW(data)), 
                                      land = rep.int(NA, NROW(data)), 
                                      frpl = rep.int(NA, NROW(data)))
        for(j in 1:NCOL(data))
            my_db[[i]]$data[, j] <- data[, j]
    }
    save(my_db, file = "sr_data.Rdata")
    return()
}

summarize_data <- function()
{
    load("sr_data.Rdata")
    rec_count <- sapply(my_db, function(sr_data) {
        sum(is.finite(sr_data$data$rec))
    })
    my_db <- my_db[rec_count >= 10]
    save(my_db, file = "sr_data_10.Rdata")
    return()
}

get_doc_info <- function()
{
    andi_stock_ids <- c()
    dir <- "andi/sr/DOC/"
    doc_names <- list.files(dir)
    for(doc_file in doc_names)
    {
        doc_text <- readLines(paste(dir, doc_file, sep = ""))
        sp <- substr(doc_text[3], 13, nchar(doc_text[3]))
        sp <- gsub("^\\s+", "", sp)
        sp <- gsub("\\s+$", "", sp)
        
        stock <- substr(doc_text[7], 13, nchar(doc_text[7]))
        stock <- gsub("^\\s+", "", stock)
        stock <- gsub("\\s+$", "", stock)
        
        id <- substr(doc_text[1], 13, nchar(doc_text[1]))
        id <- gsub("^\\s+", "", id)
        id <- gsub("\\s+$", "", id)
        andi_stock_ids <- c(andi_stock_ids, id)
        
#        andi_stock_ids <- c(andi_stock_ids, paste(sp, stock))
    }
    
    web_stock_ids <- c()
    load("sr_data.Rdata")
    for(sr_data in my_db)
    {
        doc_text <- sr_data$documentation
        sp <- substr(doc_text[3], 13, nchar(doc_text[3]))
        sp <- gsub("^\\s+", "", sp)
        sp <- gsub("\\s+$", "", sp)
        
        stock <- substr(doc_text[7], 13, nchar(doc_text[7]))
        stock <- gsub("^\\s+", "", stock)
        stock <- gsub("\\s+$", "", stock)
        
        id <- substr(doc_text[1], 13, nchar(doc_text[1]))
        id <- gsub("^\\s+", "", id)
        id <- gsub("\\s+$", "", id)
        web_stock_ids <- c(web_stock_ids, id)
        
        #web_stock_ids <- c(web_stock_ids, paste(sp, stock))
    }
    
    save(web_stock_ids, andi_stock_ids, file = "stock_ids.Rdata")
    return()
}

run_univariate_analysis <- function()
{
    make_surrogate_ar0 <- function(ts, num_surr = 1000)
    {
        vals <- ts[is.finite(ts)]
        return(sapply(1:num_surr, function(i) {
            x <- sample(vals, length(ts), replace = TRUE)
            x[!is.finite(ts)] <- NA
            return(x)
        }))
    }
    
    make_surrogate_ar1 <- function(ts, ar_model, num_surr = 1000)
    {
        a <- ar_model$coef[1]
        b <- ar_model$coef[2]
        vals <- ts[is.finite(ts)]
        err_vals <- ar_model$residuals[is.finite(ar_model$residuals)]
        return(sapply(1:num_surr, function(i) {
            err <- sample(err_vals, length(ts), replace = TRUE)
            x <- rep.int(sample(vals, 1), length(ts)) - b
            for(i in 2:length(ts))
                x[i] <- x[i-1] * a + err[i]
            x[!is.finite(ts)] <- NA
            return(x + b)
        }))        
    }
    
    do_analysis <- function(rec)
    {
        rec <- (rec - mean(rec, na.rm = TRUE)) / sd(rec, na.rm = TRUE)
        
        # univariate EDM
        simplex_out <- simplex(rec, E = 1:4, silent = TRUE)
        best_E <- simplex_out$E[which.max(simplex_out$rho)]
        smap_out <- s_map(rec, E = best_E, silent = TRUE)
        
        # determine best order-0 or order-1 AR model
        ar_0 <- arima(rec, order = c(0,0,0))
        ar_1 <- arima(rec, order = c(1,0,0))
        ar_0$aicc <- ar_0$aic + 4/(ar_0$nobs - 2) # aicc = aic + 2k(k+1)/(n-k-1)
        ar_1$aicc <- ar_1$aic + 12/(ar_1$nobs - 3)
        
        # make surrogate data
        if(ar_0$aicc < ar_1$aicc)
        {
            surr_data <- make_surrogate_ar0(rec)
        } else {
            surr_data <- make_surrogate_ar1(rec, ar_1)
        }
        
        simplex_null <- do.call(rbind, apply(surr_data, 2, simplex, E = best_E, silent = TRUE))
        smap_null <- do.call(rbind, apply(surr_data, 2, function(ts) {
            temp <- s_map(ts, E = best_E, silent = TRUE)
            return(temp[which.max(temp$rho),])
        }))
        
        return(list(simplex_out = simplex_out,
                    smap_out = smap_out, 
                    best_E = best_E, 
                    ar_0 = ar_0, 
                    ar_1 = ar_1, 
                    simplex_null = simplex_null, 
                    smap_null = smap_null, 
                    rec = rec))
    }

    load("sr_data_10.Rdata")
    
    start_time <- proc.time()
    message("Starting processing...", appendLF = FALSE)
    sr_results <- mclapply(my_db, function(x) {do_analysis(x$data$rec)}, 
                           mc.cores = 8)
    message("done! (", round((proc.time() - start_time)[3], 3), " seconds elapsed)")
    save(sr_results, file = "sr_results.Rdata")
    
    return()
}

append_univariate_analysis <- function()
{
    add_ar1_analysis <- function(df)
    {
        rec <- df$rec
        
        # do EDM calculations
        for(tp in 2:4)
        {
            simplex_temp <- simplex(rec, E = 1:4, silent = TRUE, tp = tp)
            best_E <- simplex_temp$E[which.max(simplex_temp$rho)]
            smap_temp <- s_map(rec, E = best_E, silent = TRUE, tp = tp)
            
            df$simplex_out <- rbind(df$simplex_out, simplex_temp)
            df$smap_out <- rbind(df$smap_out, smap_temp)
        }
        
        # do AR calculations
        df$ar1_out <- s_map(rec, E = 1, theta = 0, silent = TRUE, tp = 1:4)
        
        return(df)
    }
    
    add_arE_analysis <- function(df)
    {
        rec <- df$rec
        arE_temp <- data.frame()
        # do EDM calculations
        for(tp in 1:4)
        {
            simplex_temp <- df$simplex_out[df$simplex_out$tp == tp,]
            best_E <- simplex_temp$E[which.max(simplex_temp$rho)]
            arE_temp <- rbind(arE_temp, s_map(rec, E = best_E, theta = 0, silent = TRUE, tp = tp))
        }
        df$arE_out <- arE_temp
        return(df)
    }
    
    load("sr_results.Rdata")
    test_df <- sr_results[[1]]
    
    start_time <- proc.time()
    message("Starting processing...", appendLF = FALSE)
    if(is.null(test_df$ar1_out))
    {
        sr_results <- mclapply(sr_results, function(df) {add_ar1_analysis(df)}, 
                           mc.cores = 8)
    }
    if(is.null(test_df$arE_out))
    {
        sr_results <- mclapply(sr_results, function(df) {add_arE_analysis(df)}, 
                               mc.cores = 8)
    }
    message("done! (", round((proc.time() - start_time)[3], 3), " seconds elapsed)")
    save(sr_results, file = "sr_results.Rdata")
    
    return() 
}