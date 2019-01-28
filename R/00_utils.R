calc_mack_estimates <- function(x) {
    tridata <- split_triangle(x)
    
    mack_estimate <- ChainLadder::MackChainLadder(tridata$use_triangle)
    
    ldfs <- rep(1, nrow(tridata$final_vals))
    ldfs[seq_along(mack_estimate$f)] <- rev(cumprod(rev(mack_estimate$f)))
    sds <- rep(NaN, nrow(tridata$final_vals)) 
    max_col <- ncol(mack_estimate$Mack.S.E)
    sds[seq_along(mack_estimate$f)] <- rev(mack_estimate$Mack.S.E[, max_col])
    
    results_tbl <- tibble::tibble(
        lag = seq_along(ldfs),
        devfactor = ldfs,
        sd = sds
    )
    
    
    summary_tbl <- dplyr::filter(tridata$known_losses, as.numeric(ay) + lag == 1998) %>%
        inner_join(results_tbl, by = "lag") %>%
        inner_join(tridata$final_vals, by = "ay") %>%
        mutate(mack_est = incloss.x * devfactor, 
               cv = sd / mack_est, ay = as.character(ay)) %>%
        select(ay, mack_est, sd, cv, actual = incloss.y)
    
    total_row <- dplyr::summarize(summary_tbl, 
                                  mack_est = sum(mack_est), 
                                  actual = sum(actual))
   
 
    total_row <- dplyr::mutate(total_row, ay = "total", 
                      sd = mack_estimate$Total.Mack.S.E, 
                      cv = sd / mack_est) %>%
        dplyr::select(ay, mack_est, sd, cv, actual)
    
    summary_tbl <- dplyr::bind_rows(summary_tbl, total_row)
    
    
    dist_cv <- total_row$sd /total_row$mack_est
    dist_mean <- total_row$mack_est
    useful_val <- dist_cv^2 + 1
    
    sd_log <- sqrt(log(useful_val))
    mean_log <- log(dist_mean) - 0.5 * log(useful_val)
    
    percentile <- plnorm(total_row$actual, meanlog = mean_log, sdlog = sd_log)
    tibble::tibble(mack_estimate = list(mack_estimate), 
                   summary = list(summary_tbl),
                   percentile = percentile)
}

split_triangle <- function(x) {
    #following script example from Meyers, I'm backing out BulkLoss and making sure
    # values are positive
    x <- dplyr::mutate(x, incloss = pmax(IncurLoss_C - BulkLoss_C, 1)) %>%
        dplyr::select(ay = AccidentYear, 
                      lag = DevelopmentLag, 
                      incloss)
    
    known_losses <- dplyr::filter(x, as.numeric(ay) + lag <= 1998)
    zero_known_loss_years <- known_losses %>% 
        group_by(ay) %>% 
        summarise(tot = sum(incloss)) %>% 
        filter(tot == 0)
    nonzero_known_losses <- dplyr::anti_join(known_losses, zero_known_loss_years, by = "ay") 
    
    
    list(
        known_losses = known_losses,
        use_triangle = ChainLadder::as.triangle(nonzero_known_losses,
                                             origin = "ay" , 
                                             dev = "lag", 
                                             value = "incloss"),
    
        final_vals = dplyr::filter(x, lag == 10) %>% select(ay, incloss)
    )
}