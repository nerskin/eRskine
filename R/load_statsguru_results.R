#' Load Cricinfo Statsguru Results Tables into R
#'
#' This function takes a statsguru results page url and returns every page of results as a single data frame.
#' @param url The url of the results page
#' @export

load_statsguru_results <- function(url,pause=0.1) {
    finished <- FALSE
    results <- NULL
    page <- 1
    
    while (!finished) {
        page_url <- paste0(url, ";page=", page)
        success <- class(try(html_raw <- xml2::read_html(page_url)))!='try-error'
	success <- success[1]#make warning go away
	if (!success){
		print('Failure to dowload webpage. Sleeping ten seconds before trying again')
		Sys.sleep(10)
		next()
	}
        new_data <- rvest::html_table(html_raw, fill = TRUE)[[3]]
        new_data <- new_data[, names(new_data) != ""]
        if (new_data[[1]][[1]] == "No records available to match this query") {
            finished <- TRUE
        } else {
            results <- rbind(results, new_data)
        }
        page <- page + 1
        Sys.sleep(pause)
    }
    return(results)
}

