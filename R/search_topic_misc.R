set_date_boundaries <- function(plan) {
    # We set the upper bound of the research: if missing we set it to the current time
    max_text <- plan$research_max_date
    if (is.null(plan$research_max_date)) {
        plan$research_max_date <- Sys.time()
        max_text <- "last message"
    }

    # We set the lower bound of the research
    # If messages were retrived before, we set it to the newest message retrieved by the previous query
    if (is.null(plan$research_min_date)) {
        min_text <- "first message"
    }
    if (!is.null(plan$boundaries_date_max)) {
        plan$research_min_date <- plan$boundaries_date_max
        min_text <- plan$research_min_date

        if (plan$research_max_date <= plan$research_min_date) {
            stop("We already have all the data we need")
        }
    }

    list(plan = plan, max_text = max_text, min_text = min_text)
}


update_plan_boundaries <- function(plan, content) {
    # If a request retrieved no messages, we stop the research
    # if (is.null(content$newest_message_in_a_query)) {
    #     plan$research_max_date <- NA
    #     plan$research_min_date <- NA
    #     plan$boundaries_date_min <- NA
    #     plan$boundaries_date_max <- NA
    #     plan$has_more <- FALSE
    #     return(plan)
    # }

    plan$newest_messages <- c(
        plan$newest_messages,
        content$newest_message_in_a_query
    )

    # If there are more messages to retrieve, we update the research max date
    if (content$has_more) {
        plan$research_max_date <- content$oldest_message_in_a_query
        # plan$research_min_date <- NULL
        plan$boundaries_date_min <- min(c(
            plan$boundaries_date_min,
            content$oldest_message_in_a_query
        ))
    }

    # If there are no more messages to retrieve, we set the boundaries_date_max to the newest message retrieved in the entire research
    if (!content$has_more) {
        plan$boundaries_date_max <- max(c(
            plan$boundaries_date_max,
            plan$newest_messages
        ))
        plan$boundaries_date_min <- min(c(
            plan$boundaries_date_min,
            content$oldest_message_in_a_query
        ))
        plan$research_min_date <- NULL
        plan$research_max_date <- NULL
        plan$newest_messages <- NULL
    }

    plan$has_more <- content$has_more
    if (!is.null(plan$boundaries_date_max)) {
        plan$boundaries_date_max <- as.POSIXct(plan$boundaries_date_max)
    }
    if (!is.null(plan$boundaries_date_min)) {
        plan$boundaries_date_min <- as.POSIXct(plan$boundaries_date_min)
    }
    if (!is.null(plan$research_max_date)) {
        plan$research_max_date <- as.POSIXct(plan$research_max_date)
    }
    if (!is.null(plan$research_min_date)) {
        plan$research_min_date <- as.POSIXct(plan$research_min_date)
    }
    if (!is.null(plan$newest_messages)) {
        plan$newest_messages <- as.POSIXct(plan$newest_messages)
    }
    return(plan)
}
