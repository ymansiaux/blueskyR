#' @noRd
write_json_atomic <- function(x, path, ...) {
  file_name <- tail(strsplit(path, "/|\\\\")[[1]], 1)
  dir_name <- substring(path, 1, nchar(path) - nchar(file_name) - 1)
  swap_file <- tempfile(
    pattern = paste("~", file_name, sep = ""),
    tmpdir = dir_name
  )
  jsonlite::write_json(x = x, path = swap_file, ...)
  file.rename(swap_file, path)
}


#' @noRd
msg <- function(m) {
  message(paste0(Sys.time(), " [INFO]: -------> ", m))
}


# Updating statistic files
# This function is called after each successful tweet request
# stat json files are stored on data_dir/year/xyz.gz where xyz is the name of a search gzip archive
# stat files contains an array per topic indicating the posted period on that the collected files with the same name
# these files are used to improve aggregating performance, by targeting only the files containing tweets for a particular date
# filename: gzip file to update stats
# topic: topic to update stats
# year: current year to separate the stat files per year
# first_date: oldest date on the tweets collected this will replace the stat if it is older than the current oldest date for the topic on the given file
# last_date: newest date on the tweets collected this will replace the stat if it is newer than the current newest date for the topic on the given file
update_file_stats <- function(
  filename,
  topic,
  year,
  first_date,
  last_date,
  conf
) {
  # getting the stat destination file
  stat_dir <- file.path(conf$data_dir, "stats")
  if (!file.exists(stat_dir)) {
    dir.create(stat_dir)
  }
  stat_dir <- file.path(stat_dir, year)
  if (!file.exists(stat_dir)) {
    dir.create(stat_dir)
  }
  dest <- file.path(stat_dir, filename)
  now <- Sys.time()
  #Setting UTC so it can be compares with twitter created dates
  attr(now, "tzone") <- "UTC"

  # reading current statistics if they exist
  stats <-
    if (!file.exists(dest)) {
      list()
    } else {
      jsonlite::read_json(dest, simplifyVector = FALSE, auto_unbox = TRUE)
    }

  # matching record or creating new one
  found <- FALSE
  # updating stat file if it is found
  if (length(stats) > 0) {
    for (i in 1:length(stats)) {
      if (stats[[i]]$topic == topic) {
        found <- TRUE
        stats[[i]]$collected_to <- now
        if (stats[[i]]$created_from > first_date) {
          stats[[i]]$created_from <- first_date
        }
        if (stats[[i]]$created_to < last_date) {
          stats[[i]]$created_to <- last_date
        }
        break
      }
    }
  }
  # creating new statistics if not found
  if (!found) {
    stats[[length(stats) + 1]] <- list(
      topic = topic,
      created_from = first_date,
      created_to = last_date,
      collected_from = now,
      collected_to = now
    )
  }

  # saving modified JSON file
  write_json_atomic(
    stats,
    dest,
    pretty = TRUE,
    force = TRUE,
    auto_unbox = TRUE
  )
}

# Helper function to parse Twitter date as provided by the Twitter API
parse_date <- function(str_date) {
  curLocale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", curLocale))
  Sys.setlocale("LC_TIME", "C")
  strptime(str_date, format = "%a %b %d %H:%M:%S +0000 %Y", tz = "UTC")
}

# @title get_plan S3 class constructor
# @description Create a new 'get plan' for importing tweets using the Search API
# @param expected_end Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the target end datetime of this plan
# @param scheduled_for Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the expected datetime for next execution, default: Sys.time()
# @param start_on Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the datetime when this plan was first executed, default: NULL
# @param end_on Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the datetime when this plan has finished, default: NULL
# @param max_id Integer(64), the newest tweet collected by this plan represented by its tweet id. This value is defined after the first successful request is done and does not change by request, default: NULL
# @param since_id Integer(64) the oldest tweet that has currently been collected by this plan, this value is updated after each request, default: NULL
# @param since_target Interger(64), the oldest tweet id that is expected to be obtained by this plan, this value is set as the max_id from the previous plan + 1, default: NULL
# @param results_span Number of minutes after which this plan expires counting from start date, default: 0
# @param requests Integer, number of requests successfully executed, default: 0
# @param progress Numeric, percentage of progress of current plan defined when since_target_id is known or when a request returns no more results, default: 0
# @return The get_plan object defined by input parameters
# @details A plan is an S3 class representing a commitment to download tweets from the search API
# It targets a specific time frame defined from the last tweet collected by the previous plan, if any, and the last tweet collected on its first request
# This commitment will be valid during a period of time defined from the time of its first execution until the end_on parameter
# a plan will perform several requests to the search API and each time a request is performed the number of requests will be increased.
# The field scheduled_for indicates the time when the next request is expected to be executed.
# @examples
# if(FALSE){
#  #creating the default plan
#  get_plan()
# }
# @seealso
#  \code{\link[bit64]{as.integer64.character}}
# @rdname get_plan
# @importFrom bit64 as.integer64
get_plan <- function(
  expected_end,
  scheduled_for = Sys.time(),
  start_on = NULL,
  end_on = NULL,
  max_id = NULL,
  since_id = NULL,
  since_target = NULL,
  results_span = 0,
  requests = 0,
  progress = 0.0
) {
  me <- list(
    "expected_end" = if (!is.null(unlist(expected_end))) {
      strptime(unlist(expected_end), "%Y-%m-%d %H:%M:%S")
    } else {
      NULL
    },
    "scheduled_for" = if (!is.null(unlist(scheduled_for))) {
      strptime(unlist(scheduled_for), "%Y-%m-%d %H:%M:%S")
    } else {
      NULL
    },
    "start_on" = if (!is.null(unlist(start_on))) {
      strptime(unlist(start_on), "%Y-%m-%d %H:%M:%S")
    } else {
      NULL
    },
    "end_on" = if (!is.null(unlist(end_on))) {
      strptime(unlist(end_on), "%Y-%m-%d %H:%M:%S")
    } else {
      NULL
    },
    "max_id" = if (!is.null(unlist(max_id))) {
      bit64::as.integer64(unlist(max_id))
    } else {
      NULL
    },
    "since_id" = if (!is.null(unlist(since_id))) {
      bit64::as.integer64(unlist(since_id))
    } else {
      NULL
    },
    "since_target" = if (!is.null(unlist(since_target))) {
      bit64::as.integer64(unlist(since_target))
    } else {
      NULL
    },
    "requests" = unlist(requests),
    "progress" = unlist(progress)
  )
  class(me) <- append(class(me), "get_plan")
  return(me)
}


# @title Update get plans
# @description Updating plans for a particular topic
# @param plans The existing plans for the topic, default: list()
# @param schedule_target target minutes for finishing a plan
# @return updated list of 'get_plan'
# @details
# This function will update the plan list of a topic taking in consideration the search span
# This function is called at the beginning of each search loop iteration applying the following rules
# If no plans are set, a new plan for getting all possible tweets will be set
# If current plan has started and the expected end has passed, a new plan will be added for collecting new tweets (previous plan will be stored for future execution if possible)
# Any finished plans after the first will be discharged. Note that after 7 days, all plans should be discharged because of empty results and as a measure of precaution, a maximum of 100 plans are kept)
# @returns the updated list of plans
# @examples
# if(FALSE){
#  #Getting default plan
#  update_plans(plans = list(), schedule_span = 120)
#  #Updating topics for first topic
#  update_plans(plans = conf$topics[[1]]$plan, schedule_span = conf$collect_span)
# }
# @rdname update_plans
update_plans <- function(plans = list(), schedule_span) {
  # Testing if there are plans present
  if (length(plans) == 0) {
    # Getting default plan for when no existing plans are present setting the expected end
    return(list(get_plan(expected_end = Sys.time() + 60 * schedule_span)))
  } else if (plans[[1]]$requests > 0 && plans[[1]]$expected_end < Sys.time()) {
    # creating a new plan if expected end has passed
    first <-
      get_plan(
        expected_end = if (
          Sys.time() > plans[[1]]$expected_end + 60 * schedule_span
        ) {
          Sys.time() + 60 * schedule_span
        } else {
          plans[[1]]$expected_end + 60 * schedule_span
        },
        since_target = plans[[1]]$max_id + 1
      )
    # removing ended plans
    non_ended <- plans[sapply(plans, function(x) is.null(x$end_on))]
    # removing plans if more of 100 plans are activeff
    return(append(
      list(first),
      if (length(non_ended) < 100) non_ended else non_ended[1:100]
    ))
  } else {
    first <- plans[[1]]
    rest <- plans[-1]
    # removing ended plans
    non_ended <- rest[unlist(sapply(rest, function(x) is.null(x$end_on)))]
    # removing ended plans
    return(append(
      list(first),
      if (length(non_ended) < 100) non_ended else non_ended[1:100]
    ))
  }
}

# Get next plan to plan to download
next_plan <- function(plans) {
  plans <- if ("get_plan" %in% class(plans)) list(plans) else plans
  non_ended <- plans[sapply(plans, function(x) is.null(x$end_on))]
  if (length(non_ended) == 0) {
    return(NULL)
  } else {
    return(non_ended[[1]])
  }
}

# finish the provided plans
finish_plans <- function(plans = list()) {
  # Testing if there are plans present
  if (length(plans) == 0) {
    list()
  } else {
    # creating a new plan if expected end has passed
    lapply(plans, function(p) {
      get_plan(
        expected_end = strftime(
          if (is.null(p$end_on)) {
            Sys.time() - conf$schedule_span * 60
          } else {
            p$end_on
          },
          "%Y-%m-%d %H:%M:%S"
        ),
        scheduled_for = strftime(p$scheduled_for, "%Y-%m-%d %H:%M:%S"),
        start_on = strftime(
          if (is.null(p$start_on)) {
            Sys.time() - conf$schedule_span * 60
          } else {
            p$start_on
          },
          "%Y-%m-%d %H:%M:%S"
        ),
        end_on = strftime(
          if (is.null(p$end_on)) {
            Sys.time() - conf$schedule_span * 60
          } else {
            p$end_on
          },
          "%Y-%m-%d %H:%M:%S"
        ),
        max_id = p$max_id,
        since_id = p$since_target,
        since_target = p$since_target,
        requests = p$requests,
        progress = 1.0
      )
    })
  }
}

# Calculating how long in seconds should epitweetr wait before executing one of the plans in the list which would be the case only if all plans are finished before the end of the search span
can_wait_for <- function(plans) {
  plans <- if ("get_plan" %in% class(plans)) list(plans) else plans
  non_ended <- plans[sapply(plans, function(x) is.null(x$end_on))]
  if (length(non_ended) == 0) {
    expected_end <- Reduce(min, lapply(plans, function(x) x$expected_end))
    return(ceiling(as.numeric(difftime(
      expected_end,
      Sys.time(),
      units = "secs"
    ))))
  } else {
    return(0)
  }
}

# Last search time on stored in the embedded database
last_search_time <- function() {
  last_fs_updates(c("tweets"))$tweets
}

# create topic directories if they do not exist
create_dirs <- function(topic = NA, year = NA, conf) {
  #, conf
  if (!file.exists(paste(conf$data_dir, sep = "/"))) {
    dir.create(paste(conf$data_dir, sep = "/"), showWarnings = FALSE)
  }
  if (!file.exists(paste(conf$data_dir, "tweets", sep = "/"))) {
    dir.create(
      paste(conf$data_dir, "tweets", sep = "/"),
      showWarnings = FALSE
    )
  }
  if (!file.exists(paste(conf$data_dir, "tweets", "search", sep = "/"))) {
    dir.create(
      paste(conf$data_dir, "tweets", "search", sep = "/"),
      showWarnings = FALSE
    )
  }
  if (!is.na(topic) && !is.na(year)) {
    if (
      !file.exists(paste(
        conf$data_dir,
        "tweets",
        "search",
        topic,
        sep = "/"
      ))
    ) {
      dir.create(
        paste(conf$data_dir, "tweets", "search", topic, sep = "/"),
        showWarnings = FALSE
      )
    }
    if (
      !file.exists(paste(
        conf$data_dir,
        "tweets",
        "search",
        topic,
        year,
        sep = "/"
      ))
    ) {
      dir.create(
        paste(
          conf$data_dir,
          "tweets",
          "search",
          topic,
          year,
          sep = "/"
        ),
        showWarnings = FALSE
      )
    }
  }
}
