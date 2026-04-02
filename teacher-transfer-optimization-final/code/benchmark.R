run_benchmark_administrative_procedure <- function(input_obj, verbose = TRUE) {
  teachers <- input_obj$teachers
  schools <- input_obj$schools
  req <- input_obj$req
  assign_df <- input_obj$assign_df

  current_assign <- teachers$current_school
  names(current_assign) <- teachers$teacher_id

  compute_staffing_counts <- function(current_assign_vec) {
    out <- merge(
      expand.grid(
        school_id = schools$school_id,
        subject = unique(teachers$subject),
        medium = unique(teachers$medium),
        stringsAsFactors = FALSE
      ),
      data.frame(
        teacher_id = teachers$teacher_id,
        school_id = as.integer(current_assign_vec),
        subject = teachers$subject,
        medium = teachers$medium,
        stringsAsFactors = FALSE
      ),
      by = c("school_id", "subject", "medium"),
      all.x = TRUE
    )

    counts <- aggregate(
      teacher_id ~ school_id + subject + medium,
      data = out,
      FUN = function(x) sum(!is.na(x))
    )
    names(counts)[4] <- "count"
    counts
  }

  teachers$priority_group <- ifelse(
    teachers$compulsory_transfer == 1L, 1L,
    ifelse(teachers$hardship == 1L, 2L, 3L)
  )
  teachers$priority_order_score <- -teachers$years_service
  teachers_ordered <- teachers[order(teachers$priority_group, teachers$priority_order_score), ]

  forced_compulsory <- integer(0)

  for (row_i in seq_len(nrow(teachers_ordered))) {
    i <- teachers_ordered$teacher_id[row_i]
    subj <- teachers_ordered$subject[row_i]
    med <- teachers_ordered$medium[row_i]
    curr <- current_assign[as.character(i)]
    compulsory <- teachers_ordered$compulsory_transfer[row_i]

    staffing <- compute_staffing_counts(current_assign)
    req2 <- merge(req, staffing, by = c("school_id", "subject", "medium"), all.x = TRUE, sort = FALSE)
    req2$count[is.na(req2$count)] <- 0L
    req2$remaining_shortage <- pmax(0, req2$L_jsm - req2$count)

    feasible <- assign_df[
      assign_df$teacher_id == i &
        assign_df$c_ij == 1L &
        assign_df$r_ij == 1L &
        assign_df$compat == 1L,
      ,
      drop = FALSE
    ]

    if (compulsory == 1L) {
      feasible <- feasible[feasible$school_id != curr, , drop = FALSE]
    }

    req_sub <- req2[
      req2$subject == subj & req2$medium == med,
      c("school_id", "count", "remaining_shortage", "L_jsm", "U_jsm")
    ]
    feasible <- merge(feasible, req_sub, by = "school_id", all.x = TRUE, sort = FALSE)

    feasible_pref <- feasible[feasible$count < feasible$U_jsm | is.na(feasible$count), , drop = FALSE]

    if (compulsory == 1L && nrow(feasible_pref) == 0L && nrow(feasible) > 0L) {
      feasible_pref <- feasible
      forced_compulsory <- c(forced_compulsory, i)
    }

    if (nrow(feasible_pref) == 0L) next

    feasible_pref$need_positive <- ifelse(feasible_pref$remaining_shortage > 0, 1L, 0L)
    feasible_pref$stay_bonus <- ifelse(feasible_pref$school_id == curr, 1L, 0L)

    feasible_pref <- feasible_pref[
      order(
        -feasible_pref$need_positive,
        -feasible_pref$w_j,
        feasible_pref$d_ij,
        -feasible_pref$p_ij,
        -feasible_pref$stay_bonus
      ),
      ,
      drop = FALSE
    ]

    best_school <- feasible_pref$school_id[1]

    if (compulsory == 0L) {
      stay_row <- assign_df[
        assign_df$teacher_id == i & assign_df$school_id == curr,
        ,
        drop = FALSE
      ]
      stay_req <- req_sub[req_sub$school_id == curr, , drop = FALSE]
      if (nrow(stay_req) == 0L) {
        stay_req <- data.frame(
          school_id = curr,
          count = NA_integer_,
          remaining_shortage = 0L,
          L_jsm = 0L,
          U_jsm = Inf
        )
      }

      move_row <- feasible_pref[1, , drop = FALSE]

      stay_score <- c(
        as.numeric(stay_req$remaining_shortage[1] > 0),
        schools$w_j[match(curr, schools$school_id)],
        -stay_row$d_ij[1],
        stay_row$p_ij[1]
      )
      move_score <- c(
        as.numeric(move_row$remaining_shortage[1] > 0),
        schools$w_j[match(best_school, schools$school_id)],
        -move_row$d_ij[1],
        move_row$p_ij[1]
      )

      if (!(all(move_score >= stay_score) && any(move_score > stay_score))) next
    }

    current_assign[as.character(i)] <- best_school
  }

  benchmark_assign <- data.frame(
    teacher_id = teachers$teacher_id,
    teacher_name = teachers$teacher_name,
    subject = teachers$subject,
    medium = teachers$medium,
    current_school = teachers$current_school,
    compulsory_transfer = teachers$compulsory_transfer,
    assigned_school = as.integer(current_assign),
    stringsAsFactors = FALSE
  )

  benchmark_assign <- merge(
    benchmark_assign,
    schools[, c("school_id", "school_name")],
    by.x = "assigned_school",
    by.y = "school_id",
    all.x = TRUE,
    sort = FALSE
  )
  names(benchmark_assign)[names(benchmark_assign) == "school_name"] <- "assigned_school_name"

  benchmark_assign <- merge(
    benchmark_assign,
    assign_df[, c("teacher_id", "school_id", "p_ij", "d_ij", "burden_raw", "welfare_raw")],
    by.x = c("teacher_id", "assigned_school"),
    by.y = c("teacher_id", "school_id"),
    all.x = TRUE,
    sort = FALSE
  )

  final_counts <- aggregate(
    teacher_id ~ assigned_school + subject + medium,
    data = benchmark_assign,
    FUN = length
  )
  names(final_counts) <- c("school_id", "subject", "medium", "final_count")

  final_short <- merge(
    req,
    final_counts,
    by = c("school_id", "subject", "medium"),
    all.x = TRUE,
    sort = FALSE
  )
  final_short$final_count[is.na(final_short$final_count)] <- 0L
  final_short$z_jsm <- pmax(0, final_short$L_jsm - final_short$final_count)

  if (verbose && length(forced_compulsory) > 0L) {
    cat(
      "Benchmark fallback used for compulsory-transfer teachers:",
      paste(unique(forced_compulsory), collapse = ", "),
      "\n"
    )
  }

  list(
    assignments = benchmark_assign[order(benchmark_assign$teacher_id), ],
    shortages = final_short,
    forced_compulsory = unique(forced_compulsory),
    model_data = input_obj
  )
}
