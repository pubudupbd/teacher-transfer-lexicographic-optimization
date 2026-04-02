evaluate_solution <- function(solution_obj,
                              solution_name = "Solution",
                              scenario = NA_character_) {
  teachers <- solution_obj$model_data$teachers
  schools <- solution_obj$model_data$schools
  req <- solution_obj$model_data$req
  assignments <- solution_obj$assignments
  shortages <- solution_obj$shortages

  initial_counts <- aggregate(
    teacher_id ~ current_school + subject + medium,
    data = transform(teachers, teacher_id = 1L),
    FUN = sum
  )
  names(initial_counts) <- c("school_id", "subject", "medium", "initial_count")

  init_short <- merge(req, initial_counts, by = c("school_id", "subject", "medium"), all.x = TRUE, sort = FALSE)
  init_short$initial_count[is.na(init_short$initial_count)] <- 0L
  init_short$initial_shortage <- pmax(0, init_short$L_jsm - init_short$initial_count)

  final_short <- merge(
    req[, c("school_id", "subject", "medium", "L_jsm")],
    shortages[, c("school_id", "subject", "medium", "z_jsm")],
    by = c("school_id", "subject", "medium"),
    all.x = TRUE,
    sort = FALSE
  )
  final_short$z_jsm[is.na(final_short$z_jsm)] <- 0

  initial_total_shortage <- sum(init_short$initial_shortage)
  final_total_shortage <- sum(final_short$z_jsm)

  shortage_reduction_rate <- if (initial_total_shortage > 0) {
    (initial_total_shortage - final_total_shortage) / initial_total_shortage
  } else {
    NA_real_
  }

  difficult_ids <- schools$school_id[schools$underserved == 1L]
  difficult_short <- final_short[final_short$school_id %in% difficult_ids, , drop = FALSE]

  difficult_coverage_rate <- if (length(difficult_ids) > 0L && nrow(difficult_short) > 0L) {
    mean(difficult_short$z_jsm == 0)
  } else {
    NA_real_
  }

  n_understaffed_school_types <- sum(final_short$z_jsm > 0)
  avg_welfare <- mean(assignments$welfare_raw, na.rm = TRUE)
  avg_burden <- mean(assignments$burden_raw, na.rm = TRUE)

  assignments$moved_flag <- as.integer(assignments$assigned_school != assignments$current_school)
  n_moved <- sum(assignments$moved_flag, na.rm = TRUE)

  compulsory_df <- merge(
    teachers[, c("teacher_id", "current_school", "compulsory_transfer")],
    assignments[, c("teacher_id", "assigned_school", "moved_flag")],
    by = "teacher_id",
    all.x = TRUE,
    sort = FALSE
  )
  compulsory_cases <- compulsory_df[compulsory_df$compulsory_transfer == 1L, , drop = FALSE]

  compulsory_compliance <- if (nrow(compulsory_cases) > 0) {
    mean(compulsory_cases$assigned_school != compulsory_cases$current_school)
  } else {
    NA_real_
  }

  compulsory_moved <- if (nrow(compulsory_cases) > 0) {
    sum(compulsory_cases$moved_flag, na.rm = TRUE)
  } else {
    0L
  }

  voluntary_moved <- sum(
    compulsory_df$moved_flag[compulsory_df$compulsory_transfer == 0L],
    na.rm = TRUE
  )
  movement_rate <- n_moved / nrow(assignments)

  data.frame(
    scenario = scenario,
    solution = solution_name,
    initial_total_shortage = initial_total_shortage,
    final_total_shortage = final_total_shortage,
    shortage_reduction_rate = shortage_reduction_rate,
    understaffed_school_subject_medium_cells = n_understaffed_school_types,
    difficult_school_full_coverage_rate = difficult_coverage_rate,
    average_welfare = avg_welfare,
    average_transfer_burden = avg_burden,
    number_of_teachers_moved = n_moved,
    movement_rate = movement_rate,
    compulsory_teachers_moved = compulsory_moved,
    voluntary_teachers_moved = voluntary_moved,
    compulsory_transfer_compliance = compulsory_compliance,
    stringsAsFactors = FALSE
  )
}

make_school_staffing_summary <- function(solution_obj) {
  schools <- solution_obj$model_data$schools
  req <- solution_obj$model_data$req
  shortages <- solution_obj$shortages

  out <- merge(
    req[, c("school_id", "subject", "medium", "L_jsm", "U_jsm")],
    shortages[, c("school_id", "subject", "medium", "z_jsm")],
    by = c("school_id", "subject", "medium"),
    all.x = TRUE,
    sort = FALSE
  )
  out$z_jsm[is.na(out$z_jsm)] <- 0
  out$covered <- ifelse(out$z_jsm == 0, "Yes", "No")

  out <- merge(
    out,
    schools[, c("school_id", "school_name", "w_j", "underserved")],
    by = "school_id",
    all.x = TRUE,
    sort = FALSE
  )
  out[order(out$school_id, out$subject, out$medium), ]
}

make_transfer_summary <- function(solution_obj) {
  assignments <- solution_obj$assignments
  assignments$moved <- ifelse(
    assignments$current_school != assignments$assigned_school,
    "Moved",
    "Stayed"
  )

  assignments[order(assignments$teacher_id), c(
    "teacher_id", "teacher_name", "subject", "medium",
    "current_school", "assigned_school", "assigned_school_name",
    "moved", "welfare_raw", "burden_raw"
  )]
}
