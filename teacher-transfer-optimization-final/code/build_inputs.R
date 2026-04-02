build_assignment_inputs <- function(data_obj, admin_seed = 999L) {
  teachers <- data_obj$teachers
  schools <- data_obj$schools
  req <- data_obj$school_type_requirements

  check_required_columns(
    teachers,
    c(
      "teacher_id", "subject", "medium", "current_school", "hardship",
      "compulsory_transfer", "home_x", "home_y", "max_burden",
      "pref_1", "pref_2", "pref_3"
    ),
    "teachers"
  )
  check_required_columns(
    schools,
    c("school_id", "coord_x", "coord_y", "w_j", "underserved"),
    "schools"
  )
  check_required_columns(
    req,
    c("school_id", "subject", "medium", "type_key", "L_jsm", "U_jsm"),
    "school_type_requirements"
  )

  assign_df <- merge(
    teachers[, c(
      "teacher_id", "teacher_name", "subject", "medium",
      "current_school", "hardship", "compulsory_transfer",
      "home_x", "home_y", "max_burden",
      "pref_1", "pref_2", "pref_3"
    )],
    schools[, c(
      "school_id", "school_name", "category", "underserved", "w_j",
      "coord_x", "coord_y"
    )],
    by = NULL
  )

  assign_df$type_key <- make_type_key(assign_df$subject, assign_df$medium)

  assign_df <- merge(
    assign_df,
    req[, c("school_id", "type_key", "U_jsm", "L_jsm")],
    by = c("school_id", "type_key"),
    all.x = TRUE,
    sort = FALSE
  )

  same_school_rows <- assign_df$school_id == assign_df$current_school
  assign_df$U_jsm[same_school_rows & is.na(assign_df$U_jsm)] <- 1L
  assign_df$L_jsm[same_school_rows & is.na(assign_df$L_jsm)] <- 0L
  assign_df$U_jsm[same_school_rows & assign_df$U_jsm < 1L] <- 1L

  assign_df$compat <- ifelse(!is.na(assign_df$U_jsm) & assign_df$U_jsm > 0L, 1L, 0L)

  assign_df$burden_raw <- euclidean_distance(
    assign_df$home_x, assign_df$home_y,
    assign_df$coord_x, assign_df$coord_y
  )
  assign_df$move_ij <- movement_indicator(assign_df$school_id, assign_df$current_school)

  assign_df$pi_ij <- ifelse(
    assign_df$school_id == assign_df$pref_1, 1.00,
    ifelse(
      assign_df$school_id == assign_df$pref_2, 0.70,
      ifelse(assign_df$school_id == assign_df$pref_3, 0.40, 0.10)
    )
  )

  max_b <- max(assign_df$burden_raw)
  assign_df$delta_ij <- ifelse(max_b > 0, assign_df$burden_raw / max_b, 0)

  assign_df$rho_ij <- ifelse(
    assign_df$hardship == 1L,
    pmax(0, 1 - assign_df$delta_ij) + ifelse(assign_df$underserved == 1L, 0, 0.30),
    0
  )

  phi1 <- 5.0
  phi2 <- 3.0
  phi3 <- 4.0
  assign_df$welfare_raw <- phi1 * assign_df$pi_ij -
    phi2 * assign_df$delta_ij +
    phi3 * assign_df$rho_ij

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv)
  if (old_seed_exists) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(admin_seed)
  admin_random <- rbinom(nrow(assign_df), size = 1, prob = 0.90)
  if (old_seed_exists) {
    assign(".Random.seed", old_seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  assign_df$c_ij <- ifelse(assign_df$school_id == assign_df$current_school, 1L, admin_random)
  assign_df$c_ij <- ifelse(assign_df$compat == 1L & assign_df$c_ij == 1L, 1L, 0L)

  assign_df$r_ij <- ifelse(
    assign_df$hardship == 1L,
    ifelse(assign_df$burden_raw <= assign_df$max_burden, 1L, 0L),
    1L
  )

  same_school <- assign_df$school_id == assign_df$current_school
  assign_df$c_ij[same_school & assign_df$compat[same_school] == 1L] <- 1L
  assign_df$r_ij[same_school & assign_df$compat[same_school] == 1L] <- 1L

  assign_df$p_ij <- safe_scale_to_int(assign_df$welfare_raw, 100L)
  assign_df$d_ij <- safe_scale_to_int(assign_df$burden_raw, 100L)

  assign_df$feasible_basic <- with(assign_df, c_ij == 1L & r_ij == 1L & compat == 1L)

  feasible_count <- aggregate(feasible_basic ~ teacher_id, data = assign_df, FUN = sum)
  bad_teachers <- feasible_count$teacher_id[feasible_count$feasible_basic == 0L]

  if (length(bad_teachers) > 0L) {
    for (tid in bad_teachers) {
      idx <- which(assign_df$teacher_id == tid & assign_df$school_id == assign_df$current_school)
      if (length(idx) == 1L) {
        assign_df$U_jsm[idx] <- max(1L, assign_df$U_jsm[idx], na.rm = TRUE)
        assign_df$compat[idx] <- 1L
        assign_df$c_ij[idx] <- 1L
        assign_df$r_ij[idx] <- 1L
        assign_df$feasible_basic[idx] <- TRUE
      }
    }
  }

  feasible_count <- aggregate(feasible_basic ~ teacher_id, data = assign_df, FUN = sum)
  if (any(feasible_count$feasible_basic == 0L)) {
    bad_teachers <- feasible_count$teacher_id[feasible_count$feasible_basic == 0L]
    stop(sprintf(
      "At least one teacher still has no feasible school after safeguard. Teacher IDs: %s",
      paste(bad_teachers, collapse = ", ")
    ))
  }

  list(
    teachers = teachers,
    schools = schools,
    req = req,
    assign_df = assign_df
  )
}
