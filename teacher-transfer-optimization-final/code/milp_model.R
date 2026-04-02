build_milp_matrices <- function(input_obj,
                                add_stage1a_optimality = NULL,
                                add_stage1b_optimality = NULL,
                                add_stage2_optimality = NULL,
                                add_stage3_optimality = NULL) {
  teachers <- input_obj$teachers
  schools <- input_obj$schools
  req <- input_obj$req
  assign_df <- input_obj$assign_df

  nX <- nrow(assign_df)
  nZ <- nrow(req)
  total_vars <- nX + nZ

  assign_df$x_index <- seq_len(nX)
  req$z_index <- nX + seq_len(nZ)

  objective <- rep(0, total_vars)
  const_mat <- list()
  const_dir <- character()
  const_rhs <- numeric()

  add_constraint <- function(coefs, dir, rhs) {
    const_mat[[length(const_mat) + 1L]] <<- coefs
    const_dir <<- c(const_dir, dir)
    const_rhs <<- c(const_rhs, rhs)
  }

  for (i in teachers$teacher_id) {
    row <- numeric(total_vars)
    idx <- assign_df$x_index[assign_df$teacher_id == i]
    row[idx] <- 1
    add_constraint(row, "=", 1)
  }

  for (k in seq_len(nX)) {
    row <- numeric(total_vars)
    row[assign_df$x_index[k]] <- 1
    add_constraint(row, "<=", assign_df$c_ij[k])
    add_constraint(row, "<=", assign_df$r_ij[k])
    add_constraint(row, "<=", assign_df$compat[k])
  }

  for (i in seq_len(nrow(teachers))) {
    teacher_id <- teachers$teacher_id[i]
    curr_school <- teachers$current_school[i]
    g_i <- teachers$compulsory_transfer[i]

    idx <- assign_df$x_index[
      assign_df$teacher_id == teacher_id &
        assign_df$school_id == curr_school
    ]

    if (length(idx) != 1L) {
      stop(sprintf("Could not uniquely identify x_(%d,%d).", teacher_id, curr_school))
    }

    row <- numeric(total_vars)
    row[idx] <- 1
    add_constraint(row, "<=", 1 - g_i)
  }

  for (r in seq_len(nrow(req))) {
    row <- numeric(total_vars)
    idx_x <- assign_df$x_index[
      assign_df$school_id == req$school_id[r] &
        assign_df$subject == req$subject[r] &
        assign_df$medium == req$medium[r]
    ]
    row[idx_x] <- 1
    row[req$z_index[r]] <- 1
    add_constraint(row, ">=", req$L_jsm[r])
  }

  for (r in seq_len(nrow(req))) {
    row <- numeric(total_vars)
    idx_x <- assign_df$x_index[
      assign_df$school_id == req$school_id[r] &
        assign_df$subject == req$subject[r] &
        assign_df$medium == req$medium[r]
    ]
    row[idx_x] <- 1
    add_constraint(row, "<=", req$U_jsm[r])
  }

  if (!is.null(add_stage1a_optimality)) {
    row <- numeric(total_vars)
    row[req$z_index] <- 1
    add_constraint(row, "=", add_stage1a_optimality)
  }

  if (!is.null(add_stage1b_optimality)) {
    row <- numeric(total_vars)
    row[req$z_index] <- schools$w_j[match(req$school_id, schools$school_id)]
    add_constraint(row, "=", add_stage1b_optimality)
  }

  if (!is.null(add_stage2_optimality)) {
    row <- numeric(total_vars)
    row[assign_df$x_index] <- assign_df$move_ij
    add_constraint(row, "=", add_stage2_optimality)
  }

  if (!is.null(add_stage3_optimality)) {
    row <- numeric(total_vars)
    row[assign_df$x_index] <- assign_df$p_ij
    add_constraint(row, "=", add_stage3_optimality)
  }

  const_mat <- do.call(rbind, const_mat)

  list(
    objective = objective,
    const_mat = const_mat,
    const_dir = const_dir,
    const_rhs = const_rhs,
    binary_vec = assign_df$x_index,
    nX = nX,
    nZ = nZ,
    total_vars = total_vars,
    assign_df = assign_df,
    req = req,
    schools = schools,
    teachers = teachers
  )
}
