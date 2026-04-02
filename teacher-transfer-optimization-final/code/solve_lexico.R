solve_teacher_transfer_lexicographic <- function(input_obj, verbose = TRUE) {
  model1a <- build_milp_matrices(input_obj)
  model1a$objective[model1a$nX + seq_len(model1a$nZ)] <- 1

  if (verbose) cat("Solving Stage 1A: Minimize total shortage...\n")
  stage1a <- lpSolve::lp(
    direction = "min",
    objective.in = model1a$objective,
    const.mat = model1a$const_mat,
    const.dir = model1a$const_dir,
    const.rhs = model1a$const_rhs,
    all.bin = FALSE,
    binary.vec = model1a$binary_vec,
    compute.sens = FALSE
  )
  if (stage1a$status != 0) {
    stop(sprintf("Stage 1A optimization failed. lp status code: %d", stage1a$status))
  }
  Z1a_star <- as.numeric(round(stage1a$objval))

  model1b <- build_milp_matrices(input_obj, add_stage1a_optimality = Z1a_star)
  model1b$objective[model1b$nX + seq_len(model1b$nZ)] <-
    model1b$schools$w_j[match(model1b$req$school_id, model1b$schools$school_id)]

  if (verbose) cat("Solving Stage 1B: Minimize weighted shortage...\n")
  stage1b <- lpSolve::lp(
    direction = "min",
    objective.in = model1b$objective,
    const.mat = model1b$const_mat,
    const.dir = model1b$const_dir,
    const.rhs = model1b$const_rhs,
    all.bin = FALSE,
    binary.vec = model1b$binary_vec,
    compute.sens = FALSE
  )
  if (stage1b$status != 0) {
    stop(sprintf("Stage 1B optimization failed. lp status code: %d", stage1b$status))
  }
  Z1b_star <- as.numeric(round(stage1b$objval))

  model2 <- build_milp_matrices(
    input_obj,
    add_stage1a_optimality = Z1a_star,
    add_stage1b_optimality = Z1b_star
  )
  model2$objective[model2$assign_df$x_index] <- model2$assign_df$move_ij

  if (verbose) cat("Solving Stage 2: Minimize number of transfers...\n")
  stage2 <- lpSolve::lp(
    direction = "min",
    objective.in = model2$objective,
    const.mat = model2$const_mat,
    const.dir = model2$const_dir,
    const.rhs = model2$const_rhs,
    all.bin = FALSE,
    binary.vec = model2$binary_vec,
    compute.sens = FALSE
  )
  if (stage2$status != 0) {
    stop(sprintf("Stage 2 optimization failed. lp status code: %d", stage2$status))
  }
  Z2_star <- as.numeric(round(stage2$objval))

  model3 <- build_milp_matrices(
    input_obj,
    add_stage1a_optimality = Z1a_star,
    add_stage1b_optimality = Z1b_star,
    add_stage2_optimality = Z2_star
  )
  model3$objective[model3$assign_df$x_index] <- model3$assign_df$p_ij

  if (verbose) cat("Solving Stage 3: Maximize total teacher welfare...\n")
  stage3 <- lpSolve::lp(
    direction = "max",
    objective.in = model3$objective,
    const.mat = model3$const_mat,
    const.dir = model3$const_dir,
    const.rhs = model3$const_rhs,
    all.bin = FALSE,
    binary.vec = model3$binary_vec,
    compute.sens = FALSE
  )
  if (stage3$status != 0) {
    stop(sprintf("Stage 3 optimization failed. lp status code: %d", stage3$status))
  }
  Z3_star <- as.numeric(round(stage3$objval))

  model4 <- build_milp_matrices(
    input_obj,
    add_stage1a_optimality = Z1a_star,
    add_stage1b_optimality = Z1b_star,
    add_stage2_optimality = Z2_star,
    add_stage3_optimality = Z3_star
  )
  model4$objective[model4$assign_df$x_index] <- model4$assign_df$d_ij

  if (verbose) cat("Solving Stage 4: Minimize transfer burden...\n")
  stage4 <- lpSolve::lp(
    direction = "min",
    objective.in = model4$objective,
    const.mat = model4$const_mat,
    const.dir = model4$const_dir,
    const.rhs = model4$const_rhs,
    all.bin = FALSE,
    binary.vec = model4$binary_vec,
    compute.sens = FALSE
  )
  if (stage4$status != 0) {
    stop(sprintf("Stage 4 optimization failed. lp status code: %d", stage4$status))
  }

  sol <- stage4$solution
  x_sol <- sol[model4$assign_df$x_index]
  z_sol <- sol[model4$req$z_index]

  assign_out <- model4$assign_df
  assign_out$x_value <- x_sol

  chosen <- assign_out[round(assign_out$x_value) == 1L, c(
    "teacher_id", "teacher_name", "subject", "medium",
    "current_school", "school_id", "school_name",
    "move_ij", "p_ij", "d_ij", "burden_raw", "welfare_raw"
  )]
  names(chosen)[names(chosen) == "school_id"] <- "assigned_school"
  names(chosen)[names(chosen) == "school_name"] <- "assigned_school_name"

  shortage_out <- model4$req[, c("school_id", "subject", "medium", "L_jsm", "U_jsm")]
  shortage_out$z_jsm <- z_sol

  list(
    stage1a = stage1a,
    stage1b = stage1b,
    stage2 = stage2,
    stage3 = stage3,
    stage4 = stage4,
    Z1a_star = Z1a_star,
    Z1b_star = Z1b_star,
    Z2_star = Z2_star,
    Z3_star = Z3_star,
    assignments = chosen[order(chosen$teacher_id), ],
    shortages = shortage_out,
    model_data = input_obj
  )
}
