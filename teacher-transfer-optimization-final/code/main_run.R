# Main entry point for the teacher transfer optimization project

source("utils.R")
source("generate_data.R")
source("build_inputs.R")
source("milp_model.R")
source("solve_lexico.R")
source("benchmark.R")
source("evaluation.R")
source("plots.R")

run_full_example <- function(seed = 1234,
                             scenario = "S3",
                             n_underserved = 3,
                             verbose = TRUE) {
  if (verbose) cat("Generating synthetic data.\n")
  data_obj <- generate_synthetic_teacher_transfer_data(
    seed = seed,
    weight_scenario = scenario,
    n_underserved = n_underserved
  )

  if (verbose) cat("Building assignment inputs.\n")
  input_obj <- build_assignment_inputs(data_obj)

  if (verbose) cat("Running lexicographic optimization model.\n")
  opt_sol <- solve_teacher_transfer_lexicographic(input_obj, verbose = verbose)

  if (verbose) cat("Running rule-based administrative benchmark.\n")
  bench_sol <- run_benchmark_administrative_procedure(input_obj, verbose = verbose)

  if (verbose) cat("Evaluating both procedures.\n")
  eval_opt <- evaluate_solution(opt_sol, solution_name = paste("Optimization", scenario), scenario = scenario)
  eval_bench <- evaluate_solution(bench_sol, solution_name = paste("Benchmark", scenario), scenario = scenario)
  evaluation_table <- rbind(eval_opt, eval_bench)

  shortage_comparison <- build_shortage_comparison_df(opt_sol, bench_sol)
  teacher_comparison <- build_teacher_comparison_df(opt_sol, bench_sol)

  list(
    data = data_obj,
    inputs = input_obj,
    scenario = scenario,
    optimization_solution = opt_sol,
    benchmark_solution = bench_sol,
    evaluation = evaluation_table,
    optimization_transfers = make_transfer_summary(opt_sol),
    benchmark_transfers = make_transfer_summary(bench_sol),
    optimization_staffing = make_school_staffing_summary(opt_sol),
    benchmark_staffing = make_school_staffing_summary(bench_sol),
    shortage_comparison = shortage_comparison,
    teacher_comparison = teacher_comparison
  )
}

run_full_example_safe <- function(seed = 1234,
                                  scenario = "S3",
                                  n_underserved = 3,
                                  verbose = TRUE,
                                  max_attempts = 30) {
  last_error <- NULL

  for (attempt in seq_len(max_attempts)) {
    current_seed <- seed + attempt - 1L

    if (verbose) {
      cat("\n----------------------------------------\n")
      cat("Attempt", attempt, "with seed", current_seed, "for scenario", scenario, "\n")
      cat("----------------------------------------\n")
    }

    res <- tryCatch(
      run_full_example(
        seed = current_seed,
        scenario = scenario,
        n_underserved = n_underserved,
        verbose = verbose
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(res)) {
      if (verbose) {
        cat("Feasible instance found for scenario", scenario, "using seed", current_seed, "\n")
      }
      res$actual_seed <- current_seed
      return(res)
    }
  }

  stop(sprintf("No feasible instance found after %d attempts. Last error: %s",
               max_attempts,
               if (!is.null(last_error)) conditionMessage(last_error) else "unknown"))
}

run_weight_scenarios <- function(seed = 20260328,
                                 scenarios = c("S1", "S2", "S3", "S4"),
                                 n_underserved = 3,
                                 verbose = TRUE,
                                 max_attempts = 30) {
  results_by_scenario <- list()
  eval_rows <- list()
  actual_seed <- NA_integer_

  for (s in scenarios) {
    res <- run_full_example_safe(
      seed = seed,
      scenario = s,
      n_underserved = n_underserved,
      verbose = verbose,
      max_attempts = max_attempts
    )
    if (is.na(actual_seed)) actual_seed <- res$actual_seed
    res$optimization_evaluation <- evaluate_solution(res$optimization_solution, paste("Optimization", s), s)
    res$benchmark_evaluation <- evaluate_solution(res$benchmark_solution, paste("Benchmark", s), s)
    res$optimization_evaluation$actual_seed <- res$actual_seed
    res$benchmark_evaluation$actual_seed <- res$actual_seed
    results_by_scenario[[s]] <- res
    eval_rows[[length(eval_rows) + 1L]] <- res$optimization_evaluation
    eval_rows[[length(eval_rows) + 1L]] <- res$benchmark_evaluation
  }

  evaluation_table <- do.call(rbind, eval_rows)
  list(
    results_by_scenario = results_by_scenario,
    evaluation_table = evaluation_table,
    actual_seed = actual_seed
  )
}

run_weight_scenario_example <- function(seed = 20260328,
                                        n_underserved = 3,
                                        verbose = TRUE,
                                        max_attempts = 30) {
  scenario_out <- run_weight_scenarios(
    seed = seed,
    scenarios = c("S1", "S2", "S3", "S4"),
    n_underserved = n_underserved,
    verbose = verbose,
    max_attempts = max_attempts
  )

  cat("\n================ Weight Scenario Evaluation Summary ================\n")
  print(scenario_out$evaluation_table, row.names = FALSE)

  save_all_scenario_plots(scenario_out, base_output_dir = "teacher_transfer_all_scenarios")
  save_weight_scenario_plots(scenario_out$evaluation_table, output_dir = "teacher_transfer_weight_scenarios")
  save_combined_scenario_plot(scenario_out, output_dir = "teacher_transfer_all_scenarios")

  invisible(scenario_out)
}

if (sys.nframe() == 0) {
  run_weight_scenario_example(
    seed = 20260328,
    n_underserved = 3,
    verbose = TRUE,
    max_attempts = 30
  )
}
