theme_publication <- function(base_size = 12, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = base_size + 2, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5),
      axis.title = element_text(size = base_size, face = "bold"),
      axis.text = element_text(size = base_size - 1, colour = "black"),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(size = base_size, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.line = element_line(colour = "black", linewidth = 0.4),
      plot.caption = element_text(size = base_size - 2, hjust = 0),
      plot.margin = margin(10, 12, 10, 10)
    )
}

build_shortage_comparison_df <- function(opt_sol, bench_sol) {
  teachers <- opt_sol$model_data$teachers
  schools <- opt_sol$model_data$schools
  req <- opt_sol$model_data$req

  initial_counts <- aggregate(
    teacher_id ~ current_school + subject + medium,
    data = transform(teachers, teacher_id = 1L),
    FUN = sum
  )
  names(initial_counts) <- c("school_id", "subject", "medium", "initial_count")

  init_short <- merge(
    req,
    initial_counts,
    by = c("school_id", "subject", "medium"),
    all.x = TRUE,
    sort = FALSE
  )
  init_short$initial_count[is.na(init_short$initial_count)] <- 0L
  init_short$initial_shortage <- pmax(0, init_short$L_jsm - init_short$initial_count)

  opt_short <- opt_sol$shortages[, c("school_id", "subject", "medium", "z_jsm")]
  names(opt_short)[4] <- "optimization_shortage"

  bench_short <- bench_sol$shortages[, c("school_id", "subject", "medium", "z_jsm")]
  names(bench_short)[4] <- "benchmark_shortage"

  out <- Reduce(
    function(x, y) merge(x, y, by = c("school_id", "subject", "medium"), all = TRUE, sort = FALSE),
    list(
      req[, c("school_id", "subject", "medium", "L_jsm", "U_jsm")],
      init_short[, c("school_id", "subject", "medium", "initial_shortage")],
      opt_short,
      bench_short
    )
  )

  out$initial_shortage[is.na(out$initial_shortage)] <- 0
  out$optimization_shortage[is.na(out$optimization_shortage)] <- 0
  out$benchmark_shortage[is.na(out$benchmark_shortage)] <- 0

  out <- merge(
    out,
    schools[, c("school_id", "school_name", "underserved", "w_j")],
    by = "school_id",
    all.x = TRUE,
    sort = FALSE
  )

  out
}

build_teacher_comparison_df <- function(opt_sol, bench_sol) {
  opt_df <- opt_sol$assignments[, c(
    "teacher_id", "teacher_name", "subject", "medium",
    "current_school", "assigned_school", "assigned_school_name",
    "welfare_raw", "burden_raw"
  )]
  names(opt_df)[names(opt_df) == "assigned_school"] <- "assigned_school_opt"
  names(opt_df)[names(opt_df) == "assigned_school_name"] <- "assigned_school_name_opt"
  names(opt_df)[names(opt_df) == "welfare_raw"] <- "welfare_opt"
  names(opt_df)[names(opt_df) == "burden_raw"] <- "burden_opt"

  bench_df <- bench_sol$assignments[, c(
    "teacher_id", "assigned_school", "assigned_school_name",
    "welfare_raw", "burden_raw"
  )]
  names(bench_df)[names(bench_df) == "assigned_school"] <- "assigned_school_bench"
  names(bench_df)[names(bench_df) == "assigned_school_name"] <- "assigned_school_name_bench"
  names(bench_df)[names(bench_df) == "welfare_raw"] <- "welfare_bench"
  names(bench_df)[names(bench_df) == "burden_raw"] <- "burden_bench"

  out <- merge(opt_df, bench_df, by = "teacher_id", all.x = TRUE, sort = FALSE)
  out$moved_opt <- as.integer(out$current_school != out$assigned_school_opt)
  out$moved_bench <- as.integer(out$current_school != out$assigned_school_bench)
  out
}

plot_shortage_by_school <- function(shortage_df) {
  school_info <- unique(shortage_df[, c("school_name", "underserved", "w_j")])
  school_sum <- aggregate(
    cbind(initial_shortage, optimization_shortage, benchmark_shortage) ~ school_name,
    data = shortage_df,
    FUN = sum
  )
  school_sum <- merge(school_sum, school_info, by = "school_name", all.x = TRUE, sort = FALSE)
  school_sum$change_opt_vs_initial <- school_sum$optimization_shortage - school_sum$initial_shortage
  school_sum$school_label <- ifelse(
    school_sum$underserved == 1,
    paste0(school_sum$school_name, "*\n(w=", school_sum$w_j, ")"),
    paste0(school_sum$school_name, "\n(w=", school_sum$w_j, ")")
  )

  long_df <- rbind(
    data.frame(school_name = school_sum$school_name, school_label = school_sum$school_label, stage = "Benchmark", shortage = school_sum$benchmark_shortage),
    data.frame(school_name = school_sum$school_name, school_label = school_sum$school_label, stage = "Initial", shortage = school_sum$initial_shortage),
    data.frame(school_name = school_sum$school_name, school_label = school_sum$school_label, stage = "Optimization", shortage = school_sum$optimization_shortage)
  )
  long_df$stage <- factor(long_df$stage, levels = c("Benchmark", "Initial", "Optimization"))
  long_df$label_text <- round(long_df$shortage, 0)
  long_df$label_y <- ifelse(long_df$shortage == 0, 0.18, long_df$shortage + 0.18)

  increase_df <- school_sum[school_sum$change_opt_vs_initial > 0, , drop = FALSE]
  if (nrow(increase_df) > 0) {
    increase_df$increase_label <- paste0("+", round(increase_df$change_opt_vs_initial, 0))
    increase_df$school_label <- ifelse(
      increase_df$underserved == 1,
      paste0(increase_df$school_name, "*\n(w=", increase_df$w_j, ")"),
      paste0(increase_df$school_name, "\n(w=", increase_df$w_j, ")")
    )
  }

  y_max <- max(long_df$shortage, na.rm = TRUE)

  p <- ggplot(long_df, aes(x = school_label, y = shortage, fill = stage)) +
    geom_col(position = position_dodge(width = 0.9), colour = "black", linewidth = 0.25) +
    geom_text(aes(y = label_y, label = label_text), position = position_dodge(width = 0.9), vjust = 0, size = 3.8) +
    labs(
      title = "Total Shortage by School",
      x = "School",
      y = "Total shortage",
      caption = paste(
        "* Underserved schools with higher shortage weights.",
        "Increases under optimization reflect reallocation to higher-priority schools."
      )
    ) +
    expand_limits(y = y_max + 1.6) +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

  if (nrow(increase_df) > 0) {
    p <- p +
      geom_text(
        data = increase_df,
        aes(x = school_label, y = optimization_shortage + 0.6, label = increase_label),
        inherit.aes = FALSE,
        size = 3.8
      )
  }

  p
}

plot_shortage_by_subject_medium <- function(shortage_df) {
  shortage_df$type_label <- paste(shortage_df$subject, shortage_df$medium, sep = " - ")

  type_sum <- aggregate(
    cbind(initial_shortage, optimization_shortage, benchmark_shortage) ~ type_label,
    data = shortage_df,
    FUN = sum
  )

  long_df <- rbind(
    data.frame(type_label = type_sum$type_label, stage = "Benchmark", shortage = type_sum$benchmark_shortage),
    data.frame(type_label = type_sum$type_label, stage = "Initial", shortage = type_sum$initial_shortage),
    data.frame(type_label = type_sum$type_label, stage = "Optimization", shortage = type_sum$optimization_shortage)
  )

  long_df$stage <- factor(long_df$stage, levels = c("Benchmark", "Initial", "Optimization"))

  ggplot(long_df, aes(x = type_label, y = shortage, fill = stage)) +
    geom_col(position = "dodge", colour = "black", linewidth = 0.25) +
    labs(title = "Total Shortage by Subject and Medium", x = "Subject - Medium", y = "Total shortage") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_average_welfare <- function(teacher_df) {
  welfare_df <- data.frame(
    procedure = c("Optimization", "Benchmark"),
    average_welfare = c(mean(teacher_df$welfare_opt, na.rm = TRUE), mean(teacher_df$welfare_bench, na.rm = TRUE))
  )
  ggplot(welfare_df, aes(x = procedure, y = average_welfare, fill = procedure)) +
    geom_col(colour = "black", linewidth = 0.25) +
    geom_text(aes(label = round(average_welfare, 2)), vjust = -0.3, size = 3.8) +
    labs(title = "Average Teacher Welfare", x = "Procedure", y = "Average welfare score") +
    theme_publication()
}

plot_average_burden <- function(teacher_df) {
  burden_df <- data.frame(
    procedure = c("Optimization", "Benchmark"),
    average_burden = c(mean(teacher_df$burden_opt, na.rm = TRUE), mean(teacher_df$burden_bench, na.rm = TRUE))
  )
  ggplot(burden_df, aes(x = procedure, y = average_burden, fill = procedure)) +
    geom_col(colour = "black", linewidth = 0.25) +
    geom_text(aes(label = round(average_burden, 2)), vjust = -0.3, size = 3.8) +
    labs(title = "Average Transfer Burden", x = "Procedure", y = "Average burden") +
    theme_publication()
}

plot_teachers_moved <- function(teacher_df) {
  moved_df <- data.frame(
    procedure = c("Optimization", "Benchmark"),
    teachers_moved = c(sum(teacher_df$moved_opt, na.rm = TRUE), sum(teacher_df$moved_bench, na.rm = TRUE))
  )
  ggplot(moved_df, aes(x = procedure, y = teachers_moved, fill = procedure)) +
    geom_col(colour = "black", linewidth = 0.25) +
    geom_text(aes(label = round(teachers_moved, 0)), vjust = -0.3, size = 3.8) +
    labs(title = "Number of Teachers Moved", x = "Procedure", y = "Teachers moved") +
    theme_publication()
}

plot_difficult_school_coverage <- function(shortage_df) {
  difficult_df <- shortage_df[shortage_df$underserved == 1, , drop = FALSE]
  if (nrow(difficult_df) == 0) return(NULL)

  coverage_df <- aggregate(
    cbind(
      optimization_fully_covered = as.integer(difficult_df$optimization_shortage == 0),
      benchmark_fully_covered = as.integer(difficult_df$benchmark_shortage == 0)
    ) ~ school_name,
    data = difficult_df,
    FUN = mean
  )

  long_df <- rbind(
    data.frame(school_name = coverage_df$school_name, procedure = "Optimization", coverage_rate = coverage_df$optimization_fully_covered),
    data.frame(school_name = coverage_df$school_name, procedure = "Benchmark", coverage_rate = coverage_df$benchmark_fully_covered)
  )

  ggplot(long_df, aes(x = school_name, y = coverage_rate, fill = procedure)) +
    geom_col(position = "dodge", colour = "black", linewidth = 0.25) +
    geom_text(aes(label = round(coverage_rate, 2)), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.8) +
    labs(title = "Underserved-School Full-Coverage Rate", x = "Underserved school", y = "Coverage rate") +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_weight_scenario_shortage <- function(eval_table) {
  opt_df <- eval_table[grep("^Optimization", eval_table$solution), , drop = FALSE]
  ggplot(opt_df, aes(x = scenario, y = final_total_shortage, fill = scenario)) +
    geom_col(colour = "black", linewidth = 0.25) +
    geom_text(aes(label = round(final_total_shortage, 0)), vjust = -0.3, size = 3.8) +
    labs(title = "Final Total Shortage under Different Weight Scenarios", x = "Weight scenario", y = "Final total shortage") +
    theme_publication()
}

plot_weight_scenario_difficult_coverage <- function(eval_table) {
  opt_df <- eval_table[grep("^Optimization", eval_table$solution), , drop = FALSE]
  ggplot(opt_df, aes(x = scenario, y = difficult_school_full_coverage_rate, fill = scenario)) +
    geom_col(colour = "black", linewidth = 0.25) +
    geom_text(aes(label = round(difficult_school_full_coverage_rate, 2)), vjust = -0.3, size = 3.8) +
    labs(title = "Underserved-School Coverage under Different Weight Scenarios", x = "Weight scenario", y = "Coverage rate") +
    theme_publication()
}

plot_shortage_by_school_all_scenarios <- function(scenario_out) {
  scenario_names <- names(scenario_out$results_by_scenario)

  combined_df <- do.call(
    rbind,
    lapply(scenario_names, function(s) {
      res <- scenario_out$results_by_scenario[[s]]
      shortage_df <- build_shortage_comparison_df(res$optimization_solution, res$benchmark_solution)
      school_info <- unique(shortage_df[, c("school_name", "underserved", "w_j")])
      school_sum <- aggregate(cbind(initial_shortage, optimization_shortage, benchmark_shortage) ~ school_name,
                              data = shortage_df, FUN = sum)
      school_sum <- merge(school_sum, school_info, by = "school_name", all.x = TRUE, sort = FALSE)
      short_name <- gsub("^School\\s+", "S", school_sum$school_name)
      school_sum$school_label <- ifelse(school_sum$underserved == 1, paste0(short_name, "*"), short_name)
      rbind(
        data.frame(scenario = s, school_label = school_sum$school_label, stage = "Benchmark", shortage = school_sum$benchmark_shortage),
        data.frame(scenario = s, school_label = school_sum$school_label, stage = "Initial", shortage = school_sum$initial_shortage),
        data.frame(scenario = s, school_label = school_sum$school_label, stage = "Optimization", shortage = school_sum$optimization_shortage)
      )
    })
  )

  combined_df$stage <- factor(combined_df$stage, levels = c("Benchmark", "Initial", "Optimization"))

  ggplot(combined_df, aes(x = school_label, y = shortage, fill = stage)) +
    geom_col(position = position_dodge(width = 0.9), colour = "black", linewidth = 0.25) +
    facet_wrap(~ scenario, ncol = 2) +
    labs(
      title = "Total Shortage by School under Four Weight Scenarios",
      x = "School",
      y = "Total shortage",
      caption = "* Underserved schools. Weight schemes: S1 = 1, S2 = 2, S3 = 3, S4 = 5 for underserved schools."
    ) +
    theme_publication() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.spacing = grid::unit(1.2, "lines"),
      plot.caption = element_text(hjust = 0)
    )
}

save_pub_plot <- function(filename_base, plot, output_dir, width = 8, height = 5, dpi = 600) {
  if (is.null(plot)) return(invisible(NULL))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  ggsave(
    filename = file.path(output_dir, paste0(filename_base, ".png")),
    plot = plot, width = width, height = height, units = "in", dpi = dpi, bg = "white"
  )

  ggsave(
    filename = file.path(output_dir, paste0(filename_base, ".pdf")),
    plot = plot, width = width, height = height, units = "in", device = "pdf", bg = "white"
  )
}

save_all_plots <- function(results, output_dir = "teacher_transfer_plots", width = 8, height = 5, dpi = 600) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  shortage_df <- build_shortage_comparison_df(results$optimization_solution, results$benchmark_solution)
  teacher_df <- build_teacher_comparison_df(results$optimization_solution, results$benchmark_solution)

  p1 <- plot_shortage_by_school(shortage_df)
  p2 <- plot_shortage_by_subject_medium(shortage_df)
  p3 <- plot_average_welfare(teacher_df)
  p4 <- plot_average_burden(teacher_df)
  p5 <- plot_teachers_moved(teacher_df)
  p6 <- plot_difficult_school_coverage(shortage_df)

  save_pub_plot("plot1_shortage_by_school", p1, output_dir, width, height, dpi)
  save_pub_plot("plot2_shortage_by_subject_medium", p2, output_dir, width, height, dpi)
  save_pub_plot("plot3_average_welfare", p3, output_dir, width, height, dpi)
  save_pub_plot("plot4_average_burden", p4, output_dir, width, height, dpi)
  save_pub_plot("plot5_teachers_moved", p5, output_dir, width, height, dpi)

  if (!is.null(p6)) {
    save_pub_plot("plot6_difficult_school_coverage", p6, output_dir, width, height, dpi)
  }

  invisible(list(
    shortage_by_school = p1,
    shortage_by_subject_medium = p2,
    average_welfare = p3,
    average_burden = p4,
    teachers_moved = p5,
    difficult_school_coverage = p6
  ))
}

save_weight_scenario_plots <- function(eval_table,
                                       output_dir = "teacher_transfer_weight_scenarios",
                                       width = 8,
                                       height = 5,
                                       dpi = 600) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  p1 <- plot_weight_scenario_shortage(eval_table)
  p2 <- plot_weight_scenario_difficult_coverage(eval_table)
  save_pub_plot("scenario_plot1_final_total_shortage", p1, output_dir, width, height, dpi)
  save_pub_plot("scenario_plot2_underserved_school_coverage", p2, output_dir, width, height, dpi)
  invisible(list(scenario_shortage = p1, scenario_difficult_coverage = p2))
}

save_all_scenario_plots <- function(scenario_out,
                                    base_output_dir = "teacher_transfer_all_scenarios",
                                    width = 8,
                                    height = 5,
                                    dpi = 600) {
  if (!dir.exists(base_output_dir)) dir.create(base_output_dir, recursive = TRUE)

  scenario_names <- names(scenario_out$results_by_scenario)
  all_plot_objects <- list()

  for (s in scenario_names) {
    scenario_dir <- file.path(base_output_dir, s)
    if (!dir.exists(scenario_dir)) dir.create(scenario_dir, recursive = TRUE)

    res <- scenario_out$results_by_scenario[[s]]
    shortage_df <- build_shortage_comparison_df(res$optimization_solution, res$benchmark_solution)
    teacher_df <- build_teacher_comparison_df(res$optimization_solution, res$benchmark_solution)

    p1 <- plot_shortage_by_school(shortage_df)
    p2 <- plot_shortage_by_subject_medium(shortage_df)
    p3 <- plot_average_welfare(teacher_df)
    p4 <- plot_average_burden(teacher_df)
    p5 <- plot_teachers_moved(teacher_df)
    p6 <- plot_difficult_school_coverage(shortage_df)

    save_pub_plot("plot1_shortage_by_school", p1, scenario_dir, width, height, dpi)
    save_pub_plot("plot2_shortage_by_subject_medium", p2, scenario_dir, width, height, dpi)
    save_pub_plot("plot3_average_welfare", p3, scenario_dir, width, height, dpi)
    save_pub_plot("plot4_average_burden", p4, scenario_dir, width, height, dpi)
    save_pub_plot("plot5_teachers_moved", p5, scenario_dir, width, height, dpi)
    if (!is.null(p6)) save_pub_plot("plot6_difficult_school_coverage", p6, scenario_dir, width, height, dpi)

    all_plot_objects[[s]] <- list(
      shortage_by_school = p1,
      shortage_by_subject_medium = p2,
      average_welfare = p3,
      average_burden = p4,
      teachers_moved = p5,
      difficult_school_coverage = p6
    )
  }

  invisible(all_plot_objects)
}

save_combined_scenario_plot <- function(scenario_out,
                                        output_dir = "teacher_transfer_all_scenarios",
                                        width = 14,
                                        height = 9,
                                        dpi = 600) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  p <- plot_shortage_by_school_all_scenarios(scenario_out)
  save_pub_plot("combined_shortage_by_school_all_scenarios", p, output_dir, width, height, dpi)
  invisible(p)
}
