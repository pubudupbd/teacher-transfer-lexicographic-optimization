generate_synthetic_teacher_transfer_data <- function(
    n_teachers = 60,
    n_schools = 8,
    subjects = c("Mathematics", "Science", "English"),
    media = c("Sinhala", "English"),
    seed = 1234,
    n_underserved = 3,
    weight_scenario = "S3"
) {
  set.seed(seed)

  if (n_schools < 1L) stop("n_schools must be at least 1.")
  if (n_teachers < 1L) stop("n_teachers must be at least 1.")
  if (n_underserved < 0L || n_underserved > n_schools) {
    stop("n_underserved must be between 0 and n_schools.")
  }

  schools <- data.frame(
    school_id = seq_len(n_schools),
    school_name = paste("School", seq_len(n_schools)),
    category = sample(c("1AB", "1C", "Type2", "Type3"), n_schools, replace = TRUE),
    underserved = 0L,
    coord_x = runif(n_schools, 0, 100),
    coord_y = runif(n_schools, 0, 100),
    stringsAsFactors = FALSE
  )

  if (n_underserved > 0L) {
    underserved_ids <- sample(seq_len(n_schools), n_underserved)
    schools$underserved[underserved_ids] <- 1L
  }

  schools <- assign_weights(schools, scenario = weight_scenario)

  teachers <- data.frame(
    teacher_id = seq_len(n_teachers),
    teacher_name = paste("Teacher", seq_len(n_teachers)),
    subject = sample(subjects, n_teachers, replace = TRUE, prob = c(0.40, 0.35, 0.25)),
    medium = sample(media, n_teachers, replace = TRUE, prob = c(0.80, 0.20)),
    years_service = sample(1:20, n_teachers, replace = TRUE),
    hardship = sample(c(0L, 1L), n_teachers, replace = TRUE, prob = c(0.80, 0.20)),
    compulsory_transfer = sample(c(0L, 1L), n_teachers, replace = TRUE, prob = c(0.78, 0.22)),
    home_x = runif(n_teachers, 0, 100),
    home_y = runif(n_teachers, 0, 100),
    stringsAsFactors = FALSE
  )

  teachers$current_school <- sample(schools$school_id, n_teachers, replace = TRUE)
  teachers$max_burden <- ifelse(teachers$hardship == 1L, runif(n_teachers, 20, 45), Inf)

  dist_home_school <- outer(
    seq_len(n_teachers),
    seq_len(n_schools),
    Vectorize(function(i, j) {
      euclidean_distance(
        teachers$home_x[i], teachers$home_y[i],
        schools$coord_x[j], schools$coord_y[j]
      )
    })
  )

  preferred_mat <- t(apply(dist_home_school, 1, function(d) {
    ord <- order(d)
    picks <- ord[1:min(3, length(ord))]
    if (length(picks) < 3) {
      picks <- c(picks, rep(picks[length(picks)], 3 - length(picks)))
    }
    picks
  }))

  teachers$pref_1 <- preferred_mat[, 1]
  teachers$pref_2 <- preferred_mat[, 2]
  teachers$pref_3 <- preferred_mat[, 3]

  type_grid <- expand.grid(subject = subjects, medium = media, stringsAsFactors = FALSE)
  type_grid$type_key <- make_type_key(type_grid$subject, type_grid$medium)
  teachers$type_key <- make_type_key(teachers$subject, teachers$medium)

  counts_tbl <- as.data.frame.matrix(xtabs(~ current_school + type_key, data = teachers))
  counts_tbl$school_id <- as.integer(rownames(counts_tbl))
  rownames(counts_tbl) <- NULL

  school_type <- merge(
    expand.grid(
      school_id = schools$school_id,
      type_key = type_grid$type_key,
      stringsAsFactors = FALSE
    ),
    reshape(
      counts_tbl,
      direction = "long",
      varying = names(counts_tbl)[names(counts_tbl) != "school_id"],
      v.names = "current_count",
      timevar = "type_key",
      times = names(counts_tbl)[names(counts_tbl) != "school_id"]
    ),
    by = c("school_id", "type_key"),
    all.x = TRUE,
    sort = FALSE
  )
  school_type$current_count[is.na(school_type$current_count)] <- 0L

  type_parts <- do.call(rbind, strsplit(school_type$type_key, "__", fixed = TRUE))
  school_type$subject <- type_parts[, 1]
  school_type$medium <- type_parts[, 2]

  shortage_shock <- sample(
    c(-1L, 0L, 0L, 1L, 1L, 2L),
    nrow(school_type),
    replace = TRUE,
    prob = c(0.10, 0.25, 0.25, 0.20, 0.15, 0.05)
  )
  school_type$L_jsm <- pmax(0L, school_type$current_count + shortage_shock)
  school_type$U_jsm <- school_type$current_count + sample(1:4, nrow(school_type), replace = TRUE)

  total_teachers_by_type <- aggregate(
    teacher_id ~ type_key,
    data = transform(teachers, teacher_id = 1L),
    FUN = sum
  )
  names(total_teachers_by_type)[2] <- "n_teachers"

  capacity_by_type <- aggregate(U_jsm ~ type_key, data = school_type, FUN = sum)
  gap_df <- merge(total_teachers_by_type, capacity_by_type, by = "type_key", all.x = TRUE)
  gap_df$U_jsm[is.na(gap_df$U_jsm)] <- 0L
  gap_df$gap <- gap_df$n_teachers - gap_df$U_jsm

  if (any(gap_df$gap > 0L)) {
    for (k in which(gap_df$gap > 0L)) {
      type_k <- gap_df$type_key[k]
      idx <- which(school_type$type_key == type_k)
      add_each <- ceiling(gap_df$gap[k] / length(idx))
      school_type$U_jsm[idx] <- school_type$U_jsm[idx] + add_each
    }
  }

  shut_prob <- 0.08
  can_shut <- school_type$U_jsm > 0L
  for (r in which(can_shut)) {
    if (runif(1) < shut_prob) {
      type_k <- school_type$type_key[r]
      type_total_capacity <- sum(school_type$U_jsm[school_type$type_key == type_k])
      n_type_teachers <- total_teachers_by_type$n_teachers[
        match(type_k, total_teachers_by_type$type_key)
      ]
      if (!is.na(n_type_teachers) &&
          type_total_capacity - school_type$U_jsm[r] >= n_type_teachers) {
        school_type$U_jsm[r] <- 0L
        school_type$L_jsm[r] <- 0L
      }
    }
  }

  capacity_by_type <- aggregate(U_jsm ~ type_key, data = school_type, FUN = sum)
  gap_df <- merge(total_teachers_by_type, capacity_by_type, by = "type_key", all.x = TRUE)
  gap_df$gap <- gap_df$n_teachers - gap_df$U_jsm

  if (any(gap_df$gap > 0L)) {
    for (k in which(gap_df$gap > 0L)) {
      type_k <- gap_df$type_key[k]
      idx <- which(school_type$type_key == type_k)
      school_type$U_jsm[idx[1]] <- school_type$U_jsm[idx[1]] + gap_df$gap[k]
    }
  }

  list(
    teachers = teachers,
    schools = schools,
    school_type_requirements = school_type,
    subjects = subjects,
    media = media,
    weight_scenario = weight_scenario
  )
}
