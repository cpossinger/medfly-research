library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(fdapace)
library(frechet)
library(furrr)
library(readr)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(gghighlight)
library(future.apply)
library(future)
library(reshape2)
library(sjPlot)
library(scales)

# pacman::p_load(plyr, dplyr, tidyr, purrr, stringr, fdapace, frechet, furrr, readr, ggplot2, magrittr, ggpubr, gghighlight, future.apply, future, reshape2, sjPlot)



lifetable_2_df <- function(file, new_file_name) {
  lifetable <- read.table(file)

  chunk <- 177
  total_rows <- nrow(lifetable)
  chunk_num <- rep(1:ceiling(total_rows / chunk), each = chunk)[1:total_rows]
  chunk_lst <- split(lifetable, chunk_num)

  cage_lst <- chunk_lst %>%
    map_depth(2, ~.x) %>%
    flatten()

  cage_lst <- cage_lst[-1]
  names(cage_lst) <- 1:167

  cage_lst %<>% map(~ tibble(
    age = 0:172,
    n = .x[5:length(.x)] %>% as.integer(),
    cohort = .x[1],
    cage = .x[3],
  ))

  fly_df <- cage_lst %>%
    bind_rows()

  write_csv(fly_df, new_file_name)
}

create_surv <- function(df) {
  df %>%
    mutate(
      s = Lwls1D(
        bw = 2.5,
        kernel_type = "epan",
        xin = age,
        yin = n / n[1],
        xout = age
      )
    ) %>%
    filter(s != 0)
}


create_density <- function(df) {
  df %>% mutate(
    dx = c(-diff(n), 0),
    d = CreateDensity(
      y = rep(age, dx),
      optns = list(outputGrid = age)
    )$y
  )
}

create_hazard <- function(df) {
  df %>%
    mutate(
      h = Lwls1D(
        bw = 2.5,
        kernel_type = "epan",
        xin = age,
        yin = d / s,
        xout = age
      )
    ) %>%
    filter(h != 0)
}

create_log_hazard <- function(df) {
  df %>% mutate(
    lh = log(h)
  )
}

create_log_hazard_deriv <- function(df) {
  df %>% mutate(
    lhd = Lwls1D(
      bw = 1.5,
      kernel_type = "gauss",
      win = rep(1, length(age)),
      xin = age,
      yin = lh,
      xout = age,
      npoly = 2,
      nder = 1
    )
  )
}


fix_col_names <- function(df, sex) {
  sex_abbr <- sex %>%
    str_split("") %>%
    extract2(1) %>%
    tolower()

  for (col_name in c("s", "lh", "lhd", "n")) {
    names(df)[names(df) == col_name] <- paste0(col_name, sex_abbr)
  }

  df
}

clean_df <- function(df) {
  outlier_cages <- c(
    5, 6, 8, 54, 30, 72, 146, 73, 97, 106, 130, 129, 104, 110,
    126, 7, 4, 2, 99, 81, 119, 102, 101, 71, 78, 80, 1, 79, 76,
    96, 77, 48, 74, 86, 124, 22, 52, 51, 23, 41, 56, 57, 37, 20,
    24, 45, 55, 39, 21, 103, 75, 58
  )


  df %>%
    filter(
      cohort != 3,
      cohort != 1,
      cohort != 7,
      !(cage %in% outlier_cages)
    )
}

clean_func_results <- function(df) {
  df %>%
    filter(age > 0, age < 51)
}

add_funcs <- function(df) {
  df %>%
    split(df$cage) %>%
    future_map_dfr(~
      .x %>%
        create_surv() %>%
        create_density() %>%
        create_hazard() %>%
        create_log_hazard() %>%
        create_log_hazard_deriv() %>%
        select(-dx, -d, -h))
}

create_surv_diff <- function(df) {
  df %>% mutate(
    sm_sa = sm - sa,
    sf_sa = sf - sa
  )
}



medfly_preprocess <- function() {
  genders <- c("FEMALE", "MALE", "ALL")
  genders %>%
    set_names(genders) %>%
    future_map(~
      read_csv(paste0("data/", .x, ".csv"), show_col_types = FALSE) %>%
        clean_df() %>%
        group_by(cage) %>%
        add_funcs() %>%
        fix_col_names(sex = .x)) %>%
    reduce(left_join,
      by = c(
        "age",
        "cohort",
        "cage"
      )
    ) %>%
    na.omit() %>%
    create_surv_diff()
}


frechet_plot <- function(df, t, response, pred, pred_name, y_axis, y_axis_name, x_axis_name = "Remaining Lifespan", y_limits) {
  if ((t %>% unique()) != 10) {
    y_axis_name <- ""
  }

  cage_lst <- df %>% group_split()

  xin <- pred %>%
    map(function(pred) {
      cage_lst %>%
        map2_dbl(t, ~ .x %>%
          pull(pred) %>%
          extract(.y))
    }) %>%
    reduce(cbind)

  hin <- cage_lst %>%
    map2(t, function(cage, t) {
      cage %<>% filter(age >= t)
      resp <- cage$age - t
      bin <- cage[[response]]
      hist(rep(resp, bin), plot = FALSE)
    })

  dout <- GloDenReg(
    xin = xin,
    hin = hin
  )$dout %>%
    set_names(df$cage %>% unique())

  print(dout)

  if (y_axis == "lhf") {
    lh_lst <- dout %>%
      map(~ list(lhf = log(.x$y / surv_func(.x$x, .x$y))))

    dout <- map2(
      dout,
      lh_lst,
      ~ .x %>% append(.y)
    )
  }

  if (y_axis == "lhm") {
    lh_lst <- dout %>%
      map(~ list(lhm = log(.x$y / surv_func(.x$x, .x$y))))

    dout <- map2(
      dout,
      lh_lst,
      ~ .x %>% append(.y)
    )
  }

  pred_lst <- cage_lst %>% map2(t, ~ .x %>%
    select(age, all_of(pred)) %>%
    filter(age == .y) %>%
    as.list())

  plot_df <- map2(
    dout,
    pred_lst,
    ~ .x %>% append(.y)
  ) %>%
    bind_rows(.id = "cage")

  pred %>% map2(pred_name, function(pred, pred_name) {
    ggplot(
      data = plot_df,
      aes(
        x = x,
        y = !!sym(y_axis),
        group = cage,
        color = !!sym(pred)
      )
    ) +
      geom_line() +
      scale_color_gradient(
        name = pred_name,
        low = "blue",
        high = "red"
      ) +
      xlab(x_axis_name) +
      ylab(y_axis_name) +
      xlim(0, 25) +
      theme_minimal() +
      theme(text = element_text(size = 10)) +
      scale_y_continuous(
        limits = y_limits
      )
  })
}

create_mean_ci_df <- function(df, pred) {
  df %<>% filter(age <= 45)
  pred %>%
    purrr::set_names() %>%
    map_dfr(function(pred) {
      cage_lst <- df %>% group_split()
      Ly <- cage_lst %>% map(~ .x %>% pull(pred))
      Lt <- cage_lst %>% map(~ .x %>% pull(age))
      cbind(
        GetMeanCI(Ly, Lt)$CI,
        mean = GetMeanCurve(Ly, Lt)$mu
      )
    }, .id = "pred")
}

surv_func <- function(x, y) {
  surv_func <- c()

  for (t in x) {
    surv_func %<>% append(
      trapzRcpp(
        x[x >= t],
        y[x >= t]
      )
    )
  }

  return(surv_func)
}


save_ci_plot <- function(rst, rawbetas, varSelectedNames, filename, y_limits) {
  rst <- matrix(unlist(rst), nrow = length(as.matrix(rawbetas)))
  bound_beta <- apply(rst, 1, quantile, c(0.025, 0.975)) # calculate quantiles
  lower_beta <- matrix(bound_beta[1, ], ncol = ncol(rawbetas))
  upper_beta <- matrix(bound_beta[2, ], ncol = ncol(rawbetas))
  # smooth the result with Lwls1D and bw = 20
  smoothbetas <- rawbetas
  smoothlower <- lower_beta
  smoothupper <- upper_beta
  bw <- 20
  kernel <- "epan"
  for (i in 2:ncol(rawbetas)) {
    smoothbetas[, i] <- Lwls1D(
      bw = bw, kernel_type = kernel,
      xin = rawbetas$age, yin = rawbetas[, i],
      xout = rawbetas$age
    )
    smoothlower[, i] <- Lwls1D(
      bw = bw, kernel_type = kernel,
      xin = lower_beta[, 1], yin = lower_beta[, i],
      xout = lower_beta[, 1]
    )
    smoothupper[, i] <- Lwls1D(
      bw = bw, kernel_type = kernel,
      xin = upper_beta[, 1], yin = upper_beta[, i],
      xout = upper_beta[, 1]
    )
  }

  smoothbetas <- smoothbetas[, -2]
  smoothupper <- smoothupper[, -2]
  smoothlower <- smoothlower[, -2]

  names(smoothbetas) <- c("Age", varSelectedNames)
  df <- data.frame(
    smoothbetas %>% melt(
      id.vars = "Age",
      variable.name = "predictor",
      value.name = "Beta"
    ),
    lower = as.vector(smoothlower[, 2:ncol(smoothlower)]),
    upper = as.vector(smoothupper[, 2:ncol(smoothupper)])
  ) %>%
    mutate(
      highlight = (lower >= 0 | upper <= 0)
    )

  beta_ci <- ggplot() +
    geom_line(data = df, aes(x = Age, y = Beta, color = highlight, group = 1), size = 0.6) +
    scale_color_manual(values = c("black", "red")) +
    scale_y_continuous(limits = y_limits) +
    geom_ribbon(data = df, aes(x = Age, ymin = lower, ymax = upper), fill = "grey12", alpha = 0.2) +
    facet_wrap(~predictor, scales = "free_y") +
    geom_hline(data = subset(df, predictor %in% unique(df$predictor)[-6]), aes(yintercept = 0), colour = "blue", linetype = 2) +
    theme_minimal(base_size = 25) +
    theme(legend.position = "none") +
    xlim(8, 45)

  facet_dim <- df$predictor %>%
    unique() %>%
    length() %>%
    wrap_dims()


  ggsave(
    filename = paste0(filename, ".pdf"),
    plot = beta_ci, path = "plots",
    width = facet_dim[2] * 6,
    height = facet_dim[1] * 6
  )
}

bootstrap_ci_plot <- function(df, resp, varSelected, varSelectedNames, filename, y_limits) {
  rawbetas <- rawbetas(df, resp, varSelected)
  rst <- readRDS(paste0("rst/", filename, ".rds"))
  save_ci_plot(rst, rawbetas, varSelectedNames, filename, y_limits)
}

bootstrap <- function(df, resp, varSelected, filename, B) {
  print("Bootstrap Starting")
  cage <- unique(df$cage) # unique states


  # bootstrap using parallel computing
  rst <- map(.x = 1:B, .progress = TRUE, .f = function(i) {
    index <- sample(length(cage), replace = TRUE)
    df_boot <- NULL
    for (j in cage[index]) {
      df_boot <- rbind(df_boot, df[df$cage == j, ])
    }
    beta_est(df_boot, resp, varSelected, i)
  })

  dir <- paste0(getwd(), "/rst")
  if (!dir.exists(dir)) dir.create(dir)

  saveRDS(rst, file = paste0("rst/", filename, ".rds"))
}


beta_est <- function(df, resp, varSelected, i) {
  df %>%
    select("cage", "age", resp, varSelected) %>%
    ddply(.(age), function(d) {
      resp <- names(d)[3]
      varSelected <- names(d)[4:ncol(d)]
      d_std <- d %>% select(resp, varSelected)
      d_std[varSelected] <- d_std[varSelected] %>%
        apply(2, function(x) (x - mean(x)) / sd(x))
      Xmean <- d %>%
        select(varSelected) %>%
        colMeans()
      Xsd <- d %>%
        select(varSelected) %>%
        apply(2, sd)
      lmfit <- lm(
        as.formula(paste(resp, "~", paste(varSelected, collapse = " + "))),
        data = d_std
      )
      res <- coef <- lmfit$coefficients
      res[1] <- coef[1] - sum(coef[-1] * Xmean / Xsd)
      res[2:length(coef)] <- coef[-1] / Xsd
      res
    }) %>%
    as.data.frame()
}

rawbetas <- function(df, resp, varSelected) {
  df %>%
    select("cage", "age", resp, varSelected) %>%
    ddply(.(age), function(d) {
      resp <- names(d)[3]
      varSelected <- names(d)[4:ncol(d)]
      d_std <- d %>% select(resp, varSelected)
      d_std[varSelected] <- d_std[varSelected] %>%
        apply(2, function(x) (x - mean(x)) / sd(x))
      Xmean <- d %>%
        select(varSelected) %>%
        colMeans()
      Xsd <- d %>%
        select(varSelected) %>%
        apply(2, sd)
      #
      lmfit <- lm(
        as.formula(paste(resp, "~", paste(varSelected, collapse = " + "))),
        data = d_std
      )
      #
      res <- coef <- lmfit$coefficients
      res[1] <- coef[1] - sum(coef[-1] * Xmean / Xsd)
      res[2:length(coef)] <- coef[-1] / Xsd
      res
    }) %>%
    as.data.frame()
}


qf2pdf <- function(qf = NULL, prob = NULL, breaks = NULL, optns = list()) {
  hist <- qf2hist(qf = qf, prob = prob, breaks = breaks)
  return(CreateDensity(histogram = hist, optns = optns))
}

qf2hist <- function(qf = NULL, prob = NULL, breaks = NULL) { # , tol=1e-2){
  if (is.null(qf)) {
    stop("qf is missing.")
  }
  if (!is.vector(qf)) {
    stop("qf should be a vector.")
  }
  if (!is.numeric(qf)) {
    stop("qf should be a numerical vector.")
  }
  if (is.unsorted(qf)) {
    stop("qf should be an increasingly sorted numerical vector.")
  }
  if (is.null(prob)) {
    prob <- seq(0, 1, length.out = length(qf))
  }
  if (length(prob) != length(qf)) {
    stop("The length of prob should be the same as qf.")
  }
  if (!is.vector(prob) | !is.numeric(prob) | is.unsorted(prob)) {
    stop("prob should be an increasingly sorted numerical vector.")
  }

  # if(min(prob)>tol | max(prob) < 1-tol)
  #  stop("prob should be a vector with minimum 0 and maximum 1.")
  if (is.null(breaks)) {
    breaks <- seq(min(qf), max(qf), length.out = 1e3)
  }
  if (!is.vector(breaks) | !is.numeric(breaks) | is.unsorted(breaks)) {
    stop("breaks should be an increasingly sorted numerical vector.")
  }
  if (min(breaks) > min(qf) | max(breaks) < max(qf)) {
    stop("The range of breaks should cover that of qf.")
  }

  cdf <- approx(x = qf, y = prob, xout = breaks, ties = mean)$y
  if (min(qf) > min(breaks)) cdf[breaks < min(qf)] <- 0
  if (max(breaks) > max(qf)) cdf[breaks > max(qf)] <- 1
  if (sum(cdf > 1)) cdf[cdf > 1] <- 1
  density <- (cdf[-1] - cdf[-length(cdf)])
  counts <- as.integer(density * 1e5)
  density <- density / (breaks[-1] - breaks[-length(breaks)])
  mids <- (breaks[-1] + breaks[-length(breaks)]) / 2
  hist <- list(breaks = breaks, counts = counts, density = density, mids = mids)
  return(hist)
}


GloWassReg <- function(xin, qin, xout, optns = list()) {
  if (is.null(optns$Rsquared)) optns$Rsquared <- FALSE

  if (is.vector(xin)) {
    xin <- as.matrix(xin)
  }
  if (is.vector(xout)) {
    xout <- as.matrix(xout)
  }
  if (nrow(xin) != nrow(qin)) {
    stop("xin and qin should have the same number of rows.")
  }
  if (ncol(xin) != ncol(xout)) {
    stop("xin and xout should have the same number of columns.")
  }
  if (optns$Rsquared & is.null(optns$qSup)) {
    warning("optns$qSup is missing and taking the default value.")
  }

  k <- nrow(xout)
  n <- nrow(xin)
  m <- ncol(qin)
  xbar <- colMeans(xin)
  Sigma <- cov(xin) * (n - 1) / n
  invSigma <- solve(Sigma)

  # if lower & upper are neither NULL
  A <- cbind(diag(m), rep(0, m)) + cbind(rep(0, m), -diag(m))
  if (!is.null(optns$upper) & !is.null(optns$lower)) {
    b0 <- c(optns$lower, rep(0, m - 1), -optns$upper)
  } else if (!is.null(optns$upper)) {
    A <- A[, -1]
    b0 <- c(rep(0, m - 1), -optns$upper)
  } else if (!is.null(optns$lower)) {
    A <- A[, -ncol(A)]
    b0 <- c(optns$lower, rep(0, m - 1))
  } else {
    A <- A[, -c(1, ncol(A))]
    b0 <- rep(0, m - 1)
  }
  Pmat <- as(diag(m), "sparseMatrix")
  Amat <- as(t(A), "sparseMatrix")

  qout <- sapply(1:k, function(j) {
    s <- 1 + t(t(xin) - xbar) %*% invSigma %*% (xout[j, ] - xbar)
    s <- as.vector(s)
    gx <- colMeans(qin * s)

    # res <- do.call(quadprog::solve.QP, list(diag(m), gx, A, b0))
    # return(sort(res$solution)) #return(res$solution)


    res <- do.call(
      osqp::solve_osqp,
      list(P = Pmat, q = -gx, A = Amat, l = b0, pars = osqp::osqpSettings(verbose = FALSE))
    )
    return(sort(res$x))
  })
  qout <- t(qout)

  if (!optns$Rsquared) {
    return(list(qout = qout))
  } else {
    qMean <- colMeans(qin)
    if (k == n) {
      if (sum(abs(xout - xin)) > 1e-10 * length(xout)) {
        qin.est <- qout
      }
      qin.est <- qout
    } else {
      qin.est <- sapply(1:n, function(j) {
        s <- 1 + t(t(xin) - xbar) %*% invSigma %*% (xin[j, ] - xbar)
        s <- as.vector(s)
        gx <- colMeans(qin * s)

        # res <- do.call(quadprog::solve.QP, list(diag(m), gx, A, b0))
        # return(sort(res$solution)) #return(res$solution)

        res <- do.call(
          osqp::solve_osqp,
          list(P = Pmat, q = -gx, A = Amat, l = b0, pars = osqp::osqpSettings(verbose = FALSE))
        )
        return(sort(res$x))
      })
      qin.est <- t(qin.est)
    }
    Rsq <- ifelse(
      is.null(optns$qSup),
      1 - sum(t(qin - qin.est)^2) / sum((t(qin) - qMean)^2),
      1 - pracma::trapz(x = optns$qSup, y = colSums((qin - qin.est)^2)) /
        pracma::trapz(x = optns$qSup, y = rowSums((t(qin) - qMean)^2))
    )
    if (Rsq < 0) Rsq <- 0
    return(list(qout = qout, R.squared = Rsq))
  }
}

GloDenReg <- function(xin = NULL, yin = NULL, hin = NULL, qin = NULL, xout = NULL, optns = list()) {
  if (is.null(optns$Rsquared)) optns$Rsquared <- FALSE
  if (is.null(xin)) {
    stop("xin has no default and must be input by users.")
  }
  if (is.null(yin) & is.null(qin) & is.null(hin)) {
    stop("One of the three arguments, yin, hin and qin, should be input by users.")
  }
  if (is.null(xout)) {
    xout <- xin
  }
  if (!is.null(optns$qSup)) {
    if (min(optns$qSup) != 0 | max(optns$qSup) - 1 != 0) {
      stop("optns$qSup must have minimum 0 and maximum 1.")
    }
    if (sum(duplicated(optns$qSup)) > 0) {
      optns$qSup <- unique(optns$qSup)
      warning("optns$qSup has duplicated elements which has been removed.")
    }
    if (is.unsorted(optns$qSup)) {
      optns$qSup <- sort(optns$qSup)
      warning("optns$qSup has been reordered to be increasing.")
    }
  } else {
    if (!(is.null(yin) & is.null(hin))) {
      if (is.null(optns$nqSup)) {
        optns$nqSup <- 201
      }
      optns$qSup <- seq(0, 1, length.out = optns$nqSup)
    } else {
      if (is.matrix(qin)) {
        optns$qSup <- seq(0, 1, length.out = ncol(qin))
        warning("optns$qSup is missing and is set by default as an equidistant grid on [0,1] with length equal to the number of columns in matrix qin.")
      } else {
        if (is.null(optns$nqSup)) {
          optns$nqSup <- 201
        }
        optns$qSup <- seq(0, 1, length.out = optns$nqSup)
      }
    }
  }
  qSup <- optns$qSup

  optnsRegIdx <- match(c("Rsquared", "lower", "upper", "qSup", "nqSup"), names(optns))
  optnsRegIdx <- optnsRegIdx[!is.na(optnsRegIdx)]
  optnsReg <- optns[optnsRegIdx]

  optnsDen <- optns[-optnsRegIdx]
  if (!is.null(optnsDen$kernelDen)) {
    names(optnsDen)[which(names(optnsDen) == "kernelDen")] <- "kernel"
  }
  if (!is.null(optnsDen$bwDen)) {
    names(optnsDen)[which(names(optnsDen) == "bwDen")] <- "userBwMu"
  }
  # moved to just the last step transforming output quantile to densities
  # don't want a common support before F reg
  # if (!is.null(optnsDen$ndSup))
  #  names(optnsDen)[which(names(optnsDen) == "ndSup")] <- "nRegGrid"
  # if (!is.null(optnsDen$dSup))
  #  names(optnsDen)[which(names(optnsDen) == "dSup")] <- "outputGrid"

  if (!(is.null(yin) & is.null(hin))) {
    # require(fdadensity)

    if (!is.null(yin)) {
      if (!is.null(hin) | !is.null(qin)) {
        warning("hin and qin are redundant when yin is available.")
      }
      if (is.matrix(yin)) {
        yin <- as.data.frame(t(yin))
      }
      if (!is.list(yin)) {
        stop("yin must be a matrix or list.")
      }
      den <- lapply(yin, CreateDensity, optns = optnsDen)
    } else if (!is.null(hin)) {
      if (!is.null(qin)) {
        warning("qin is redundant when hin is available.")
      }
      for (histogram in hin) {
        if (!is.list(histogram)) {
          stop("Each element of hin must be a list.")
        }
        if (is.null(histogram$breaks) & is.null(histogram$mids)) {
          stop("Each element of hin must be a list with at least one of the components breaks or mids.")
        }
        if (is.null(histogram$counts)) {
          stop("Each element of hin must be a list with component counts.")
        }
      }
      den <- lapply(hin, function(histogram) {
        CreateDensity(histogram = histogram, optns = optnsDen)
      })
    }
    qin <- sapply(den, function(deni) {
      fdadensity::dens2quantile(dens = deni$y, dSup = deni$x, qSup = qSup)
    })
    qin <- t(qin)
  } else {
    # if (!is.matrix(qin))
    #  stop ("qin must be a matrix, of which each row holding the values of a quantile function evaluated on a common grid from 0 to 1.")
    if (!is.matrix(qin)) {
      if (!is.list(qin)) {
        stop("qin must be a matrix or list.")
      }
      for (qt in qin) {
        if (!is.list(qt)) {
          stop("If qin is a list, each element must also be a list with two components, x and y.")
        } else if (is.null(qt$x) | is.null(qt$y)) {
          stop("If qin is a list, each element must also be a list with two components, x and y.")
        }
      }
      qin <- sapply(qin, function(q) {
        approx(x = q$x, y = q$y, xout = qSup, rule = 2)$y
      })
      qin <- t(qin)
    }
    den <- apply(qin, 1, function(q) qf2pdf(qf = sort(q), prob = qSup))
  }

  if (is.null(optns$denLowerThreshold)) {
    optns$denLowerThreshold <- 0.001 * mean(qin[, ncol(qin)] - qin[, 1])
  } else if (optns$denLowerThreshold) {
    if (!is.numeric(optns$denLowerThreshold) | optns$denLowerThreshold < 0) {
      optns$denLowerThreshold <- 0.001 * mean(qin[, ncol(qin)] - qin[, 1])
    }
  }

  if (optns$denLowerThreshold) {
    # density thresholding from below
    if (sum(sapply(den, function(d) sum(d$y < optns$denLowerThreshold / diff(range(d$x))))) > 0) {
      den <- lapply(den, function(d) {
        lower <- optns$denLowerThreshold / diff(range(d$x))
        if (sum(d$y < lower) > 0) {
          d$y[d$y < lower] <- lower
          d$y <- d$y / pracma::trapz(d$x, d$y)
        }
        list(x = d$x, y = d$y)
      })
      qin <- sapply(den, function(deni) {
        fdadensity::dens2quantile(dens = deni$y, dSup = deni$x, qSup = qSup)
      })
      qin <- t(qin)
    }
  }

  if (sum(abs(xin - 1)) == 0) {
    # compute the Fréchet mean
    qout <- t(colMeans(qin))
  } else {
    regRes <- GloWassReg(xin = xin, qin = qin, xout = xout, optns = optnsReg)
    qout <- regRes$qout
  }

  if (!is.null(optnsDen$ndSup)) {
    names(optnsDen)[which(names(optnsDen) == "ndSup")] <- "nRegGrid"
  }
  if (!is.null(optnsDen$dSup)) {
    names(optnsDen)[which(names(optnsDen) == "dSup")] <- "outputGrid"
  }

  if (is.null(optnsDen$outputGrid)) {
    dout <- apply(qout, 1, qf2pdf, prob = qSup, optns = optnsDen)
    dout <- lapply(dout, function(d) d[c("x", "y")])
    res <- list(xout = xout, dout = dout, qout = qout, qSup = qSup, xin = xin, din = den, qin = qin, optns = optns)
  } else {
    dSup <- optnsDen$outputGrid
    dout <- apply(qout, 1, function(q) qf2pdf(q, prob = qSup, optns = optnsDen)$y)
    # dout <- apply(qout, 1, qnt2dens, qSup = qSup, dSup = optnsDen$outputGrid)
    dout <- t(dout)
    res <- list(xout = xout, dout = dout, dSup = dSup, qout = qout, qSup = qSup, xin = xin, din = den, qin = qin, optns = optns)
  }
  if (optns$Rsquared & sum(abs(xin - 1)) > 0) res$Rsq <- regRes$R.squared
  class(res) <- "denReg"
  return(res)
}
