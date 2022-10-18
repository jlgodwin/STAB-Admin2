smoothDirect <- function (data, Amat, X = NULL, formula = NULL,
                                      time.model = c("rw1", "rw2", "ar1")[2],
                                      st.time.model = NULL, year_label,
                                      year_range = c(1980,2014), is.yearly = TRUE,
                                      m = 5, type.st = 1, survey.effect = FALSE, 
                                      hyper = c("pc", "gamma")[1], pc.u = 1, pc.alpha = 0.01, pc.u.phi = 0.5, 
                                      pc.alpha.phi = 2/3, pc.u.cor = 0.7, pc.alpha.cor = 0.9, pc.st.u = NA, 
                                      pc.st.alpha = NA,
                                      options = list(dic = TRUE,
                                                     mlik = TRUE,cpo = TRUE,
                                                     openmp.strategy = "default"),
                                      control.inla = list(strategy = "adaptive", 
                                                          int.strategy = "auto"),
                                      verbose = FALSE, geo = NULL, 
                                      rw = NULL, ar = NULL, include_time_unstruct = TRUE) 
{
  if (!is.null(geo)) {
    message("Argument geo is deprecated in the smoothDirect function. Only Amat is needed.")
  }
  if (!is.null(rw) || !is.null(ar)) {
    if (is.null(ar)) 
      ar <- 0
    if (is.null(rw)) 
      rw <- 2
    st.rw <- rw
    st.ar <- ar
    is.ar <- ar > 0
    is.main.ar <- rw == 0
    if (rw %in% c(0, 1, 2) == FALSE) 
      stop("Random walk only support rw = 1 or 2.")
    time.model <- paste0("rw", rw)
    if (is.main.ar) 
      time.model <- paste0("ar", ar)
    st.time.model <- paste0("rw", st.rw)
    if (is.ar) 
      st.time.model <- paste0("ar", ar)
    message(paste0("Argument 'rw' and 'ar' have been deprecated in version 1.0.0. They still work as intended for now. In the future, use time.model and st.time.model to specify temporal components\n e.g., time.model = '", 
                   time.model, "', st.time.model = '", st.time.model, 
                   "'"))
  }
  else {
    if (is.null(st.time.model)) 
      st.time.model = time.model
    time.model <- tolower(time.model)
    st.time.model <- tolower(st.time.model)
    is.main.ar = FALSE
    is.ar = FALSE
    ar = 0
    st.ar = 0
    if (time.model == "rw1") {
      rw = 1
    }
    else if (time.model == "rw2") {
      rw = 2
    }
    else if (time.model == "ar1") {
      rw = 1
      ar = 1
      is.main.ar = TRUE
    }
    else {
      stop("Temporal main effect only support rw1, rw2, and ar1.")
    }
    if (st.time.model == "rw1") {
      st.rw = 1
    }
    else if (st.time.model == "rw2") {
      st.rw = 2
    }
    else if (st.time.model == "ar1") {
      st.rw = NULL
      st.ar = 1
      is.ar = TRUE
    }
    else {
      stop("Temporal interaction effect only support rw1, rw2, and ar1.")
    }
  }
  message("----------------------------------", "\nSmoothed Direct Model", 
          "\n  Main temporal model:        ", time.model, appendLF = FALSE)
  if (m == 1) {
    if (is.yearly) 
      message("\n  Temporal resolution:        period model (m = 1)", 
              appendLF = FALSE)
    is.yearly = FALSE
  }
  else if (is.yearly) {
    message(paste0("\n  Temporal resolution:        yearly model (m = ", 
                   m, ")"), appendLF = FALSE)
  }
  else {
    message("\n  Temporal resolution:        period model (m = 1)", 
            appendLF = FALSE)
  }
  if (is.ar && hyper == "gamma") {
    stop("ar1 model only implemented with PC priors for now.")
  }
  if (is.yearly && is.main.ar) {
    stop("ar1 effects are not implemented with is.yearly = TRUE. Consider using rw1 or rw2")
  }
  if (!is.null(Amat)) {
    if (is.null(rownames(Amat))) {
      stop("Row names of Amat needs to be specified to region names.")
    }
    if (is.null(colnames(Amat))) {
      stop("Column names of Amat needs to be specified to region names.")
    }
    if (sum(rownames(Amat) != colnames(Amat)) > 0) {
      stop("Row and column names of Amat needs to be the same.")
    }
    is.spatial <- TRUE
  }
  else {
    is.spatial <- FALSE
    is.ar <- FALSE
  }
  if (is.spatial) 
    message("\n  Spatial effect:             bym2", "\n  Interaction temporal model: ", 
            st.time.model, "\n  Interaction type:           ", 
            type.st, appendLF = FALSE)
  message("\n----------------------------------")
  rate0 <- shape0 <- my.cache <- inla.as.sparse <- type <- NULL
  if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
    stop("You need to install the packages 'INLA'. Please run in your R terminal:\n  install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)")
  }
  if (!is.element("Matrix", (.packages()))) {
    attachNamespace("Matrix")
  }
  if (isTRUE(requireNamespace("INLA", quietly = TRUE))) {
    if (!is.element("INLA", (.packages()))) {
      attachNamespace("INLA")
    }
    tau = exp(10)
    if (!is.spatial) {
      data <- data[which(data$region == "All"), ]
      if (length(data) == 0) {
        stop("Spatial adjacency matrix is not provided and no observation labeled 'All' either.")
      }
    }
    else {
      data <- data[which(data$region != "All"), ]
    }
    priors <- simhyper(R = 2, nsamp = 1e+05, nsamp.check = 5000, 
                       Amat = Amat, nperiod = length(year_label), only.iid = TRUE)
    a.iid <- priors$a.iid
    b.iid <- priors$b.iid
    a.rw <- priors$a.iid
    b.rw <- priors$b.iid
    a.icar <- priors$a.iid
    b.icar <- priors$b.iid
    na.count <- apply(data, 1, function(x) {
      length(which(is.na(x)))
    })
    to_remove <- which(na.count == 6)
    if (length(to_remove) > 0) 
      data <- data[-to_remove, ]
    if (!is.spatial) {
      region_names <- regions <- "All"
      region_count <- S <- 1
      dat <- cbind(data, region_number = 0)
    }
    else {
      region_names <- colnames(Amat)
      region_count <- S <- length(region_names)
      regions <- data.frame(region = region_names, region_number = seq(1, 
                                                                       region_count))
      dat <- merge(data, regions, by = "region")
    }
    dat$region.struct <- dat$region.unstruct <- dat$region.int <- dat$region_number
    if (is.yearly) {
      n <- year_range[2] - year_range[1] + 1
      if (n%%m != 0) 
        stop("The year_range specification is not a multiple of m. Please check that  year_range contains the first and last  year of the periods used. For example, if the last period is 2015-2019, the second element in year_range should be 2019.")
      nn <- n%/%m
      N <- n + nn
      rw.model <- INLA::inla.rgeneric.define(model = rw.new, 
                                             n = n, m = m, order = rw, tau = exp(10), shape0 = a.rw, 
                                             rate0 = b.rw)
      iid.model <- INLA::inla.rgeneric.define(model = iid.new, 
                                              n = n, m = m, tau = exp(10), shape0 = a.iid, 
                                              rate0 = b.iid)
      rw.model.pc <- INLA::inla.rgeneric.define(model = rw.new.pc, 
                                                n = n, m = m, order = rw, tau = exp(10), u0 = pc.u, 
                                                alpha0 = pc.alpha)
      iid.model.pc <- INLA::inla.rgeneric.define(model = iid.new.pc, 
                                                 n = n, m = m, tau = exp(10), u0 = pc.u, alpha0 = pc.alpha)
      if (is.spatial) {
        st.model <- INLA::inla.rgeneric.define(model = st.new, 
                                               n = n, m = m, order = st.rw, S = region_count, 
                                               Amat = Amat, type = type.st, tau = exp(10), 
                                               shape0 = a.iid, rate0 = b.iid)
        st.model.pc <- INLA::inla.rgeneric.define(model = st.new.pc, 
                                                  n = n, m = m, order = st.rw, S = region_count, 
                                                  Amat = Amat, type = type.st, tau = exp(10), 
                                                  u0 = pc.u, alpha0 = pc.alpha)
      }
      year_label_new <- c(as.character(c(year_range[1]:year_range[2])), 
                          year_label)
      time.index <- cbind.data.frame(idx = 1:N, Year = year_label_new)
      constr = list(A = matrix(c(rep(1, n), rep(0, nn)), 
                               1, N), e = 0)
      if (type.st %in% c(2, 4)) {
        tmp <- matrix(0, S, N * S)
        for (i in 1:S) {
          tmp[i, ((i - 1) * n + 1):(i * n)] <- 1
        }
      }
      else {
        tmp <- NULL
      }
      if (type.st %in% c(3, 4)) {
        tmp2 <- matrix(0, n, N * S)
        for (i in 1:n) {
          tmp2[i, which((1:(n * S))%%n == i - 1)] <- 1
        }
      }
      else {
        tmp2 <- NULL
      }
      tmp <- rbind(tmp, tmp2)
      if (is.null(tmp)) {
        constr.st <- NULL
      }
      else {
        constr.st <- list(A = tmp, e = rep(0, dim(tmp)[1]))
      }
      years <- data.frame(year = year_label_new[1:N], year_number = seq(1, 
                                                                        N))
    }
    else {
      n <- 0
      N <- nn <- length(year_label)
      years <- data.frame(year = year_label, year_number = seq(1, 
                                                               N))
    }
    if (is.yearly) {
      dat$time.unstruct <- dat$time.struct <- dat$time.int <- years[match(dat$years, 
                                                                          years[, 1]), 2]
    }
    else {
      dat$time.unstruct <- dat$time.struct <- dat$time.int <- years[match(dat$years, 
                                                                          years[, 1]), 2]
    }
    if (is.yearly) {
      x <- rbind(expand.grid(1:n, 1:region_count), expand.grid((n + 
                                                                  1):N, 1:region_count))
    }
    else {
      x <- expand.grid(1:N, 1:region_count)
    }
    time.area <- data.frame(region_number = x[, 2], time.unstruct = x[, 
                                                                      1], time.area = c(1:nrow(x)))
    if (!is.spatial) {
      time.area$region_number <- 0
    }
    if (survey.effect) {
      if ("survey" %in% colnames(data) == FALSE) 
        stop("survey.effect is set to TRUE, but no survey column in the input data")
      survey_count <- length(table(data$survey))
      if (survey_count <= 1) {
        warning("survey.effect is set to TRUE, but only one survey index is provided, the survey effect has been removed.")
        survey.effect <- FALSE
      }
    }
    newdata <- dat
    if (survey.effect) {
      newdata$survey.id <- match(newdata$survey, unique(newdata$survey))
      survey.table <- data.frame(survey = unique(newdata$survey), 
                                 survey.id = 1:survey_count)
    }
    else {
      newdata$survey.id <- NA
      survey.table <- NULL
    }
    if (is.spatial) {
      newdata <- merge(newdata, time.area, by = c("region_number", 
                                                  "time.unstruct"))
    }
    else {
      newdata$time.area <- NA
    }
    if (!is.null(X)) {
      by <- NULL
      if ("region" %in% colnames(X)) {
        by <- c(by, "region")
      }
      if ("years" %in% colnames(X)) {
        by <- c(by, "years")
      }
      covariate.names <- colnames(X)[colnames(X) %in% by == 
                                       FALSE]
      if (length(covariate.names) == 0) {
        warning("The X argument is specified but no covariate identified.")
        X <- NULL
      }
      if ("region" %in% by && (!"years" %in% by)) {
        for (ii in unique(newdata$region)) {
          which <- which(X$region == ii)
          if (length(which) == 0) {
            stop(paste("Missing region in the covariate matrix:", 
                       ii))
          }
          if (length(which) > 1) 
            stop(paste("Duplicated covariates for region", 
                       ii))
          if (sum(is.na(X[which, ])) > 0) 
            stop("NA in covariate matrix.")
        }
      }
      else if ((!"region" %in% by) && "years" %in% by) {
        for (tt in unique(newdata$years)) {
          which <- which(X$years == tt)
          if (length(which) == 0) {
            stop(paste("Missing years in the covariate matrix:", 
                       tt))
          }
          if (length(which) > 1) 
            stop(paste("Duplicated covariates for period", 
                       tt))
          if (sum(is.na(X[which, ])) > 0) 
            stop("NA in covariate matrix.")
        }
      }
      else if ("region" %in% by && "years" %in% by) {
        for (tt in unique(newdata$years)) {
          for (ii in unique(newdata$region)) {
            which <- intersect(which(X$years == tt), 
                               which(X$region == ii))
            if (length(which) == 0) {
              stop(paste("Missing region-years in the covariate matrix:", 
                         ii, tt))
            }
            if (length(which) > 1) 
              stop(paste("Duplicated covariates for region-years", 
                         ii, tt))
            if (sum(is.na(X[which, ])) > 0) 
              stop("NA in covariate matrix.")
          }
        }
      }
      else {
        stop("Covariate need to contain column 'region' or 'years'.")
      }
    }
    exdat <- newdata
    for (i in 1:N) {
      tmp <- exdat[match(unique(exdat$region), exdat$region), 
                   ]
      tmp$time.unstruct <- tmp$time.struct <- tmp$time.int <- i
      tmp$logit.est <- tmp$logit.prec <- tmp$survey <- tmp$survey.id <- NA
      tmp <- tmp[, colnames(tmp) != "time.area"]
      tmp <- merge(tmp, time.area, by = c("region_number", 
                                          "time.unstruct"))
      tmp$years <- years[i, 1]
      tmp$mean <- tmp$lower <- tmp$upper <- tmp$var.est <- NA
      if ("mean.nohiv" %in% colnames(data)) {
        tmp$mean.nohiv <- tmp$lower.nohiv <- tmp$upper.nohiv <- tmp$var.est.nohiv <- tmp$logit.prec.nohiv <- tmp$logit.est.nohiv <- NA
      }
      exdat <- rbind(exdat, tmp)
    }
    if (!is.null(X)) {
      exdat <- exdat[, colnames(exdat) %in% covariate.names == 
                       FALSE]
      Xnew <- expand.grid(time.struct = 1:N, region.struct = 1:S)
      if (!is.spatial) 
        Xnew$region.struct <- 0
      Xnew[, covariate.names] <- NA
      for (i in 1:dim(Xnew)[1]) {
        tt <- ifelse(Xnew$time.struct[i] > n, Xnew$time.struct[i] - 
                       n, (Xnew$time.struct[i] - 1)%/%m + 1)
        ii <- ifelse(!is.spatial, 1, Xnew$region.struct[i])
        if ("region" %in% by && (!"years" %in% by)) {
          which <- which(X$region == region_names[ii])
        }
        else if ((!"region" %in% by) && "years" %in% 
                 by) {
          which <- which(X$years == year_label[tt])
        }
        else {
          which <- intersect(which(X$years == year_label[tt]), 
                             which(X$region == region_names[ii]))
        }
        Xnew[i, covariate.names] <- X[which, covariate.names]
      }
      exdat <- merge(exdat, Xnew, by = c("time.struct", 
                                         "region.struct"))
    }
    if (is.main.ar) {
      exdat$time.slope <- exdat$time.struct
      center <- (N + 1)/2 + 1e-05
      exdat$time.slope <- (exdat$time.slope - center)/(N - 
                                                         1)
      exdat$region.slope <- exdat$region.struct
    }
    if (is.null(formula)) {
      period.constr <- NULL
      if (tolower(hyper) == "pc") {
        hyperpc1 <- list(prec = list(prior = "pc.prec", 
                                     param = c(pc.u, pc.alpha)))
        pc.st.u <- ifelse(is.na(pc.st.u), pc.u, pc.st.u)
        pc.st.alpha <- ifelse(is.na(pc.st.alpha), pc.alpha, 
                              pc.st.alpha)
        hyperpc1.interact <- list(prec = list(prior = "pc.prec", 
                                              param = c(pc.st.u, pc.st.alpha)))
        hyperpc2 <- list(prec = list(prior = "pc.prec", 
                                     param = c(pc.u, pc.alpha)), phi = list(prior = "pc", 
                                                                            param = c(pc.u.phi, pc.alpha.phi)))
        hyperar1 = list(prec = list(prior = "pc.prec", 
                                    param = c(pc.u, pc.alpha)), theta2 = list(prior = "pc.cor1", 
                                                                              param = c(pc.u.cor, pc.alpha.cor)))
        hyperar2 = list(theta2 = list(prior = "pc.cor1", 
                                      param = c(pc.u.cor, pc.alpha.cor)))
        if (!is.yearly && !is.spatial) {
          formula <- logit.est ~ f(time.struct, model = paste0("rw", 
                                                               rw), constr = TRUE, extraconstr = period.constr, 
                                   hyper = hyperpc1) + f(time.unstruct, model = "iid", 
                                                         hyper = hyperpc1)
        }
        else if (is.yearly && !is.spatial) {
          formula <- logit.est ~ f(time.struct, model = rw.model.pc, 
                                   diagonal = 1e-06, extraconstr = constr, values = 1:N) + 
            f(time.unstruct, model = iid.model.pc)
        }
        else if (!is.yearly && (is.spatial)) {
          formula <- logit.est ~ f(time.struct, model = paste0("rw", 
                                                               rw), constr = TRUE, extraconstr = period.constr, 
                                   hyper = hyperpc1) + f(time.unstruct, model = "iid", 
                                                         hyper = hyperpc1) + f(region.struct, graph = Amat, 
                                                                               model = "bym2", hyper = hyperpc2, scale.model = TRUE, 
                                                                               adjust.for.con.comp = TRUE)
          if (type.st == 1) {
            formula <- update(formula, ~. + f(time.area, 
                                              model = "iid", hyper = hyperpc1.interact))
          }
          else if (type.st == 2) {
            if (!is.ar) {
              formula <- update(formula, ~. + f(region.int, 
                                                model = "iid", group = time.int, control.group = list(model = paste0("rw", 
                                                                                                                     st.rw), scale.model = TRUE), hyper = hyperpc1.interact))
            }
            else {
              formula <- update(formula, ~. + f(region.int, 
                                                model = "iid", hyper = hyperpc1.interact, 
                                                group = time.int, control.group = list(model = "ar", 
                                                                                       order = st.ar, hyper = hyperar2)))
            }
          }
          else if (type.st == 3) {
            formula <- update(formula, ~. + f(region.int, 
                                              model = "besag", graph = Amat, group = time.int, 
                                              control.group = list(model = "iid"), hyper = hyperpc1.interact, 
                                              scale.model = TRUE, adjust.for.con.comp = TRUE))
          }
          else {
            if (is.ar) {
              formula <- update(formula, ~. + f(region.int, 
                                                model = "besag", graph = Amat, group = time.int, 
                                                control.group = list(model = "ar", order = st.ar, 
                                                                     hyper = hyperar2), hyper = hyperpc1.interact, 
                                                scale.model = TRUE, adjust.for.con.comp = TRUE))
            }
            else {
              inla.rw = utils::getFromNamespace("inla.rw", 
                                                "INLA")
              inla.scale.model.bym = utils::getFromNamespace("inla.scale.model.bym", 
                                                             "INLA")
              inla.bym.constr.internal = utils::getFromNamespace("inla.bym.constr.internal", 
                                                                 "INLA")
              R2 <- inla.rw(N, order = st.rw, scale.model = TRUE, 
                            sparse = TRUE)
              R4 = Amat
              if (sum(R4 > 0 & R4 < 1) != 0) {
                for (row in 1:nrow(R4)) {
                  idx <- which(R4[row, ] > 0 & R4[row, 
                                                  ] < 1)
                  R4[row, idx] <- 1
                }
              }
              diag(R4) <- 0
              diag <- apply(R4, 1, sum)
              R4[R4 != 0] <- -1
              diag(R4) <- diag
              Q1 <- inla.bym.constr.internal(R4, adjust.for.con.comp = TRUE)
              R4 <- inla.scale.model.bym(R4, adjust.for.con.comp = TRUE)
              R <- R4 %x% R2
              tmp <- matrix(0, S, N * S)
              for (i in 1:S) {
                tmp[i, ((i - 1) * N + 1):(i * N)] <- 1
              }
              tmp2 <- matrix(0, N * Q1$rankdef, N * S)
              for (j in 1:Q1$rankdef) {
                for (i in 1:N) {
                  this_t_i <- which((1:(N * S))%%N == 
                                      i - 1)
                  this_t_i <- this_t_i[Q1$constr$A[j, 
                                                   ] == 1]
                  tmp2[i + (j - 1) * N, this_t_i] <- 1
                }
              }
              tmp <- rbind(tmp, tmp2)
              constr.st <- list(A = tmp, e = rep(0, dim(tmp)[1]))
              formula <- update(formula, ~. + f(time.area, 
                                                model = "generic0", Cmatrix = R, extraconstr = constr.st, 
                                                rankdef = N * S - (N - st.rw) * (S - 
                                                                                   Q1$rankdef), hyper = hyperpc1.interact))
            }
          }
        }
        else {
          formula <- logit.est ~ f(time.struct, model = rw.model.pc, 
                                   diagonal = 1e-06, extraconstr = constr, values = 1:N) + 
            f(time.unstruct, model = iid.model.pc) + 
            f(region.struct, graph = Amat, model = "bym2", 
              hyper = hyperpc2, scale.model = TRUE, adjust.for.con.comp = TRUE) + 
            f(time.area, model = st.model.pc, diagonal = 1e-06, 
              extraconstr = constr.st, values = 1:(N * 
                                                     S))
        }
        if (survey.effect) {
          formula <- update(formula, ~. + f(survey.id, 
                                            model = "iid", hyper = hyperpc1))
        }
      }
      else if (tolower(hyper) == "gamma") {
        if (!is.yearly && !is.spatial) {
          formula <- logit.est ~ f(time.struct, model = paste0("rw", 
                                                               rw), param = c(a.rw, b.rw), constr = TRUE) + 
            f(time.unstruct, model = "iid", param = c(a.iid, 
                                                      b.iid))
        }
        else if (is.yearly && !is.spatial) {
          formula <- logit.est ~ f(time.struct, model = rw.model, 
                                   diagonal = 1e-06, extraconstr = constr, values = 1:N) + 
            f(time.unstruct, model = iid.model)
        }
        else if (!is.yearly && (is.spatial)) {
          formula <- logit.est ~ f(time.struct, model = paste0("rw", 
                                                               rw), param = c(a.rw, b.rw), scale.model = TRUE, 
                                   extraconstr = period.constr) + f(time.unstruct, 
                                                                    model = "iid", param = c(a.iid, b.iid)) + 
            f(region.struct, graph = Amat, model = "besag", 
              param = c(a.icar, b.icar), scale.model = TRUE, 
              adjust.for.con.comp = TRUE) + f(region.unstruct, 
                                              model = "iid", param = c(a.iid, b.iid))
          if (type.st == 1) {
            formula <- update(formula, ~. + f(time.area, 
                                              model = "iid", param = c(a.iid, b.iid)))
          }
          else if (type.st == 2) {
            formula <- update(formula, ~. + f(region.int, 
                                              model = "iid", group = time.int, control.group = list(model = paste0("rw", 
                                                                                                                   st.rw), scale.model = TRUE), param = c(a.iid, 
                                                                                                                                                          b.iid)))
          }
          else if (type.st == 3) {
            formula <- update(formula, ~. + f(region.int, 
                                              model = "besag", graph = Amat, group = time.int, 
                                              control.group = list(model = "iid"), param = c(a.iid, 
                                                                                             b.iid), scale.model = TRUE, adjust.for.con.comp = TRUE))
          }
          else {
            inla.rw = utils::getFromNamespace("inla.rw", 
                                              "INLA")
            inla.scale.model.bym = utils::getFromNamespace("inla.scale.model.bym", 
                                                           "INLA")
            inla.bym.constr.internal = utils::getFromNamespace("inla.bym.constr.internal", 
                                                               "INLA")
            R2 <- inla.rw(N, order = st.rw, scale.model = TRUE, 
                          sparse = TRUE)
            R4 = Amat
            if (sum(R4 > 0 & R4 < 1) != 0) {
              for (row in 1:nrow(R4)) {
                idx <- which(R4[row, ] > 0 & R4[row, 
                                                ] < 1)
                R4[row, idx] <- 1
              }
            }
            diag(R4) <- 0
            diag <- apply(R4, 1, sum)
            R4[R4 != 0] <- -1
            diag(R4) <- diag
            Q1 <- inla.bym.constr.internal(R4, adjust.for.con.comp = TRUE)
            R4 <- inla.scale.model.bym(R4, adjust.for.con.comp = TRUE)
            R <- R4 %x% R2
            tmp <- matrix(0, S, N * S)
            for (i in 1:S) {
              tmp[i, ((i - 1) * N + 1):(i * N)] <- 1
            }
            tmp2 <- matrix(0, N * Q1$rankdef, N * S)
            for (j in 1:Q1$rankdef) {
              for (i in 1:N) {
                this_t_i <- which((1:(N * S))%%N == i - 
                                    1)
                this_t_i <- this_t_i[Q1$constr$A[j, ] == 
                                       1]
                tmp2[i + (j - 1) * N, this_t_i] <- 1
              }
            }
            tmp <- rbind(tmp, tmp2)
            constr.st <- list(A = tmp, e = rep(0, dim(tmp)[1]))
            formula <- update(formula, ~. + f(time.area, 
                                              model = "generic0", Cmatrix = R, extraconstr = constr.st, 
                                              rankdef = N * S - (N - st.rw) * (S - Q1$rankdef), 
                                              param = c(a.iid, b.iid)))
          }
        }
        else {
          formula <- logit.est ~ f(time.struct, model = rw.model, 
                                   diagonal = 1e-06, extraconstr = constr, values = 1:N) + 
            f(time.unstruct, model = iid.model) + f(region.struct, 
                                                    graph = Amat, model = "besag", param = c(a.icar, 
                                                                                             b.icar), scale.model = TRUE, adjust.for.con.comp = TRUE) + 
            f(region.unstruct, model = "iid", param = c(a.iid, 
                                                        b.iid)) + f(time.area, model = st.model, 
                                                                    diagonal = 1e-06, extraconstr = constr.st, 
                                                                    values = 1:(N * S))
        }
        if (survey.effect) {
          formula <- update(formula, ~. + f(survey.id, 
                                            model = "iid", param = c(a.iid, b.iid)))
        }
      }
      else {
        stop("hyper needs to be either pc or gamma.")
      }
      if (!is.null(X)) {
        formula <- as.formula(paste("logit.est~", as.character(formula)[3], 
                                    "+", paste(covariate.names, collapse = " + ")))
      }
    }
    if (is.main.ar) {
      formula <- update(formula, ~. - f(time.struct, model = paste0("rw", 
                                                                    rw), constr = TRUE, extraconstr = period.constr, 
                                        hyper = hyperpc1) + f(time.struct, model = "ar", 
                                                              order = ar, hyper = hyperar1, extraconstr = period.constr, 
                                                              constr = TRUE) + time.slope)
    }
    mod <- formula
    inla.uncbind0 <- function(A, name.prefix = "col") {
      if (!is.matrix(A)) 
        return(NULL)
      result = list()
      rownames(A) = NULL
      tmp.result = apply(A, 2, function(x) list(x))
      result = sapply(tmp.result, function(x) c(x))
      return(result)
    }
    if (is.spatial) {
      lincombs.info <- data.frame(Index = 1:(region_count * 
                                               N), District = NA, Year = NA)
      index <- 0
      for (j in 1:region_count) {
        for (i in 1:N) {
          index <- index + 1
          time <- rep(NA, N)
          area <- rep(NA, region_count)
          spacetime <- rep(NA, N * region_count)
          space.time.id <- unique(time.area$time.area[time.area$time.unstruct == 
                                                        i & time.area$region_number == j])
          spacetime[space.time.id] <- 1
          time[i] <- 1
          area[j] <- 1
          time.unstruct <- time
          if (!is.null(X)) {
            if ("region" %in% by && (!"years" %in% by)) {
              which <- which(exdat$region.struct == j)
            }
            else if ((!"region" %in% by) && "years" %in% 
                     by) {
              which <- which(exdat$time.struct == i)
            }
            else {
              which <- intersect(which(exdat$time.struct == 
                                         i), which(exdat$region.struct == j))
            }
            sub <- matrix(exdat[which[1], covariate.names], 
                          nrow = 1)
            colnames(sub) <- covariate.names
            XX <- inla.uncbind0(sub)
          }
          else {
            XX <- NULL
          }
          object.name <- paste("lc", index, sep = "")
          lincombs.info[index, c("District", "Year")] <- c(j, 
                                                           i)
          if (is.ar && type.st == 1) {
            if(include_time_unstruct){
              tmplin <- list(`(Intercept)` = 1, 
                             time.area = spacetime, 
                             time.struct = time, 
                             time.unstruct = time, 
                             region.struct = area)
            }else{
              tmplin <- list(`(Intercept)` = 1, 
                             time.area = spacetime, 
                             time.struct = time, 
                             region.struct = area)
            }
            if (is.main.ar) 
              tmplin <- c(tmplin, time.slope = (i - center)/(N - 
                                                               1))
            assign(object.name, INLA::inla.make.lincomb(c(tmplin, 
                                                          XX)))
          }
          else if (is.ar) {
            if(include_time_unstruct){
              tmplin <- list(`(Intercept)` = 1, region.int = area, 
                             time.struct = time, time.unstruct = time, 
                             region.struct = area)
            }else{
              tmplin <- list(`(Intercept)` = 1, region.int = area, 
                             time.struct = time, 
                             region.struct = area)
            }
            if (is.main.ar) 
              tmplin <- c(tmplin, time.slope = (i - center)/(N - 
                                                               1))
            assign(object.name, INLA::inla.make.lincomb(c(tmplin, 
                                                          XX)))
          }
          else if (is.yearly || type.st %in% c(1, 4)) {
            if(include_time_unstruct){
              assign(object.name, INLA::inla.make.lincomb(c(list(`(Intercept)` = 1, 
                                                                 time.area = spacetime, 
                                                                 time.struct = time, 
                                                                 time.unstruct = time, 
                                                                 region.struct = area), 
                                                            XX)))
            }else{
              assign(object.name, INLA::inla.make.lincomb(c(list(`(Intercept)` = 1, 
                                                                 time.area = spacetime, 
                                                                 time.struct = time, 
                                                                 region.struct = area), 
                                                            XX)))
            }
          }
          else {
            newidx <- rep(0, j + (i - 1) * region_count)
            newidx[j + (i - 1) * region_count] <- 1
            if(include_time_unstruct){
              assign(object.name, INLA::inla.make.lincomb(c(list(`(Intercept)` = 1, 
                                                                 time.struct = time,
                                                                 time.unstruct = time, 
                                                                 region.struct = area,
                                                                 region.int = newidx), 
                                                            XX)))
            }else{
              assign(object.name, INLA::inla.make.lincomb(c(list(`(Intercept)` = 1, 
                                                                 time.struct = time, 
                                                                 region.struct = area,
                                                                 region.int = newidx), 
                                                            XX)))
            }
          }
          if (index == 1) {
            lincombs.fit <- get(object.name)
            names(lincombs.fit)[index] <- object.name
          }
          else {
            tmp <- get(object.name)
            lincombs.fit <- c(lincombs.fit, tmp)
            names(lincombs.fit)[index] <- object.name
          }
        }
      }
    }
    else {
      lincombs.info <- data.frame(Index = 1:N, District = NA, 
                                  Year = NA)
      index <- 0
      for (i in 1:N) {
        index <- index + 1
        time <- rep(NA, N)
        time[i] <- 1
        time.unstruct <- time
        if (!is.null(X)) {
          if ("region" %in% by && (!"years" %in% by)) {
            which <- which(exdat$region.struct == 0)
          }
          else if ((!"region" %in% by) && "years" %in% 
                   by) {
            which <- which(exdat$time.struct == i)
          }
          else {
            which <- intersect(which(exdat$time.struct == 
                                       i), which(exdat$region.struct == 0))
          }
          sub <- matrix(exdat[which[1], covariate.names], 
                        nrow = 1)
          colnames(sub) <- covariate.names
          XX <- inla.uncbind0(sub)
        }
        else {
          XX <- NULL
        }
        object.name <- paste("lc", index, sep = "")
        lincombs.info[index, c("District", "Year")] <- c(0, 
                                                         i)
        if (rw == 1) {
          if(include_time_unstruct){
            assign(object.name, INLA::inla.make.lincomb(c(list(`(Intercept)` = 1, 
                                                               time.struct = time,
                                                               time.unstruct = time), 
                                                          XX)))
          }else{
            assign(object.name, INLA::inla.make.lincomb(c(list(`(Intercept)` = 1, 
                                                               time.struct = time), 
                                                          XX)))
          }
         }
        else {
          if(include_time_unstruct){
            assign(object.name, INLA::inla.make.lincomb(c(list(`(Intercept)` = 1, 
                                                               time.struct = time,
                                                               time.unstruct = time), 
                                                          XX)))
          }else{
            assign(object.name, INLA::inla.make.lincomb(c(list(`(Intercept)` = 1, 
                                                               time.struct = time), 
                                                          XX)))
          }
        }
        if (index == 1) {
          lincombs.fit <- get(object.name)
          names(lincombs.fit)[index] <- object.name
        }
        else {
          lincombs.fit <- c(lincombs.fit, get(object.name))
          names(lincombs.fit)[index] <- object.name
        }
      }
    }
    fit <- INLA::inla(mod, family = "gaussian", control.compute = options, 
                      data = exdat, control.predictor = list(compute = TRUE), 
                      control.family = list(hyper = list(prec = list(initial = log(1), 
                                                                     fixed = TRUE))), scale = exdat$logit.prec, lincomb = lincombs.fit, 
                      control.inla = control.inla, verbose = verbose)
    return(list(model = mod, fit = fit, Amat = Amat, newdata = exdat, 
                time = seq(0, N - 1), area = seq(0, region_count - 
                                                   1), time.area = time.area, survey.table = survey.table, 
                a.iid = a.iid, b.iid = b.iid, a.rw = a.rw, b.rw = b.rw, 
                a.rw = a.rw, b.rw = b.rw, a.icar = a.icar, b.icar = b.icar, 
                lincombs.info = lincombs.info, is.yearly = is.yearly, 
                type.st = type.st, year_range = year_range, year_label = year_label, 
                Amat = Amat, has.Amat = TRUE))
  }
}
