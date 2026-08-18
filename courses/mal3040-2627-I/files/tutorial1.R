# ===========================================================================
#  MAL3040 -- Statistics (B.Sc. B.Ed.)
#  Reading Data from Pictures: reproduce every exhibit from the tutorial.
#
#  Base R only. No packages to install. Run the whole file, or run one
#  exhibit at a time, e.g.   source("tutorial.R"); ex03_mean_vs_median()
#
#  A NOTE ON REPRODUCIBILITY. The slides were generated in Python; R uses a
#  different random number generator, so the individual observations here will
#  NOT be identical to the ones on the slides. That is deliberate, and it is
#  the interesting part: the CONCLUSIONS are all still true. Better still,
#  every quantity that was *imposed* by construction -- the equal means and
#  correlations of the quartet, the identical five-number summaries, the exact
#  values of r in the guessing grid -- comes out exactly right in R as well,
#  because those are forced algebraically and owe nothing to the random draw.
#
#  Change the seed on the next line and run again. Ask yourself each time:
#  which of my conclusions survived, and which were an accident of this sample?
# ===========================================================================

set.seed(20260817)

BLUE <- "#2b6cb0"; ORANGE <- "#dd6b20"; GREEN <- "#2f855a"
RED  <- "#c53030"; GREY   <- "#4a5568"

# CV is meaningless when the mean sits at (or near) zero -- see the exhibit on
# standardisation, where Z has mean exactly 0. Return NA rather than nonsense.
cv <- function(x) if (abs(mean(x)) < 1e-8) NA_real_ else 100 * sd(x) / mean(x)

# --- the device used by three of the exhibits ------------------------------
# Given paired data (u,v), return new data with EXACTLY the prescribed mean,
# standard deviation and correlation, while keeping the shape of the scatter.
# z1 is the standardised u; z2 is the standardised residual of v after the
# part of it lying along z1 has been removed. Then z1 and z2 each have mean 0
# and variance 1, and their correlation is 0, so the combination below has the
# means, standard deviations and correlation we asked for.
impose <- function(u, v, xbar, sx, ybar, sy, r) {
  z1 <- (u - mean(u)) / sd(u)
  w  <- v - mean(v)
  w  <- w - sum(w * z1) / sum(z1 * z1) * z1
  z2 <- w / sd(w)
  list(x = xbar + sx * z1,
       y = ybar + sy * (r * z1 + sqrt(1 - r^2) * z2))
}

# Report the summaries of a scatter, so you can check the construction worked.
describe_xy <- function(x, y, label = "") {
  cat(sprintf("%-12s n=%3d  xbar=%7.3f  sx=%6.3f  ybar=%7.3f  sy=%6.3f  r=%7.4f\n",
              label, length(x), mean(x), sd(x), mean(y), sd(y), cor(x, y)))
}

describe <- function(x, label = "") {
  q <- quantile(x, c(0, .25, .5, .75, 1))
  cat(sprintf(paste0("%-22s n=%3d  min=%6.2f  Q1=%6.2f  med=%6.2f  Q3=%6.2f  ",
                     "max=%6.2f  mean=%7.3f  sd=%6.3f  CV=%6.2f%%\n"),
              label, length(x), q[1], q[2], q[3], q[4], q[5],
              mean(x), sd(x), cv(x)))
}

# ===========================================================================
# 1. The axis is a choice
# ===========================================================================
ex01_axis <- function() {
  cats <- c("A", "B", "C", "D", "E")
  vals <- c(96, 98, 99, 101, 102)

  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  barplot(vals, names.arg = cats, col = BLUE, ylim = c(95, 103), xpd = FALSE,
          main = "Plot 1", xlab = "Centre", ylab = "Mean marks")
  barplot(vals, names.arg = cats, col = BLUE, ylim = c(0, 115),
          main = "Plot 2", xlab = "Centre", ylab = "Mean marks")
  par(op)

  cat("\nThe same five numbers:", vals, "\n")
  cat("A to E is a gap of", max(vals) - min(vals), "marks, i.e.",
      round(100 * (max(vals) - min(vals)) / min(vals), 1), "% of A.\n")
  cat("Try replacing ylim = c(95, 103) by other ranges. Which range is honest?\n")
}

# ===========================================================================
# 2. Bin width
# ===========================================================================
ex02_bins <- function() {
  x <- c(rnorm(150, 38, 5.5), rnorm(150, 66, 5.5))

  op <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  for (w in c(20, 8, 1.5)) {
    hist(x, breaks = seq(floor(min(x)) - w, ceiling(max(x)) + w, by = w),
         col = BLUE, border = "white", xlab = "Score",
         main = paste("Bin width", w))
  }
  par(op)

  describe(x, "bimodal scores")
  cat("Students within 3 marks of the mean:",
      sum(abs(x - mean(x)) <= 3), "out of", length(x), "\n")
  cat("Change the bin widths. Which features survive, and which come and go?\n")
}

# ===========================================================================
# 3. Mean against median
# ===========================================================================
ex03_mean_vs_median <- function() {
  inc  <- round(rlnorm(200, meanlog = log(24), sdlog = 0.45), 1)
  inc2 <- c(inc, 400)

  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  for (i in 1:2) {
    d <- if (i == 1) inc else inc2
    hist(d, breaks = seq(0, 420, by = 6), xlim = c(0, 120), col = BLUE,
         border = "white", xlab = "Monthly income (thousand Rs.)",
         main = if (i == 1) "Before" else "After")
    abline(v = mean(d),   col = RED,   lwd = 2)
    abline(v = median(d), col = GREEN, lwd = 2, lty = 2)
    legend("topright", bty = "n", lwd = 2, lty = c(1, 2),
           col = c(RED, GREEN),
           legend = c(sprintf("mean = %.1f",   mean(d)),
                      sprintf("median = %.1f", median(d))))
  }
  par(op)

  describe(inc,  "before")
  describe(inc2, "after")
  # how much of the total sum of squares does the one household account for?
  ss <- sum((inc2 - mean(inc2))^2)
  cat(sprintf("The single value 400 supplies %.1f%% of the total sum of squares.\n",
              100 * (400 - mean(inc2))^2 / ss))
}

# ===========================================================================
# 4. Reading a box plot
# ===========================================================================
ex04_boxplot_read <- function() {
  A <- rnorm(90, 62, 8)
  B <- 95 - rgamma(90, shape = 4.0, scale = 5.0)
  C <- c(30 + rgamma(90, shape = 2.2, scale = 7.0), 118, 124)
  g <- list("Section A" = A, "Section B" = B, "Section C" = C)

  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  boxplot(g, col = "#bee3f8", border = GREY, medlwd = 2,
          outcol = ORANGE, outpch = 19, ylab = "Marks", main = "Box plots")
  plot(NA, xlim = c(0.5, 3.5), ylim = range(unlist(g)), xaxt = "n",
       xlab = "", ylab = "Marks", main = "The same data, every point shown")
  axis(1, at = 1:3, labels = names(g))
  for (k in 1:3)
    points(jitter(rep(k, length(g[[k]])), amount = .16), g[[k]],
           col = adjustcolor(BLUE, .6), pch = 19, cex = .6)
  par(op)

  for (k in 1:3) describe(g[[k]], names(g)[k])
  for (k in 1:3)
    cat(sprintf("%-12s IQR = %6.3f   sd = %6.3f\n",
                names(g)[k], IQR(g[[k]]), sd(g[[k]])))
  cat("Compare mean with median in each group. Which way does the tail lie?\n")
}

# ===========================================================================
# 5. Three data sets with identical box plots
# ===========================================================================
# The trick: take 401 evenly spaced quantiles of some distribution, then apply
# an INCREASING piecewise-linear map that sends that distribution's own
# five-number summary onto (10, 30, 50, 70, 90). An increasing map does not
# reorder the data, so the quartiles are guaranteed to land where we want.
ex05_boxplot_same <- function() {
  FIVE <- c(10, 30, 50, 70, 90)
  n <- 401
  p <- (seq_len(n) - 0.5) / n

  remap <- function(v) {
    v <- sort(v)
    approx(v[c(1, 101, 201, 301, 401)], FIVE, xout = v)$y
  }

  p200  <- (seq_len(200) - 0.5) / 200
  flat  <- p                                            # uniform
  humps <- c(qnorm(p200, -1, 0.3), 0, qnorm(p200, 1, 0.3))  # two bumps
  ends  <- sin(pi * p / 2)^2                            # arcsine: piled at ends

  sets <- list("P (flat)"              = remap(flat),
               "Q (two humps)"         = remap(humps),
               "R (piled at the ends)" = remap(ends))

  op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
  for (k in 1:3)
    hist(sets[[k]], breaks = seq(8, 92, by = 4), xlim = c(5, 95),
         ylim = c(0, 130), col = BLUE, border = "white",
         xlab = "Value", main = names(sets)[k])
  for (k in 1:3)
    boxplot(sets[[k]], horizontal = TRUE, ylim = c(5, 95), col = "#bee3f8",
            border = GREY, medcol = RED, medlwd = 2, xlab = "Value")
  par(op)

  for (k in 1:3) describe(sets[[k]], names(sets)[k])
  cat("\nObservations in each quarter (all three sets should read 100):\n")
  for (k in 1:3) {
    d <- sets[[k]]
    cat(sprintf("%-22s %s\n", names(sets)[k],
                paste(sapply(list(c(10,30), c(30,50), c(50,70), c(70,90)),
                             function(b) sum(d > b[1] & d <= b[2])),
                      collapse = "  ")))
  }
  cat("\nWhy is that forced, and not a coincidence?\n")
}

# ===========================================================================
# 6. y = a x + b
# ===========================================================================
ex06_linear <- function(a = 1.8, b = 32) {
  X <- rnorm(250, 24, 4)
  Y <- a * X + b
  Z <- (X - mean(X)) / sd(X)

  op <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  hist(X, breaks = 18, col = BLUE, border = "white", xlab = "deg C",
       main = "X"); abline(v = mean(X), col = RED, lwd = 2)
  hist(Y, breaks = 18, col = BLUE, border = "white", xlab = "deg F",
       main = sprintf("Y = %.1f X + %g", a, b)); abline(v = mean(Y), col = RED, lwd = 2)
  hist(Z, breaks = 18, col = BLUE, border = "white", xlab = "standard units",
       main = "Z = (X - mean)/sd"); abline(v = mean(Z), col = RED, lwd = 2)
  par(op)

  describe(X, "X (deg C)"); describe(Y, "Y (deg F)"); describe(Z, "Z")
  cat(sprintf("\nCheck: a*mean(X)+b = %.6f, mean(Y) = %.6f\n", a * mean(X) + b, mean(Y)))
  cat(sprintf("Check: |a|*sd(X)   = %.6f, sd(Y)   = %.6f\n", abs(a) * sd(X), sd(Y)))
  cat(sprintf("Check: cor(X, Y)   = %+.6f   (sign follows the sign of a)\n", cor(X, Y)))
  cat(sprintf("CV of X = %.2f%%, CV of Y = %.2f%%  -- and yet nothing changed.\n",
              cv(X), cv(Y)))
  cat("Now run ex06_linear(a = -1.8) and see which of these survive.\n")
}

# ===========================================================================
# 7. Coefficient of variation
# ===========================================================================
ex07_cv <- function() {
  A <- rnorm(120, 100, 4)   # bolts, mm
  B <- rnorm(120, 25,  2)   # pins, mm

  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  for (k in 1:2) {
    d <- if (k == 1) A else B
    col <- if (k == 1) BLUE else ORANGE
    plot(d, runif(length(d), -.4, .4), pch = 19, cex = .6,
         col = adjustcolor(col, .65), yaxt = "n", ylab = "",
         xlim = mean(d) + c(-5, 5) * sd(d), xlab = "Length (mm)",
         main = if (k == 1) "Machine A (bolts)" else "Machine B (pins)")
    abline(v = mean(d), col = RED, lwd = 2)
  }
  par(op)

  describe(A, "Machine A"); describe(B, "Machine B")
  cat("\nEach panel is drawn five standard deviations either side of its OWN mean.\n")
  cat("Redraw both on a common scale -- xlim = c(0, 115) -- and look again.\n")
}

# ===========================================================================
# 8. Guess the correlation
# ===========================================================================
ex08_guess_r <- function(targets = c(0.95, 0.60, 0.20, -0.45, -0.85, 0.00)) {
  labs <- c("I", "J", "K", "L", "M", "N")
  op <- par(mfrow = c(2, 3), mar = c(2, 2, 3, 1))
  out <- numeric(6)
  for (k in seq_along(targets)) {
    d <- impose(rnorm(70), rnorm(70), 50, 10, 50, 10, targets[k])
    out[k] <- cor(d$x, d$y)
    plot(d$x, d$y, pch = 19, cex = .6, col = BLUE, xaxt = "n", yaxt = "n",
         xlim = c(15, 85), ylim = c(15, 85), xlab = "", ylab = "",
         main = paste("Panel", labs[k]))
  }
  par(op)
  cat("\nWrite your guesses down before running the next line.\n")
  for (k in 1:6) cat(sprintf("Panel %s : r = %+.4f\n", labs[k], out[k]))
  cat("\nNote these are exact, not approximate: impose() forces them.\n")
}

# ===========================================================================
# 9. Three data sets with r = 0
# ===========================================================================
ex09_r_zero <- function() {
  th <- seq(0, 2 * pi, length.out = 81)[1:80]
  t2 <- seq(-3, 3, length.out = 80)
  t3 <- seq(0, 6, length.out = 80)

  sets <- list(S = impose(cos(th), sin(th), 0, 1, 0, 1, 0),
               T = impose(t2, t2^2 + rnorm(80, 0, .3), 0, 1, 0, 1, 0),
               U = impose(t3, sin(t3 * 1.6) + rnorm(80, 0, .12), 0, 1, 0, 1, 0))

  op <- par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))
  for (k in 1:3) {
    d <- sets[[k]]
    plot(d$x, d$y, pch = 19, cex = .6, col = ORANGE, xaxt = "n", yaxt = "n",
         xlab = "x", ylab = "y",
         main = sprintf("Panel %s:  r = %.3f", names(sets)[k], cor(d$x, d$y)))
    abline(h = 0, col = GREY, lty = 2)
  }
  par(op)

  # a linear fit is blind to all of this; the residuals are not.
  d <- sets$T
  cat("\nPanel T: r =", round(cor(d$x, d$y), 4),
      "but the correlation of y with x^2 is",
      round(cor(d$x^2, d$y), 4), "\n")
  cat("Try: plot(d$x, residuals(lm(d$y ~ d$x))) -- the curvature is unmistakable.\n")
}

# ===========================================================================
# 10. One point
# ===========================================================================
ex10_one_point <- function() {
  d  <- impose(runif(40, 10, 30), runif(40, 10, 30), 20, 5.5, 20, 5.5, 0.02)
  x2 <- c(d$x, 90); y2 <- c(d$y, 90)

  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  plot(d$x, d$y, pch = 19, cex = .7, col = BLUE, xlim = c(0, 100),
       ylim = c(0, 100), xlab = "x", ylab = "y",
       main = sprintf("40 points:  r = %.3f", cor(d$x, d$y)))
  plot(x2, y2, pch = 19, cex = .7, col = BLUE, xlim = c(0, 100),
       ylim = c(0, 100), xlab = "x", ylab = "y",
       main = sprintf("41 points:  r = %.3f", cor(x2, y2)))
  points(90, 90, pch = 18, cex = 1.8, col = RED)
  par(op)

  cat(sprintf("\nr before = %.4f, r after = %.4f\n", cor(d$x, d$y), cor(x2, y2)))
  cat(sprintf("median of x before = %.3f, after = %.3f\n", median(d$x), median(x2)))
  cat("\nDelete each point in turn and see how much r depends on any one of them:\n")
  r_drop <- sapply(seq_along(x2), function(i) cor(x2[-i], y2[-i]))
  cat("largest r after deleting one point:", round(max(r_drop), 3),
      " smallest:", round(min(r_drop), 3), "\n")
}

# ===========================================================================
# 11. Pooling three groups
# ===========================================================================
ex11_pooling <- function() {
  cx <- c(20, 35, 50); cy <- c(55, 68, 81)
  xs <- ys <- vector("list", 3)
  for (k in 1:3) {
    xs[[k]] <- rnorm(45, cx[k], 3.2)
    ys[[k]] <- cy[k] - 0.55 * (xs[[k]] - cx[k]) + rnorm(45, 0, 1.6)
  }
  X <- unlist(xs); Y <- unlist(ys)

  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  plot(X, Y, pch = 19, cex = .6, col = GREY, xlab = "x", ylab = "y",
       main = sprintf("All data pooled:  r = %.3f", cor(X, Y)))
  abline(lm(Y ~ X), col = RED, lwd = 2)
  cols <- c(BLUE, ORANGE, GREEN)
  plot(X, Y, type = "n", xlab = "x", ylab = "y", main = "Split by group")
  for (k in 1:3) {
    points(xs[[k]], ys[[k]], pch = 19, cex = .6, col = cols[k])
    abline(lm(ys[[k]] ~ xs[[k]]), col = cols[k], lwd = 2)
  }
  legend("topleft", bty = "n", pch = 19, col = cols,
         legend = paste("Group", 1:3))
  par(op)

  cat(sprintf("\npooled r = %+.3f\n", cor(X, Y)))
  for (k in 1:3)
    cat(sprintf("group %d r = %+.3f\n", k, cor(xs[[k]], ys[[k]])))
  cat("\nBoth are correct. They answer different questions.\n")
}

# ===========================================================================
# 12. Four data sets with identical summaries
# ===========================================================================
ex12_quartet <- function(XB = 9, SX = 3, YB = 7.5, SY = 2, R = 0.8) {
  n <- 60
  u <- seq(3, 15, length.out = n)

  uc <- c(seq(5, 12, length.out = n - 1), 8.5)
  vc <- c(seq(5, 12, length.out = n - 1) + rnorm(n - 1, 0, .18), 1)
  ud <- c(rnorm(n / 2, 5, 1),   rnorm(n / 2, 13, 1))
  vd <- c(rnorm(n / 2, 4.5, 1), rnorm(n / 2, 10.5, 1))

  sets <- list(A = impose(u,  u + rnorm(n, 0, 1.6), XB, SX, YB, SY, R),
               B = impose(u,  -(u - 9)^2,           XB, SX, YB, SY, R),
               C = impose(uc, vc,                   XB, SX, YB, SY, R),
               D = impose(ud, vd,                   XB, SX, YB, SY, R))

  b1 <- R * SY / SX; b0 <- YB - b1 * XB
  xr <- range(sapply(sets, function(d) d$x))
  yr <- range(sapply(sets, function(d) d$y))

  op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  for (k in 1:4) {
    d <- sets[[k]]
    plot(d$x, d$y, pch = 19, cex = .7, col = BLUE, xlim = xr, ylim = yr,
         xlab = "x", ylab = "y", main = paste("Data set", names(sets)[k]))
    abline(b0, b1, col = GREY, lwd = 1.3, lty = 2)
  }
  par(op)

  cat("\n")
  for (k in 1:4) describe_xy(sets[[k]]$x, sets[[k]]$y, paste("Set", names(sets)[k]))

  # two things the table cannot tell you
  C <- sets$C; i <- which.min(C$y)
  cat(sprintf("\nSet C: r with all 60 points = %.4f; with the low point removed = %.4f\n",
              cor(C$x, C$y), cor(C$x[-i], C$y[-i])))
  D <- sets$D; lo <- D$x < mean(D$x)
  cat(sprintf("Set D: r within the two clusters = %+.3f and %+.3f, pooled = %+.3f\n",
              cor(D$x[lo], D$y[lo]), cor(D$x[!lo], D$y[!lo]), cor(D$x, D$y)))
}

# ===========================================================================
run_all <- function() {
  for (f in list(ex01_axis, ex02_bins, ex03_mean_vs_median, ex04_boxplot_read,
                 ex05_boxplot_same, ex06_linear, ex07_cv, ex08_guess_r,
                 ex09_r_zero, ex10_one_point, ex11_pooling, ex12_quartet)) {
    f()
    if (interactive()) readline("press Enter for the next exhibit... ")
  }
}

# Uncomment to run everything at once:
# run_all()
