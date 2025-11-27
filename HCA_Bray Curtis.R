# ==========================================================
# FINAL HCA EXPORT: Bray–Curtis, site-wise
# Outputs: SVG, PDF, PNG (600 dpi)
# ==========================================================

# Make sure this is at the top of your HCA script:
# suppressPackageStartupMessages({
#   library(dendextend)
#   # library(vegan)  # for vegdist, used earlier
# })

# 'hc' should already be created earlier, e.g.:
# bc_dist <- vegdist(dat_scaled, method = "bray")
# hc      <- hclust(bc_dist, method = "ward.D2")

# ---- 1) Prepare publication dendrogram object ----
w_in <- 7.09   # Water Research double-column width
h_in <- 5.5    # Approximate figure height

dend_pub <- as.dendrogram(hc) |>
  set("branches_col", "black") |>
  set("branches_lwd", 1.1) |>
  set("labels_col",   "black") |>
  set("labels_cex",   1.0)

# Common plotting parameters
plot_par <- list(mar = c(5, 20, 1, 2), family = "sans")

# ---- 2) SVG (vector) ----
svg("HCA_SiteWise_BrayCurtis_pub.svg", width = w_in, height = h_in, family = "sans")
par(plot_par)
plot(
  dend_pub,
  horiz   = TRUE,
  leaflab = "perpendicular",
  xlab    = "",
  main    = ""
)
axis(side = 1, lwd = 0.8, cex.axis = 0.8)
title(xlab = "Height (Bray–Curtis dissimilarity)", cex.lab = 1.0)
rect.dendrogram(dend_pub, k = 3, border = "black", lwd = 1.1, horiz = TRUE)
dev.off()

# ---- 3) PDF (vector) ----
pdf("HCA_SiteWise_BrayCurtis_pub.pdf", width = w_in, height = h_in, family = "sans")
par(plot_par)
plot(
  dend_pub,
  horiz   = TRUE,
  leaflab = "perpendicular",
  xlab    = "",
  main    = ""
)
axis(side = 1, lwd = 0.8, cex.axis = 0.8)
title(xlab = "Height (Bray–Curtis dissimilarity)", cex.lab = 1.0)
rect.dendrogram(dend_pub, k = 3, border = "black", lwd = 1.1, horiz = TRUE)
dev.off()

# ---- 4) PNG (600 dpi) ----
png("HCA_SiteWise_BrayCurtis_pub.png",
    width = w_in * 600, height = h_in * 600, res = 600, family = "sans")
par(plot_par)
plot(
  dend_pub,
  horiz   = TRUE,
  leaflab = "perpendicular",
  xlab    = "",
  main    = ""
)
axis(side = 1, lwd = 0.8, cex.axis = 0.8)
title(xlab = "Height (Bray–Curtis dissimilarity)", cex.lab = 1.0)
rect.dendrogram(dend_pub, k = 3, border = "black", lwd = 1.1, horiz = TRUE)
dev.off()
