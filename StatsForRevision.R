## Stats for revision of OAs SR MS

## Mark Farrell, 26/11/20

#### Correlations ####

# Packages
install.packages("PerformanceAnalytics")
install.packages("corrplot")

library(tidyverse)
library(janitor)
library(PerformanceAnalytics)
library(corrplot)

# Data in and clean
chem <- read_csv("CorrelChem.csv") %>% remove_empty()
resp <- read_csv("CorrelResp.csv") %>% remove_empty()

dat <- left_join(resp, chem)

dat_na <- dat %>% drop_na()

# Base(ish) http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
chart.Correlation(dat_na[, 2:17], histogram = TRUE, pch = 19)
pairs(dat_na[, 2:17], pch = 19)

# Correlograms http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
M <- cor(dat_na[, 2:17])

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(dat_na[, 2:17])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

setEPS()
postscript("SI1.eps", height = 10, width = 10)

corrplot(M, method="color", col=col(200),  
         type="lower", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
dev.off()

tiff("SI1.tiff", units="in", width=10, height=10, res=300)
corrplot(M, method="color", col=col(200),  
         type="lower", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
dev.off()
