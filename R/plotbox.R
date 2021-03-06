#'@title plotbox
#'
#'@description get a boxplot, showing the mean in plot
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export plotbox
#'@keywords plotbox
#'@return a plotbox (a elegant boxplot)
#'@export plotbox
#'
#'@examples
#'x<-rnorm(25,2,3)
#'plotbox(x)
#'

plotbox <- function(x) {
  require(graphics)
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow=c(1,1))
  ylab <- deparse(substitute(x))  # get the expression passed as y
  boxplot(x,main=paste("Boxplot of", ylab, NULL), xlab=NULL,
          ylab=ylab, cex=.9, boxwex = .55,col = "#EBEBEB")
  Mean<-round(mean(x, na.rm = TRUE), 3)
  Median <- round(median(x, na.rm = TRUE), 3)
  Max <- round(max(x, na.rm = TRUE), 3)
  Min <- round(min(x, na.rm = TRUE), 3)
  Q1<-round(quantile(x, 0.25, na.rm = TRUE), 3)
  Q3<-round(quantile(x, 0.75, na.rm = TRUE), 3)
  points(mean(x), pch=20, cex = 1.8, col="black")
  text((Mean), "Mean", col="#918686", font=8, cex = .7, adj=-5)
  text((Median), "Median", col="#918686", font=8, cex = .7, adj=-3.7)
  text((Min), "Min", col="#918686", font=8, cex = .7, adj=-3.7)
  text((Max), "Max", col="#918686", font=8, cex = .7, adj=-3.7)
  text((Q1), "Q1", col="#918686", font=7, cex = .5, adj=-10)
  text((Q3), "Q3", col="#918686", font=7, cex = .5, adj=-10)
  summary(x)
}

