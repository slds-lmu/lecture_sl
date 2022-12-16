#################################################################
# Übung zu Schätzen und Testen 1 im WiSe 2013/14
# R-Lösung zur Übung 8 am 10.12.2013
# Autor: Ludwig Bothmann
#################################################################

#################### 
# Aufgabe 17
####################

# Dichte der Doppelexponentialverteilung
ddopexp <- function(x, mu_0=0, sigma_0=1){

	1/(2*sigma_0) * exp(- abs(x-mu_0)/sigma_0)

}

# oder:
library(VGAM)
?dlaplace

# Gitter an x-Werten
x.grid <- seq(-10, 10, length=1000)

# Lokations- und Skalenparameter der DE-Vtlg.
mu_0 <- 0
sigma_0 <- 1


pdf("exercises/information-theory/figure/plots_kld_example.pdf", width=12, height=8)

# Standardabweichung der NV
sd2 <- sqrt(9)
sd1 <- sqrt(1)

# Plot Dichte DE und NV mit den beiden Standardabweichungen
par(mfrow=c(1,1))
plot(x.grid, ddopexp(x.grid, mu_0, sigma_0), type="l", lwd=2, 
	  ylab="", xlab="x", main="Densities")
lines(x.grid, dnorm(x.grid, mean=0, sd=sd1), lty=2, col="blue", lwd=2)
lines(x.grid, dnorm(x.grid, mean=0, sd=sd2), lty=4, col="orange", lwd=2)
legend("topright", c("DE(0,1)",paste("N(0,",round(sd1^2,2),")", sep=""), 
							paste("N(0,",round(sd2^2,2),")", sep="")),
		 lty=c(1,2,4), 
		 col=c("black", "blue", "orange"), lwd=2)

# Plot Log-Dichten
par(mfrow=c(1,1))
plot(x.grid, log(ddopexp(x.grid, mu_0, sigma_0)), type="l", lwd=2, 
	  ylab=" ", xlab="x", main="Log-densities")
lines(x.grid, log(dnorm(x.grid, mean=0, sd=sd1)), lty=2, col="blue", lwd=2)
lines(x.grid, log(dnorm(x.grid, mean=0, sd=sd2)), lty=4, col="orange", lwd=2)
legend("topright", c("DE(0,1)",paste("N(0,",round(sd1^2,2),")", sep=""), 
							paste("N(0,",round(sd2^2,2),")", sep="")),
		 lty=c(1,2,4), 
		 col=c("black", "blue", "orange"), lwd=2)

# Plot Differenz Log-Dichten
par(mfrow=c(1,1))
plot(x.grid, log(ddopexp(x.grid, mu_0, sigma_0)) 
	  - log(dnorm(x.grid, mean=0, sd=sd1)), type="l", lty=2, col="blue", lwd=2, 
	  ylab="", xlab="x", ylim=c(-2,5), main="Difference of the Log-densities")
lines(x.grid, log(ddopexp(x.grid, mu_0, sigma_0)) 
		- log(dnorm(x.grid, mean=0, sd=sd2)), lty=4, col="orange", lwd=2)
abline(h=0)
legend("topright", c(paste("N(0,",round(sd1^2,2),")", sep=""), 
							paste("N(0,",round(sd2^2,2),")", sep="")),
		 lty=c(2,4), 
		 col=c( "blue", "orange"), lwd=2)

# Plot (Differenz der Log-Dichten) * DE-Dichte
par(mfrow=c(1,1))
plot(x.grid, ddopexp(x.grid, mu_0, sigma_0)*(log(ddopexp(x.grid, mu_0, sigma_0)) 
	  - log(dnorm(x.grid, mean=0, sd=sd1))), type="l", lty=2, col="blue", lwd=2, 
	  ylab="", xlab="x", ylim=c(-0.5,1), 
	  main="(Difference of the Log-densities) * DE-density")
lines(x.grid, ddopexp(x.grid, mu_0, sigma_0)*(log(ddopexp(x.grid, mu_0, sigma_0)) 
		- log(dnorm(x.grid, mean=0, sd=sd2))), lty=4, col="orange", lwd=2)
abline(h=0)
legend("topright", c(paste("N(0,",round(sd1^2,2),")", sep=""), 
							paste("N(0,",round(sd2^2,2),")", sep="")),
		 lty=c(2,4), 
		 col=c( "blue", "orange"), lwd=2)

dev.off()




pdf("exercises/information-theory/figure/kld_maxim_example.pdf", width=12, height=8)

Elogf <- function(sigma2, mu, sigma_02, mu_0){
	
	elog <- -1/2*log(2*pi*sigma2) - 1/(2*sigma2)*(2*sigma_02 + (mu - mu_0)^2)

	return(elog)
}

# (Folgende Funktion erleichtert das Plotten)
perspPlot <- function(yMin=0.5, yMax=10, zMin=-3, zMax=3, 
							 sigma_02=1, mu_0=0, seqLength=30,...){
	
	# Gitter von x- und y-Werten
	ygrid <- seq(yMin, yMax, length=seqLength)
	zgrid <- seq(zMin, zMax, length=seqLength)
	
	# z-Matrix aufstellen und füllen
	fmat <- matrix(0, nrow=seqLength, ncol=seqLength)
	for(i in 1:seqLength){
		for(j in 1:seqLength){
			fmat[i,j] <- Elogf(ygrid[i], zgrid[j], sigma_02, mu_0)
		}
	}
	
	# Eigentliches Plotten
	persp(ygrid, zgrid, fmat, xlab="sigma^2", ylab="mu", zlab="E(log(f))", 
			ticktype="detailed", 
			col="lightgray", ...) -> res
	return(res)

}

perspPlot(theta=50, phi=25) -> res
points(trans3d(2, 0, Elogf(2, 0,1,0), pmat=res),
		 col="blue", pch="x", cex=3)

perspPlot(theta=0, phi=16) -> res
points(trans3d(2, 0, Elogf(2, 0,1,0), pmat=res),
		 col="blue", pch="x", cex=3)

perspPlot(theta=-90, phi=15) -> res
points(trans3d(2, 0, Elogf(2, 0,1,0), pmat=res),
		 col="blue", pch="x", cex=3)
dev.off()
# => Funktion in sigma^2 und mu!

pdf("exercises/information-theory/figure/kld_configurations_example.pdf", width=12, height=8)
# Plots der Dichten der DE- und N-Vtlg für verschiedene 
#	Parameterkombinationen
par(mfrow=c(1,1))
plot(x.grid, ddopexp(x.grid, mu_0, sigma_0), type="l", lwd=2, 
	  ylab="", xlab="x", main="Same Variance")
lines(x.grid, dnorm(x.grid, mean=-3, sd=sqrt(2)), lty=4, col="red", lwd=2)
lines(x.grid, dnorm(x.grid, mean=0, sd=sqrt(2)), lty=2, col="blue", lwd=2)
lines(x.grid, dnorm(x.grid, mean=2, sd=sqrt(2)), lty=3, col="orange", lwd=2)
legend("topright", c("DE(0,1)", "N(-3,2)", "N(0,2)", "N(2,2)"), 
		 lty=c(1,4,2,3), 
		 col=c("black", "red", "blue", "orange"), lwd=2)

par(mfrow=c(1,1))
plot(x.grid, ddopexp(x.grid, mu_0, sigma_0), type="l", lwd=2, 
	  ylab="", xlab="x", main="Original and misspecified density, with optimal parameters. ")
lines(x.grid, dnorm(x.grid, mean=0, sd=sqrt(2)), lty=2, col="blue", lwd=2)
legend("topright", c("DE(0,1)","N(0,2)"), 
		 lty=c(1,4,2), 
		 col=c("black", "blue"), lwd=2)

dev.off()

