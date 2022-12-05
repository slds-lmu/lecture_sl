#################################################################
# Graphics for IC Exercise sheet 8: VC Dimension
# Autor: Patricio Massaro
#################################################################

pdf("exercises/hypospaces-capacity/figure/3points_impossible_assignment.pdf", width=12, height=8)

# Draw a 3 points in a horizontal line in a 2d graphic and the horizontal line

plot(0, 0, type="n", xlim=c(-1, 3), ylim=c(-1, 1), xlab="", ylab="", axes=F)
points(0, 0.5, pch=16, cex=2,col="red")
points(1, 0.5, pch=16, cex=2,col="blue")
points(2, 0.5, pch=16, cex=2,col="red")
points(0, 0.25, pch=16, cex=2,col="red")
points(1, 0.25, pch=16, cex=2,col="blue")
points(2, 0.25, pch=16, cex=2,col="red")
points(0, 0, pch=16, cex=2,col="red")
points(1, 0, pch=16, cex=2,col="blue")
points(2, 0, pch=16, cex=2,col="red")
points(0, -0.5, pch=16, cex=2,col="red")
points(1, -0.5, pch=16, cex=2,col="blue")
points(2, -0.5, pch=16, cex=2,col="red")
points(0, -0.25, pch=16, cex=2,col="red")
points(1, -0.25, pch=16, cex=2,col="blue")
points(2, -0.25, pch=16, cex=2,col="red")
abline(h=0, lty=2)
abline(h=0.25, lty=2)
abline(h=0.5, lty=2)
abline(h=-0.25, lty=2)
abline(h=-0.5, lty=2)

text(x = -0.1, y = 0.5, '[', srt = 0, cex = 3)
text(x = 0.1, y = 0.5, ']', srt = 0, cex = 3)

text(x = -0.1, y = 0.25, '[', srt = 0, cex = 3)
text(x = 1.1, y = 0.25, ']', srt = 0, cex = 3)
text(x = -0.1, y = 0, '[', srt = 0, cex = 3)
text(x = 2.1, y = 0, ']', srt = 0, cex = 3)
text(x = 0.9, y = -0.25 ,'[', srt = 0, cex = 3)
text(x = 2.1, y = -0.25 ,']', srt = 0, cex = 3)

text(x = 1.9, y = -0.5, '[', srt = 0, cex = 3)
text(x = 2.1, y = -0.5, ']', srt = 0, cex = 3)

dev.off()


pdf("exercises/hypospaces-capacity/figure/2points_possible_assignment.pdf", width=12, height=8)



plot(0, 0, type="n", xlim=c(-1.5, 4), ylim=c(-0.5, 0.5), xlab="", ylab="", axes=F)
points(0, 0.375, pch=16, cex=2,col="red")
points(1.5, 0.375, pch=16, cex=2,col="red")
points(0, 0.125, pch=16, cex=2,col="red")
points(1.5, 0.125, pch=16, cex=2,col="blue")
points(0, -0.125, pch=16, cex=2,col="blue")
points(1.5, -0.125, pch=16, cex=2,col="red")
points(0, -0.375, pch=16, cex=2,col="blue")
points(1.5, -0.375, pch=16, cex=2,col="blue")
abline(h=0.375, lty=2)
abline(h=0.125, lty=2)
abline(h=-0.125, lty=2)
abline(h=-0.375, lty=2)


text(x = -0.25, y = 0.375, '[', srt = 0, cex = 3)
text(x = 1.75, y = 0.375, ']', srt = 0, cex = 3)

text(x = -1, y = 0.125, '[', srt = 0, cex = 3)
text(x = 1, y = 0.125, ']', srt = 0, cex = 3)

text(x = 0.5, y = -0.125, '[', srt = 0, cex = 3)
text(x = 2.5, y = -0.125, ']', srt = 0, cex = 3)

text(x = 1.6, y = -0.375, '[', srt = 0, cex = 3)
text(x = 3.6, y = -0.375, ']', srt = 0, cex = 3)

dev.off()