
# Code to calculate moisture optimum for each species following Condit et al. (2013).
# Example below is for 'Randia armata'. See Figure S2 from Condit et al. (2013).
b0 = -1.543082203
b1 = 2.33734300392549
b2 = -0.969260502938853
x = seq(-3,3,.01)
y = b0 + b1*x + b2*x^2
p = exp(y)/(1+exp(y))
plot(x,p)
z = data.frame(cbind(x,p))
moisture.optimum = z$x[z$p==max(z$p)]


b0 = 1.33 # TABG
b1 = -0.82 # TABG
b2 = -0.77 #TABG

b0 = -.12 # HYBP
b1 = .33 # HYBP
b2 = -0.91 #HYBP
