#Codigo para problema 2

mis_dades <- iris
mis_dades

#Buscar regresion entre x e y

x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length

plot(x,y)

x_bar <- mean(x)
y_bar <- mean(y)

#pendiente de la recta de regresion y=mx +b

m <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)

b <- y_bar -m*x_bar

#prediccion petallength=1.5

m*1.5 +b

#dibujar linea
x_pred <- x
y_pred<- m*x_pred +b

plot(x,y)
lines(x_pred, y_pred)

# R^2 coef de determinacion
Rsq <- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)

# R coef de correlacion
cor <- sqrt(Rsq)




# EN R studio

#linear regresion (intercept=b, x=m, R-squared=R^2)
mod <- lm(y~x)
mod

summary(mod)
# R
cor.test(x,y)

# ver todos los valores de y para todas las x en prediccion
y_pred2 <- predict(mod, data.frame(x=1.5))
y_pred2
