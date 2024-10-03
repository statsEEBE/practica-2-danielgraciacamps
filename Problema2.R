#Codigo para problema 2
#Codigo para problema 2 
Unstage line	
 
 
2
 
3
mis_dades <- iris
 
4
mis_dades
 
5
 
6
#Buscar regresion entre x e y
 
7
 
8
x <- mis_dades$Petal.Length
 
9
y <- mis_dades$Sepal.Length
 
10
 
11
plot(x,y)
 
12
 
13
x_bar <- mean(x)
 
14
y_bar <- mean(y)
 
15
 
16
#pendiente de la recta de regresion y=mx +b
 
17
 
18
m <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
 
19
 
20
b <- y_bar -m*x_bar
 
21
 
22
#prediccion petallength=1.5
 
23
 
24
m*1.5 +b
 
25
 
26
#dibujar linea
 
27
x_pred <- x
 
28
y_pred<- m*x_pred +b
 
29
 
30
plot(x,y)
 
31
lines(x_pred, y_pred)
 
32
 
33
# R^2 coef de determinacion
 
34
Rsq <- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)
 
35
 
36
# R coef de correlacion
 
37
cor <- sqrt(Rsq)
 
38
 
39
 
40
 
41
 
42
# EN R studio
 
43
 
44
#linear regresion (intercept=b, x=m, R-squared=R^2)
 
45
mod <- lm(y~x)
 
46
mod
 
47
 
48
summary(mod)
 
49
# R
 
50
cor.test(x,y)
 
51
 
52
# ver todos los valores de y para todas las x en prediccion
 
53
y_pred2 <- predict(mod, data.frame(x=1.5))
 
54
y_pred2
