myCube = [y^3 | y <- [1..5]]
mySqr = [x^2 | x <- [1..5]]

tup1 = [(x,y) | x <- mySqr, y <- myCube]
tup2 = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
n = length tup2
