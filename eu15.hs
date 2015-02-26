import Factorial

num = 20

main = print $ (round (fac (num * 2) / (fac num * fac num)) :: Integer)
