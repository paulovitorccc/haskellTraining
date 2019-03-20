-- Questão 1
xor False False = False
xor True True = False
xor _ _ = True

-- Questão 2
impl a b = (not a) || b

-- Questão 3
equiv a b = impl a b && impl b a

-- Questão 4
pow a b 
    | b == 0    = 1.0
    | b < 0     = 1/product (replicate (abs b) a)
    | otherwise = product (replicate b a)

-- Questão 5
fatorial a = product [1..a]

-- Questão 6
primeAux a 1 = True
primeAux a b = if((mod a b) == 0) then False else primeAux a (b-1)
isPrime 1 = True 
isPrime a = primeAux a (a-1)

-- Questão 7
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib (x-2)

-- Questão 8
mdc x y = if(x >= y) then mdcAux x y else mdcAux y x
mdcAux x 0 = x
mdcAux x y = mdcAux y (mod x y)

-- Questão 9
mmc x y = (x*y) / (mdc x y)