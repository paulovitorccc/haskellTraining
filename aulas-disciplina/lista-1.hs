-- Questão 1
-- - Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
-- - impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
-- - Procure usar casamento de padroes e reutilizar as funcoes.
-- xor a b = undefined
xor a b = (not (a && b)) && (a || b)

-- Questão 2
-- impl a b = undefined
impl a b = (not a) || b

-- Questão 3
-- equiv a b = undefined
equiv a b = impl a b && impl b a

-- Questão 4
-- - Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-- pow x y = undefined
pow a b 
    | b == 0    = 1.0
    | b < 0     = 1/product (replicate (abs b) a)
    | otherwise = product (replicate b a)

-- Questão 5
-- - Implemente a funcao fatorial que calcula o fatorial de um numero 
-- fatorial x = undefined
fatorial a = product [1..a]

-- Questão 6
-- - Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
-- - Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-- isPrime x = undefined
primeAux a 1 = True
primeAux a b = if((mod a b) == 0) then False else primeAux a (b-1)
isPrime 1 = True 
isPrime a = primeAux a (a-1)

-- Questão 7
-- - Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-- fib x = undefined
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib (x-2)

-- Questão 8
-- - Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-- mdc x y = undefined
mdc x y = if(x >= y) then mdcAux x y else mdcAux y x
mdcAux x 0 = x
mdcAux x y = mdcAux y (mod x y)

-- Questão 9
-- - Calcula um MMC de dois numeros. 
-- mmc x y = undefined
-- mmc x y = (mdc x y)

-- - Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
-- - o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-- coprimo x y = if (mmc x y == 1) then True else False

-- - Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito 
--   como a soma de dois numeros primos. Ex: 28 = 5 + 23.
goldbach x = (coPrimes !! 0, x-(coPrimes !! 0))
    where primesToX = [p | p <- [1..x], isPrime p] 
          coPrimes = [y | y <- primesToX, isPrime (x-y)]