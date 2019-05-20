-- EXER: https://drive.google.com/file/d/1vAKU4T-wuvmvBClZ1WkcPsmWkkxTscqN/view
-- TAREFA 2

data Part = AM | PM deriving (Eq, Show)
data TIME = Local Int Int Part | Total Int Int

-- Questão 2
totalMinuto (Total a b) = a*60 + b 
totalMinuto (Local a b AM) = totalMinuto (Total a b)
totalMinuto (Local a b PM) = totalMinuto (Total (a+12) b) 

-- Questão 3
instance Eq TIME where
    (Local a b c) == (Local ai bi ci) = (a == ai) && (b == bi) && (c == ci) 
    (Total a b) == (Total ai bi) = (a == ai) && (b == bi)
    (Total a b) == (Local ai bi ci) 
        | ci == AM && ai == 12 = (Total 0 bi) == (Total a b) 
        | ci == AM = (Total ai bi) == (Total a b) 
        | otherwise = (Total (ai+12) bi) == (Total a b) 
    (Local ai bi ci) == (Total a b) 
        | ci == AM && ai == 12 = (Total 0 bi) == (Total a b) 
        | ci == AM = (Total ai bi) == (Total a b) 
        | otherwise = (Total (ai+12) bi) == (Total a b) 

-- Questão 4
instance Ord TIME where
    (Total a b) < (Total ai bi) = (a < ai) || ((a == ai) && (b < bi))
    (Local a b c) < (Local ai bi ci)
        | c == ci = (Total a b) < (Total ai bi)
        | otherwise = if (c == AM) then True else False
    (Local a b c) < (Total ai bi) 
        | c == AM && a == 12 = (Total 0 b) < (Total ai bi) 
        | c == AM = (Total a b) < (Total ai bi) 
        | otherwise = (Total (a+12) b) < (Total ai bi) 
    (Total a b) < (Local ai bi ci) = (Local ai bi ci) > (Total a b)

    (Total a b) > (Total ai bi) = (Total ai bi) < (Total a b)
    (Local a b c) > (Local ai bi ci) = (Local ai bi ci) < (Local a b c)
    (Local a b c) > (Total ai bi) 
        | c == AM && a == 12 = (Total 0 b) > (Total ai bi) 
        | c == AM = (Total a b) > (Total ai bi) 
        | otherwise = (Total (a+12) b) > (Total ai bi) 
    (Total a b) > (Local ai bi ci) = (Local ai bi ci) < (Total a b)

    (Total a b) <= (Total ai bi) = (Total a b) < (Total ai bi) ||
                                   (Total a b) == (Total ai bi)
    (Local a b c) <= (Local ai bi ci) = (Local a b c) < (Local ai bi ci) ||
                                        (Local a b c) == (Local ai bi ci)
    (Local a b c) <= (Total ai bi) = (Local a b c) < (Total ai bi) ||
                                     (Local a b c) == (Total ai bi)
    (Total a b) <= (Local ai bi ci) = (Local ai bi ci) >= (Total a b)

    (Total a b) >= (Total ai bi) = (Total a b) > (Total ai bi) ||
                                   (Total a b) == (Total ai bi)
    (Local a b c) >= (Local ai bi ci) = (Local a b c) > (Local ai bi ci) ||
                                   (Local a b c) == (Local ai bi ci)
    (Local a b c) >= (Total ai bi) = (Local a b c) > (Total ai bi) ||
                                     (Local a b c) == (Total ai bi)
    (Total a b) >= (Local ai bi ci) = (Local ai bi ci) <= (Total a b)

-- Questão 5
instance Show TIME where
    show (Local a b AM) = (show a) ++ [':'] ++ (show b) ++ " am"
    show (Local a b PM) = (show a) ++ [':'] ++ (show b) ++ [' ', 'p', 'm']
    show (Total a b) = (show a) ++ ['h'] ++ (show b) ++ ['m']
    
-- Questão 6
seleciona :: TIME -> [(TIME,String)] -> [(TIME,String)]    
seleciona time timesFilms = filter (\x -> (fst x) > time) timesFilms


class FigFechada a where
    area :: a -> Float
    perimetro :: a -> Float

fun figs = filter (\fig -> (area fig) > 100) figs

type Ponto = (Float,Float)
type Lado = Float
data Rectangulo = PP Ponto Ponto | PLL Ponto Lado Lado

instance FigFechada Rectangulo where
    area (PP (a, b) (c, d)) = (abs (a-c)) * (abs (b-d)) 
    area (PLL (a, b) c d) = c * d
    perimetro (PP (a, b) (c, d)) = (abs (a-c))*2 + (abs (b-d))*2
    perimetro (PLL (a, b) c d) = c*2 + d*2

somaAreas :: FigFechada a => [a] -> Float
somaAreas listRec = sum (map (\x -> area x) listRec)

instance Enum TIME where
    fromEnum a = 20
    toEnum (Total a b) = [(Total [0..2] [0..2])]
    succ (Total a b)
        | b == 59 = (Total (a+1) 0)
        | otherwise = (Total a (b+1))
    