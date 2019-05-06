data Part = AM | PM deriving (Eq, Show)
data TIME = Local Int Int Part | Total Int Int

totalMinuto (Total a b) = a*60 + b  
totalMinuto (Local a b AM) = totalMinuto (Total a b)
totalMinuto (Local a b PM) = totalMinuto (Total (a+12) b) 

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

instance Ord TIME where
    (Total a b) <= (Total ai bi) = (a <= ai) || ((a == ai) && (b <= bi))
    (Total a b) >= (Total ai bi) = (Total ai bi) <= (Total a b)

