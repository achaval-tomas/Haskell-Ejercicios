main = do
  print "My first Haskell program"
  name <- getLine
  print ("Hello, " ++ name)

f:: Int -> Int -> Int
f x y = x + y

parOimpar:: (String, Int, String) -> String
parOimpar (a, b, c) |b `mod` 2 == 0 = "El numero es par"
                |otherwise = "El numero es impar"

bhaskara:: Float -> Float -> Float -> Float
bhaskara a b c = a*a*a + b*b + c

type Func = (Int, Int, Int)
minPorMax:: Func -> Int
minPorMax (a, b, c) = min (min a b) c * max (max a b) c

celToFahr:: Float -> Float
celToFahr c = c*1.8 + 32 

haceFrio:: Float -> String
haceFrio c | (c-32)/1.8 >= 30 = ("Hace mucho calor, eso son " ++ show((c-32)/1.8) ++ " grados celsius")
           | (c-32)/1.8 < 30 && (c-32)/1.8 > 20 = ("Esta lindo el clima, esos Fahrenheit son equivalentes a " ++ show((c-32)/1.8) ++ " grados celsius")
           | (c-32)/1.8 <= 20 && (c-32)/1.8 > 15 = ("Esta fresquito porque tan solo hace " ++ show((c-32)/1.8) ++ " grados celsius")
           | (c-32)/1.8 <= 15 && (c-32)/1.8 > 0 = ("Esta helado, la temperatura es de " ++ show((c-32)/1.8) ++ " grados celsius")
           | (c-32)/1.8 <= 0 = ("Me estoy congelando literalmente la puta madre hace " ++ show((c-32)/1.8) ++ " grados celsius")

type Numeros = (Float, Float, Float, Float)
duplicaMax:: Numeros -> Float
duplicaMax (a, b, c, d) = max (max (max a b) c) d * 2

type Orden = (Float, Float, Float, Float, Float)
ordenMinMax:: Orden -> (Float, Float)
ordenMinMax (a, b, c, d, e) = ((min (min (min (min a b) c) d) e), (max (max (max (max a b) c) d) e))

horasDeEstudio = do
  print "Cuantas clases tuviste hasta ahora?"
  b <- getLine
  print ("Tus horas estudiadas son aproximadamente cuatro veces " ++ b ++ " y a eso sumale lo que hayas estudiado en casa, osea que un monton")

fact :: Float -> Float
fact 0 = 1 
fact n = n * fact ( n - 1 ) 

cuantoDormiste = do
  print "Cuantos anios tienes?"
  b <- getLine
  print ("Has dormido aproximadamente un tercio de " ++ b ++ " anios")

heInvertido:: Float -> (String, Float)
heInvertido a = ("Vas a haber ganado mas o menos ", (a*30))

soloPares:: [Int] -> [Int]
soloPares [] = []
soloPares (x:xs) | (x `mod` 2) == 0 = x:(soloPares xs)
                 | (x `mod` 2) /= 0 = soloPares xs

mayoresQue10:: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x:xs) | x > 10 = x:(mayoresQue10 xs)
                    | otherwise = mayoresQue10 xs

mayoresQue:: Int -> [Int] -> [Int]
mayoresQue a [] = []
mayoresQue a (x:xs) | x > a = x:(mayoresQue a xs)
                    | otherwise = mayoresQue a xs

duplicaTodos:: [Int] -> [Int]
duplicaTodos [] = []
duplicaTodos (x:xs) = 2*x:(duplicaTodos xs)

imparesCercanos:: [Int] -> [Int]
imparesCercanos [] = []
imparesCercanos (x:xs) | mod x 2 /= 0 = x:(imparesCercanos xs)
                       | mod x 2 == 0 = (x+1):(imparesCercanos xs)

todosMenores10:: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (a:l) | a < 10 = (todosMenores10 l)
                     | otherwise = False

sum1:: [Int] -> Int
sum1 [] = 0
sum1 (x:a) = x + (sum a)

repartir:: [String] -> [String] -> [String]
repartir [] [] = [] 
repartir (x:xs) (y:ys) = (x ++ " " ++ y):(repartir xs ys)

type Alumnos = [(String, String, Int)]
apellidos:: Alumnos -> [String]
apellidos [] = []
apellidos ((a, b, c):xs) = b:(apellidos xs)

tomar :: [a] -> Int -> [a]
tomar [] b = []
tomar (x:xs) b | b > 0 = x:(tomar xs (b-1))
               | b == 0 = tomar [] b

tirar :: [a] -> Int -> [a] 
tirar [] b = [] 
tirar (x:xs) b | b > 0 = tirar xs (b-1)
               | b == 0 = x:xs        
   
indice:: [a] -> Int -> a
indice (x:xs) b | b > 0 = indice xs (b-1)
                | b == 0 = x

indice2:: [a] -> Int -> a
indice2 (x:xs) 0 = x
indice2 (x:xs) b = indice2 xs (b-1)             

sacaLosTrolos:: [String] -> [String]
sacaLosTrolos [] = []
sacaLosTrolos (x:xs) | x == "Tomas Achaval" = x:[]
                     | x /= "Tomas Achaval" = sacaLosTrolos xs

hacerE :: String -> String
hacerE "" = ""
hacerE (x:xs) | x == 'a' || x == 'i' || x == 'o' || x == 'u' = 'e':(hacerE xs)
              | otherwise = x:(hacerE xs)

tirar1 :: [a] -> Int -> [a]
tirar1 [] n = []
tirar1 ys 0 = ys
tirar1 (x:xs) n = tirar1 xs (n-1)

pegAlFin :: [a] -> a -> [a]
pegAlFin xs b = xs ++ b:[]

-- Ejericio 20.a) 
listasIguales:: Eq a => [a] -> [a] -> Bool
listasIguales [] [] = True
listasIguales xs [] = False
listasIguales [] ys = False
listasIguales (x:xs) (y:ys) | x == y = listasIguales xs ys
                            | otherwise = False

-- Ejercicio 20.b)
mejorNota:: [(String, Int, Int, Int)] -> [(String, Int)]
mejorNota [] = []
mejorNota ((a, b, c, d):xs) = (a, max b (max c d)):(mejorNota xs)

-- Ejercicio 20.c)
incPrim:: [(Int, Int)] -> [(Int, Int)]
incPrim [] = []
incPrim ((a,b):xs) = ((a+1), b):(incPrim xs)

-- Ejercicio 20.d)
expandir:: String -> String
expandir [z] = [z]
expandir (x:xs) = [x] ++ " " ++ expandir xs

-- 21 defino un tipo
type Peliculas = [(String, Int, Int, String)]

-- Ejercicio 21.a)
verTodas:: Peliculas -> Int
verTodas [] = 0
verTodas ((a, b, c, d):xs) = c + verTodas xs

-- Ejercicio 21.b)
estrenos :: Peliculas -> [String]
estrenos [] = []
estrenos ((a, b, c, d):xs) | b == 2016 = a:(estrenos xs)
                           | otherwise = estrenos xs

-- Ejercicio 21.c)
filmografia:: Peliculas -> String -> [String]
filmografia [] dir = []
filmografia ((a, b, c, d):xs) dir | d == dir = a:(filmografia xs dir)
                                  | otherwise = filmografia xs dir

-- Ejercicio 21.d)
duracion:: Peliculas -> String -> Int
duracion [] peli = 0
duracion ((a, b, c, d):xs) peli | a == peli = c 
                                | otherwise = duracion xs peli

-- Algunos de parciales viejos
componer:: String -> [String] -> [(String, String)]
componer u [] = []
componer u (x:xs) = (u, x):(componer u xs)

saludar:: [String] -> [(String, String)]
saludar ys = componer "Hola" ys

tamaños:: [[a]] -> [Int]
tamaños [[]] = [0]
tamaños (xs:xss) = (length xs):(tamaños xss)

posCeroNeg:: (Int, Int, Int) -> Bool
posCeroNeg (x, y, z) = (x > 0) && (y == 0) && (z < 0)

-- Boludeces
porcentajeParcial :: (String, Float) ->  (String, Float)
porcentajeParcial (a, b) | b >= 4 = (a, (50+((b*10)-40)*0.8333334))
                         | b < 4 = (a, (b*10)*1.25)

