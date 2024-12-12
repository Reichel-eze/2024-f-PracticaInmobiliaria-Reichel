module Library where
import PdePreludat

-- Tenemos los departamentos modelados de la siguiente forma

-- Alias de tipos definidos

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]     -- una lista de requisitos

data Depto = Depto {
    ambientes :: Number,
    superficie :: Number,
    precio :: Number,
    barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
} -- No pongo el deriving porque tengo funciones en las busquedas y eso no le gusta a haskell

-- Funciones que nos da el enunciado

-- Me ordena una lista segun un criterio
ordenarSegun ::  (a -> a -> Bool) -> [a] -> [a]     -- lo hice yo el tipado (a -> a -> Bool) recibe dos parametros y me dice si el 1ero es mayor que el 2do segun un criterio
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
    (ordenarSegun criterio . filter (not . criterio x)) xs ++
    [x] ++
    (ordenarSegun criterio . filter (criterio x)) xs

-- Me dice si el tercer parametro se encuentra entre otros dos parametros 
between :: Ord a => a -> a -> a -> Bool             -- lo hice yo el tipado
between cotaInferior cotaSuperior valor =
    valor <= cotaSuperior && valor >= cotaInferior

-- Algunos deptos de ejemplo
deptosDeEjemplo = [
    Depto 3 80 7500 "Palermo",
    Depto 1 45 3500 "Villa Urquiza",
    Depto 2 50 5000 "Palermo",
    Depto 1 45 5500 "Recoleta"]

-------------------------------------
-- Se pide desarrollar las siguientes funciones y consultas 
-- de modo que se aprovechen tanto como sea posible los 
-- conceptos de orden superior, aplicación parcial y composición.

-- 1.a) Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true 
-- si el resultado de evaluar esa función sobre el primer valor es mayor o menor que el resultado 
-- de evaluarlo sobre el segundo valor respectivamente.

mayor :: Ord b => (a -> b) -> (a -> a -> Bool)  -- b tiene que ordenable porque es lo que estoy comparando
mayor f x y = f x > f y

menor :: Ord b => (a -> b) -> a -> a -> Bool 
menor f x y = f x < f y

-- Por ejemplo
-- mayor (*(-1)) (-3) 2
-- True

-- 1.b) Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings 
-- en base a su longitud usando ordenarSegun.

                                -- (:: a -> a -> Bool) 
ejemploDeOrdenarSegun = ordenarSegun (menor length) ["1", "esteVaASerElTercero", "dos"]
-- Probar en terminal

-- 2.a) Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
-- ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en
-- alguno de los barrios de la lista.

ubicadoEn :: [Barrio] -> Depto -> Bool
ubicadoEn barrios depto = barrio depto `elem` barrios  

-- 1ero. Obtengo el barrio
-- 2dos. Averiguo si esta en la lista de barrios
ubicadoEn' :: [Barrio] -> (Depto -> Bool)
ubicadoEn' barrios = (`elem` barrios) . barrio -- tambien lo podria hacer con un flip!! (el primer argumento del elem es la cosa, luego es la lista)

ubicadoEn'' :: [Barrio] -> Depto -> Bool
ubicadoEn'' barrios = (flip elem barrios). barrio

-- 2.b) cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser 
-- aplicada con el departamento se encuentra entre los dos valores indicados.


