import Text.Show.Functions
import Data.Char
import Data.List


someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- 1) Modelar a los héroes. Tip: lean todo el enunciado!


data Heroe = Heroe {
    reconocimiento :: Int,
    epiteto :: String,
    artefactos :: [Artefacto],
    listaTareas :: [Tarea]

}deriving(Show)



data Artefacto = Artefacto {
    rareza :: Int

}deriving(Show,Eq)

modificarEpitetoHeroe :: (String->String)->Heroe->Heroe
modificarEpitetoHeroe funcion unHeroe = unHeroe {epiteto = funcion.epiteto $ unHeroe }

modificarReconocimientoHeroe :: (Int->Int)->Heroe->Heroe
modificarReconocimientoHeroe funcion unHeroe = unHeroe {reconocimiento = funcion.reconocimiento $ unHeroe }

modificarArtefactosHeroe :: ([Artefacto]->[Artefacto])->Heroe->Heroe
modificarArtefactosHeroe funcion unHeroe = unHeroe { artefactos = funcion.artefactos $ unHeroe }

modificarTareasHeroe :: ([Tarea]->[Tarea])->Heroe->Heroe
modificarTareasHeroe funcion unHeroe = unHeroe {listaTareas = funcion.listaTareas $ unHeroe }

-- 2) Hacer que un héroe pase a la historia. Esto varía según el índice de reconocimiento que tenga el héroe a la hora de su muerte:
{--
Si su reconocimiento es mayor a 1000, su epíteto pasa a ser "El mítico", y no obtiene ningún artefacto. 
¿Qué artefacto podría desear tal espécimen?
Si tiene un reconocimiento de al menos 500, su epíteto pasa a ser "El magnífico" y añade a sus artefactos la lanza del Olimpo 
(100 de rareza). 
Si tiene menos de 500, pero más de 100, su epíteto pasa a ser "Hoplita" y añade a sus artefactos una Xiphos (50 de rareza).
En cualquier otro caso, no pasa a la historia, es decir, no gana ningún epíteto o artefacto.

--}
cantReconocimientoMitico :: Int
cantReconocimientoMitico = 1000

cantReconocimientoMagnifico :: Int 
cantReconocimientoMagnifico = 500

cantMaxHoplita :: Int
cantMaxHoplita = 500

cantMinHoplita :: Int
cantMinHoplita = 100

condicionReconocimiento :: (Int->Bool)->Heroe->Bool
condicionReconocimiento unaCondicion unHeroe = unaCondicion.reconocimiento $ unHeroe

cambiarEpiteto :: String->Heroe->Heroe
cambiarEpiteto nuevoEpiteto unHeroe= modificarEpitetoHeroe (const nuevoEpiteto) unHeroe

lanzaDelOlimpo = Artefacto 100
xiphos = Artefacto 50 
elRelampagoDeZeus = Artefacto 500

añadirArtefacto :: Artefacto->Heroe->Heroe
añadirArtefacto unArtefacto unHeroe = unHeroe {artefactos= unArtefacto : (artefactos unHeroe) }
 
pasarALaHistoria :: Heroe->Heroe
pasarALaHistoria unHeroe 
                     | condicionReconocimiento (>cantReconocimientoMitico) unHeroe = cambiarEpiteto "El mitico" unHeroe
                     | condicionReconocimiento (>=cantReconocimientoMagnifico) unHeroe = (añadirArtefacto lanzaDelOlimpo).(cambiarEpiteto "El magnifico") $ unHeroe
                     | (&& (condicionReconocimiento (<cantMaxHoplita)  unHeroe)).(condicionReconocimiento (>cantMinHoplita)) $ unHeroe =  (añadirArtefacto xiphos).(cambiarEpiteto  "Hoplita") $ unHeroe
                     | otherwise = unHeroe               

{--
Día a día, los héroes realizan tareas. Llamamos tareas a algo que modifica a un héroe de alguna manera, 
algo tan variado como aumentar su reconocimiento, obtener un nuevo epíteto o artefacto, y muchas más. 
Tras realizar una tarea, los héroes se la anotan en una lista, para luego recordarlas y presumirlas ante sus compañeros. 
Hay infinidad de tareas que un héroe puede realizar, por el momento conocemos las siguientes:
Encontrar un artefacto: el héroe gana tanto reconocimiento como rareza del artefacto, además de guardarlo entre los que lleva.
Escalar el Olimpo: esta ardua tarea recompensa a quien la realice otorgándole 500 unidades de reconocimiento 
y triplica la rareza de todos sus artefactos, pero desecha todos aquellos que luego de triplicar su rareza 
no tengan un mínimo de 1000 unidades. Además, obtiene "El relámpago de Zeus" (un artefacto de 500 unidades de rareza).

Ayudar a cruzar la calle: incluso en la antigua Grecia los adultos mayores necesitan ayuda para ello.
 Los héroes que realicen esta tarea obtiene el epíteto "Groso", 
 donde la última 'o' se repite tantas veces como cuadras haya ayudado a cruzar. 
 Por ejemplo, ayudar a cruzar una cuadra es simplemente "Groso", pero ayudar a cruzar 5 cuadras es "Grosooooo".
Matar una bestia: Cada bestia tiene una debilidad (por ejemplo: que el héroe tenga cierto artefacto,
 o que su reconocimiento sea al menos de tanto). Si el héroe puede aprovechar esta debilidad, entonces obtiene el epíteto de 
 "El asesino de <la bestia>". Caso contrario, huye despavorido, perdiendo su primer artefacto. 
 Además, tal cobardía es recompensada con el epíteto  "El cobarde".

Modelar las tareas descritas, contemplando que en el futuro podría haber más.

--}

type Tarea = Heroe->Heroe
modificarRarezaArtefacto :: (Int->Int)->Artefacto -> Artefacto
modificarRarezaArtefacto funcion unArtefacto = unArtefacto{rareza = funcion.rareza $ unArtefacto }

encontrarUnArtefacto :: Artefacto->Tarea
encontrarUnArtefacto unArtefacto unHeroe = (añadirArtefacto unArtefacto).(modificarReconocimientoHeroe (+(rareza unArtefacto))) $ unHeroe

escalarElOlimpo :: Tarea 
escalarElOlimpo unHeroe = añadirArtefacto elRelampagoDeZeus.modificarArtefactosHeroe (filter ((>=1000).rareza)).modificarArtefactosHeroe (map (modificarRarezaArtefacto (*3))).(modificarReconocimientoHeroe (+500)) $ unHeroe

letrasO :: Int -> String
letrasO cantCuadras = take cantCuadras (concat (iterate (++"") "o" ))

ayudarACruzarLaCalle :: Int->Tarea
ayudarACruzarLaCalle cantCuadras unHeroe = cambiarEpiteto ((++) "Gros" (letrasO cantCuadras))  unHeroe

quitarPrimerArtefacto :: [Artefacto]->[Artefacto]
quitarPrimerArtefacto listaArtefactos = tail listaArtefactos

matarUnaBestia :: String ->Debilidad->Tarea
matarUnaBestia nombreBestia debilidadBestia unHeroe 
                                                    | debilidadBestia unHeroe = cambiarEpiteto ((++)"El asesino de" nombreBestia ) unHeroe 
                                                    | otherwise = (cambiarEpiteto "El cobarde").(modificarArtefactosHeroe quitarPrimerArtefacto) $ unHeroe



{--
Modelar a Heracles, cuyo epíteto es "Guardián del Olimpo" y tiene un reconocimiento de 700. 
Lleva una pistola de 1000 unidades de rareza (es un fierro en la antigua Grecia, obviamente que es raro) 
y el relámpago de Zeus. Este Heracles es el Heracles antes de realizar sus doce tareas, hasta ahora sabemos que solo hizo una tarea...

--}
pistola = Artefacto 1000

heracles= Heroe 700 "Guardián del Olimpo" [pistola,elRelampagoDeZeus] [matarAlLeonDeNemea]

{--
    Modelar la tarea "matar al león de Nemea", que es una bestia cuya debilidad es que el epíteto del héroe sea de 20 caracteres o más. 
    Esta es la tarea que realizó Heracles.
--}
type Debilidad = Heroe->Bool

debilidadLeonDeNemea :: Debilidad
debilidadLeonDeNemea = (>=20).length.epiteto

matarAlLeonDeNemea :: Tarea 
matarAlLeonDeNemea unHeroe = matarUnaBestia "león de Nemea" debilidadLeonDeNemea unHeroe 

-- Hacer que un héroe haga una tarea. Esto nos devuelve un nuevo héroe con todos los cambios que conlleva realizar una tarea.



hacerUnaTarea :: Heroe -> Heroe
hacerUnaTarea unHeroe = (modificarTareasHeroe tail).(head (listaTareas  unHeroe)) $ unHeroe


{--
Hacer que dos héroes presuman sus logros ante el otro. Como resultado, queremos conocer la tupla que tenga en primer lugar al ganador de la contienda, y en segundo al perdedor. Cuando dos héroes presumen, comparan de la siguiente manera:
Si un héroe tiene más reconocimiento que el otro, entonces es el ganador.
Si tienen el mismo reconocimiento, pero la sumatoria de las rarezas de los artefactos de un héroe es mayor al otro, entonces es el ganador.
Caso contrario, ambos realizan todas las tareas del otro, y vuelven a hacer la comparación desde el principio. Llegado a este punto, el intercambio se hace tantas veces sea necesario hasta que haya un ganador.


--}
presumir :: Heroe->Heroe->(Heroe,Heroe)
presumir heroe1 heroe2 
                        | reconocimiento heroe1 > reconocimiento heroe2 = (heroe1,heroe2)
                        | reconocimiento heroe2 > reconocimiento heroe1 = (heroe2,heroe1) 
                        | reconocimiento heroe1 == reconocimiento heroe2 = desempatePorArtefactos heroe1 heroe2 

sumatoriaRarezas :: Heroe -> Int
sumatoriaRarezas unHeroe = sum.map rareza.artefactos $ unHeroe

--Hacer que un héroe realice una labor, obteniendo como resultado el héroe tras haber realizado todas las tareas.


labor :: [Tarea]->Heroe->Heroe
labor listaTareas unHeroe =  foldr ($) unHeroe listaTareas 



desempatePorArtefactos heroe1 heroe2 
                        |   (sumatoriaRarezas  heroe1) < (sumatoriaRarezas heroe2) = (heroe2,heroe1)
                        |   (sumatoriaRarezas  heroe1) > (sumatoriaRarezas heroe2) = (heroe1,heroe2)
                        |   (sumatoriaRarezas  heroe1) == (sumatoriaRarezas heroe2) = presumir (labor (listaTareas heroe2) heroe1) (labor (listaTareas heroe1) heroe2)


{--
¿Cuál es el resultado de hacer que presuman dos héroes con reconocimiento 100, ningún artefacto y ninguna tarea realizada?
--}

-- Nunca se terminaría de ejecutar por el simple hecho de que ambos heroes tienen las mismas características 
-- (reconocimiento, rarezas y tareas), lo que hace que simpre empaten y la ejecución continue.


--Si invocamos la función anterior (labor) con una labor infinita, ¿se podrá conocer el estado final del héroe? ¿Por qué?

-- No se podría conocer el estado final del Heroe ya que nunca se modificaría lo suficiente para conocer su estado final
-- Son funciones que se aplican infinitamente sobre el héroe y nunca terminan de afectar al mismo


