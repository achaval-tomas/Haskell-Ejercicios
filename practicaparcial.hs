data EmpresaTelefono = Claro | Personal | Movistar | Tuenti deriving Show
type Frase = String

fraseEmpresa:: EmpresaTelefono -> Frase
fraseEmpresa Claro = "Claro, la red mas poderosa"
fraseEmpresa Personal = "Personal, es como vos"
fraseEmpresa Movistar = "Movistar, Compartida la vida es mas"
fraseEmpresa Tuenti = "Tuenti es la mas economica"

type NombrePersona = String
data MisEmpresas = Ninguna | AgregaEmpresa EmpresaTelefono NombrePersona MisEmpresas

mismaEmpresa :: EmpresaTelefono -> EmpresaTelefono -> Bool
mismaEmpresa Claro Claro = True
mismaEmpresa Personal Personal = True
mismaEmpresa Movistar Movistar = True
mismaEmpresa Tuenti Tuenti = True
mismaEmpresa a b = False

tengoEmpresa :: MisEmpresas -> EmpresaTelefono -> NombrePersona -> Bool
tengoEmpresa Ninguna a b = False
tengoEmpresa (AgregaEmpresa a b c) d e = ((mismaEmpresa a d) && (b == e)) || tengoEmpresa c d e

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving Show
type NroTel = Int
agregaLA :: ListaAsoc EmpresaTelefono NroTel -> EmpresaTelefono -> NroTel -> ListaAsoc EmpresaTelefono NroTel
agregaLA Vacia a b = Nodo a b Vacia
agregaLA a b c = Nodo b c a

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)
aBusca :: Eq a => Arbol a -> a -> Bool
aBusca Hoja x = False
aBusca (Rama a b c) d = (b == d) || aBusca a d || aBusca c d





data Forma = Piedra | Papel | Tijera
legana :: Forma -> Forma -> Bool
legana Piedra Tijera = True
legana Tijera Papel = True
legana Papel Piedra = True
legana a b = False


type Nombre = String
data Jugador = Mano Nombre Forma

ganador :: Jugador -> Jugador -> Maybe Nombre
ganador (Mano a b) (Mano c d) | legana b d = Just a
                              | legana d b = Just c
                              | otherwise = Nothing 

mismaforma :: Forma -> Forma -> Bool
mismaforma Piedra Piedra = True
mismaforma Papel Papel = True
mismaforma Tijera Tijera = True
mismaforma a b = False

quien_jugo :: Forma -> [Jugador] -> [Nombre]
quien_jugo a [] = []
quien_jugo a ((Mano b c):xs) | mismaforma a c = b:(quien_jugo a xs)
                             | otherwise = quien_jugo a xs


type NombrePersona = String
data ServiciosDeuda = Ninguna | AregaDeuda ServicioPublico NombrePersona Int ServiciosDeuda

tengoDeuda :: ServiciosDeuda -> ServicioPublico -> NombrePersona -> Int
tengoDeuda Ninguna a b = 0
tengoDeuda (AgregaDeuda a b c x) d e | (mismoservicio a d) && (b == e) = c
                                     | otherwise = tengoDeuda x d e

