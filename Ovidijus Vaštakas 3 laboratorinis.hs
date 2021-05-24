import Data.List --funkcijai transpose
type Vektorius = [Int]
type Matrica = [[Int]]

eilSk :: Matrica -> Int
eilSk = length

stulpSk :: Matrica -> Int
stulpSk = length . head

vektoriuSuma :: Vektorius -> Vektorius -> Vektorius
vektoriuSuma = zipWith (+)

matricuSuma :: Matrica -> Matrica -> Matrica
matricuSuma = zipWith vektoriuSuma

-- Skaičiuojam kiek paduotame vektoryje yra nulių
-- Paduoto vektoriaus pirmas elementas atskiriamas(x)
-- Jei pirmas elementas lygus nuliui, tai pridedam 1, ir likusios vektoriaus dalies(xs) pirmas elementas tampa x
-- Jei pirmas elementas nėra lygus nuliui, tai likusios vektoriaus dalies(xs) pirmas elementas tampa x
-- Baigiame skaičiuoti, kai vektoriuje nebelieka elementų
kiekNul :: Vektorius -> Int
kiekNul [] = 0
kiekNul (x:xs)
    | x == 0 = 1 + kiekNul xs
    | otherwise = kiekNul xs

-- Funkcija kuri visą matricą sudeda į vieną vektorių
-- Jei tiksliau, list of lists, paverčiam į list, pasinaudodami concat funkcija
eilute :: Matrica -> Vektorius
eilute matrica = concat matrica

-- Funkcija, kuri matricos pagrindinę įstrižainę patalpina į vieną vektorių
istrizaine :: Matrica -> Vektorius
istrizaine matrix = zipWith (!!) matrix [0..]

-- Apsirašau tapatumo matricą, jos prireiks skaičiuojant pilnumą
-- Jos pagrindinėje įstrižainėje tik vienetai, o aplink visur nuliai
tapatumo = [[1,0,0,0,0,0],[0,1,0,0,0,0],[0,0,1,0,0,0],[0,0,0,1,0,0],[0,0,0,0,1,0],[0,0,0,0,0,1]]

-- Tikriname ar sąryšis refleksyvusis
-- Pirma patikrinam ar matrica yra kvadratinė, jei taip tęsiam toliau
-- Toliau skaičiuojame, kiek nulių yra matricos pagrindinėje įstrižainėje
-- Jei yra bent vienas nulis, galime iš karto teigti, jog sąryšis nėra refleksyvusis
-- Jei pagrindinėje įstrižainėje nėra nei vieno nulio, sąryšis yra refleksyvusis.
refleksyvumas :: Matrica -> String
refleksyvumas matrix
    | (eilSk(matrix) == stulpSk(matrix)) == False = "Matrica nera kvadratine, savybiu surast negalima"
    | (kiekNul(istrizaine matrix)) > 0 = "Nerefleksyvus"
    | (kiekNul(istrizaine matrix)) == 0 = "Refleksyvus"

-- Tikriname ar sąryšis yra simetriškas
-- Pirmiausia pasinaudodami transpose funkcija, randame įvestos matricos, atvirkštinę matricą
-- Tikrinam, jei atvirkštinė matrica nėra lygi įvestai matricai, sąryšis nesimetriškas
-- Jei atvirkštinė matrica yra lygi įvestai matricai, sąryšis yra simetriškas.
simetriskumas :: Matrica -> String
simetriskumas matrix
    | (transpose(matrix) == matrix) == False = "Nesimetriskas"
    | (transpose(matrix)) == matrix = "Simetriskas"

-- Tikriname ar sąryšis yra tranzityvus
-- Pritaikome apibrėžimą, jei egzistuoja (a,b) ir (b,c), tai turi egzistuot ir (a,c)
-- Naudojame !! indexavimui, tai 0 = a, 1 = b ir t.t
-- Jei egzistuoja (!!0 !!2) == (a, c) ir (!!2 !!3) == (c, d), tai turi egzistuot ir (!!0 !!3) == (a,d)
tranzityvumas :: Matrica -> String
tranzityvumas matrix
    | matrix !!0 !!2 == 1 && matrix !!2 !!3 == 1 && matrix !!0 !!3 == 0 = "Netranzityvus"
    | matrix !!0 !!2 == 1 && matrix !!1 !!3 == 1 && matrix !!0 !!3 == 1 = "Tranzityvus"

-- Tikriname ar pilnasis
-- Ivedus matricą, pirmiausia sudedame turimą matricą, jos atvirkštinę matricą ir tapatumo
-- Po sudeties, gautą matricą (list of lists), paverčiam į vektorių (list)
-- Pavertus matricą į vieną vektorių, galime suskaičiuoti kiek gautame vektoriuje yra 0 su funkcija kiekNul
-- Pagal žinomą apibrėžimą, jei matrica yra pilnoji, jos visų elementų reikšmės turėtų būt 1
-- Tą ir tikriname, jei 0 yra bent vienas, sąryšis nėra pilnasis, jei neivieno yra pilnasis
pilnumas :: Matrica -> String
pilnumas matrix
    | kiekNul(eilute (matricuSuma (matricuSuma (transpose(matrix)) matrix) tapatumo)) >0 = "Nepilnasis"
    | kiekNul(eilute (matricuSuma (matricuSuma (transpose(matrix)) matrix) tapatumo)) ==0 = "Pilnasis"

-- Funkcija, su kuria iškviečiamos visos funkcijos savybėms tirti
savybes :: Matrica -> [Char]
savybes matrix = refleksyvumas matrix ++ ", " ++ simetriskumas matrix ++ ", " ++ tranzityvumas matrix ++ ", " ++ pilnumas matrix