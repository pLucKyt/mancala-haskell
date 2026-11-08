-- Crtanje Rose stabla odozgo nadole, u stilu crteza iz tree_example.png.
-- Ovo NIJE deo zadatka 1 — cisto pomocna stvar za prikaz na ekranu.
-- Zato stoji u zasebnom fajlu, da Rose.hs ostane tacno ono sto se ocenjuje.
--
-- Ideja u tri recenice:
--   1. Svako podstablo se nacrta u svoj pravougaoni "blok" linija, i uz blok
--      se pamti u kojoj koloni stoji njegov koren.
--   2. Blokovi dece se slepe jedan pored drugog, pa se roditelj centrira
--      iznad raspona od prvog do poslednjeg deteta.
--   3. Izmedju roditelja i dece ide jedan red veza: iznad deteta koje je levo
--      od roditelja stoji '/', iznad onog koje je desno stoji '\', a iznad onog
--      koje je tacno ispod (ili do) roditelja stoji '|'.
--
-- Kao i na papiru, kosa crta samo NAGOVESTAVA liniju do roditelja, ne crta je
-- celu. Zato je dovoljan jedan red i crtez ostaje nizak.
module Crtanje where

import Rose

-- ---------- sitni pomocni alati za blokove teksta ----------

sirinaBloka :: [String] -> Int
sirinaBloka ls = maximum (0 : [length l | l <- ls])

dopuniDesno :: Int -> String -> String
dopuniDesno n s = s ++ replicate (n - length s) ' '

-- svede blok na tacno h redova i tacno w kolona
poravnaj :: Int -> Int -> [String] -> [String]
poravnaj h w ls = [dopuniDesno w l | l <- ls] ++ replicate (h - length ls) (replicate w ' ')

-- zalepi blokove jedan pored drugog, sa `razmak` praznih kolona izmedju
spojiUporedo :: Int -> [[String]] -> [String]
spojiUporedo razmak = foldl1 dva
  where
    dva a b = [la ++ replicate razmak ' ' ++ lb | (la, lb) <- zip a' b']
      where
        h  = max (length a) (length b)
        a' = poravnaj h (sirinaBloka a) a
        b' = poravnaj h (sirinaBloka b) b

-- ---------- samo crtanje ----------

-- Vraca (linije bloka, kolona u kojoj stoji koren tog bloka).
crtajBlok :: Show a => Rose a -> ([String], Int)
crtajBlok (Node val []) = ([show val], length (show val) `div` 2)
crtajBlok (Node val deca) = (redOznake : redVeza : telo, korenKol)
  where
    razmak  = 3
    oznaka  = show val
    blokovi = [crtajBlok d | d <- deca]

    -- gde svaki decji blok pocinje, pa odatle apsolutne kolone njihovih korena
    pocetci     = scanl (\p (ls, _) -> p + sirinaBloka ls + razmak) 0 blokovi
    decaKolone0 = [p + k | (p, (_, k)) <- zip pocetci blokovi]
    teloSirovo  = spojiUporedo razmak [ls | (ls, _) <- blokovi]

    -- roditelj ide na sredinu izmedju prvog i poslednjeg deteta
    sredina0       = (head decaKolone0 + last decaKolone0) `div` 2
    pocetakOznake0 = sredina0 - (length oznaka `div` 2)

    -- ako bi siroka oznaka izasla van leve ivice, ceo crtez se pomeri udesno
    pomak      = max 0 (negate pocetakOznake0)
    telo       = [replicate pomak ' ' ++ l | l <- teloSirovo]
    decaKolone = [k + pomak | k <- decaKolone0]
    korenKol   = sredina0 + pomak

    redOznake = replicate (pocetakOznake0 + pomak) ' ' ++ oznaka
    sirina    = maximum (length redOznake : [length l | l <- telo])
    redVeza   = [vezniZnak i | i <- [0 .. sirina - 1]]

    -- dete levo od roditelja dobija '/' odmah desno od sebe,
    -- dete desno dobija '\' odmah levo od sebe,
    -- a dete koje je tacno ispod roditelja (ili do njega) dobija '|'
    koseLevo  = [c + 1 | c <- decaKolone, c < korenKol - 1]
    koseDesno = [c - 1 | c <- decaKolone, c > korenKol + 1]
    uspravne  = [c     | c <- decaKolone, abs (c - korenKol) <= 1]

    vezniZnak i
      | i `elem` koseLevo  = '/'
      | i `elem` koseDesno = '\\'
      | i `elem` uspravne  = '|'
      | otherwise          = ' '

-- ---------- ono sto se zove spolja ----------

prikaziStablo :: Show a => Rose a -> String
prikaziStablo t = unlines [bezRepa l | l <- fst (crtajBlok t)]
  where
    bezRepa = reverse . dropWhile (== ' ') . reverse

-- najkrace za ghci: crtaj primer1
crtaj :: Show a => Rose a -> IO ()
crtaj = putStr . prikaziStablo
