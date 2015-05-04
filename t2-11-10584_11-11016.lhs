{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

>import Control.Applicative 	(pure)
>import Control.DeepSeq		(NFData, ($!!))
>import Control.Monad		(void)
>import Data.Map			(Map, empty, foldWithKey, singleton)
>import GHC.Generics		(Generic)
>import System.Environment 	(getArgs, getProgName)
>import System.IO 			(hPutStrLn, stderr)

>data Expresión
>	= Suma 			 Expresión Expresión
>	| Resta 		 Expresión Expresión
>	| Multiplicación Expresión Expresión
>	| División 		 Expresión Expresión
>	| Negativo 		 Expresión
>	| Literal 		 Integer
>	deriving (Eq, Read, Show)

>evaluar :: Expresión -> Double
>evaluar (Suma e1 e2) 			= evaluar e1 + evaluar e2
>evaluar (Resta e1 e2) 			= evaluar e1 - evaluar e2
>evaluar (Multiplicación e1 e2) = evaluar e1 * evaluar e2
>evaluar (División e1 e2) 		= evaluar e1 / evaluar e2
>evaluar (Negativo e) 			= - evaluar e
>evaluar (Literal n) 			= fromIntegral n

>operaciones :: Expresión -> Integer
>operaciones (Literal n)			= 0
>operaciones (Negativo e) 			= 1 + operaciones e
>operaciones (Suma e1 e2) 			= 1 + operaciones e1 + operaciones e2
>operaciones (Resta e1 e2) 			= 1 + operaciones e1 + operaciones e2
>operaciones (Multiplicación e1 e2) = 1 + operaciones e1 + operaciones e2
>operaciones (División e1 e2) 		= 1 + operaciones e1 + operaciones e2

>sumaLiterales :: Expresión -> Integer
>sumaLiterales (Literal n)				= n
>sumaLiterales (Negativo e) 			= sumaLiterales e
>sumaLiterales (Suma e1 e2) 			= sumaLiterales e1 + sumaLiterales e2
>sumaLiterales (Resta e1 e2) 			= sumaLiterales e1 + sumaLiterales e2
>sumaLiterales (Multiplicación e1 e2) 	= sumaLiterales e1 + sumaLiterales e2
>sumaLiterales (División e1 e2) 		= sumaLiterales e1 + sumaLiterales e2

>literales :: Expresión -> [Integer]
>literales (Literal n)				= [n]
>literales (Negativo e) 			= literales e
>literales (Suma e1 e2) 			= literales e1 ++ literales e2
>literales (Resta e1 e2) 			= literales e1 ++ literales e2
>literales (Multiplicación e1 e2) 	= literales e1 ++ literales e2
>literales (División e1 e2) 		= literales e1 ++ literales e2

>altura :: Expresión -> Integer
>altura (Literal n)				= 0
>altura (Negativo e) 			= 1 + altura e
>altura (Suma e1 e2) 			= 1 + max (altura e1)  (altura e2)
>altura (Resta e1 e2) 			= 1 + max (altura e1)  (altura e2)
>altura (Multiplicación e1 e2) 	= 1 + max (altura e1)  (altura e2)
>altura (División e1 e2) 		= 1 + max (altura e1)  (altura e2)


>cataExpresión :: (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (Integer -> a) -> Expresión -> a
>cataExpresión suma resta multiplicación división negativo literal = undefined

>type Atributos = Map String String
>newtype Documento
>	= Documento Elemento
>	deriving Show

>data Elemento
>	= Elemento String Atributos [Elemento]
>	| Texto String
>	deriving Show

>htmlE, headE, bodyE, divE :: [Elemento] -> Elemento
>htmlE	[] 	= Elemento "html" (singleton "xmlns" "http://www.w3.org/1999/xhtml") []
>htmlE	[e] = Elemento "html" (singleton "xmlns" "http://www.w3.org/1999/xhtml") [e]https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-ghc.html#idp8605200
>headE	[]	= Elemento "head" (empty) []
>headE	[e]	= Elemento "head" (empty) [e]
>bodyE	[]	= Elemento "body" (empty) []
>bodyE	[e]	= Elemento "body" (empty) [e]
>divE	[]	= Elemento "div" (empty) []
>divE	[e]	= Elemento "div" (empty) [e]

>styleE, titleE, h1E, pE :: String -> Elemento
>styleE	s = Elemento "style" (singleton "type" "text/css") [Texto s]
>titleE	s = Elemento "tittle" empty [Texto s]
>h1E	s = Elemento "h1" empty [Texto s]
>pE 	s = Elemento "p" empty [Texto s]

>showP :: Show a => a -> Elemento
>showP	s = Texto (show s)

>t1, t2, t3 :: Expresión
>t1  = Literal 42
>t2  = Suma (Literal 27) t1
>t3  = Suma (Multiplicación t2 (Multiplicación t2 (Literal 1))) (Negativo (División (Suma t1 (Literal 0)) (Literal 3)))
