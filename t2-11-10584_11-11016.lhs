{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

>import Control.Applicative 	(pure)
>import Control.DeepSeq		(NFData, ($!!))
>import Control.Monad		(void)
>import Data.Map				(Map, empty, singleton)
>import GHC.Generics			(Generic)
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

>t1, t2, t3 :: Expresión
>t1  = Literal 42
>t2  = Suma (Literal 27) t1
>t3  = Suma (Multiplicación t2 (Multiplicación t2 (Literal 1))) (Negativo (División (Suma t1 (Literal 0)) (Literal 3)))