{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import Data.Text as T
default (T.Text)

main :: IO ()
main = shelly $ verbosely $ run_ "ghc" [ "-O2"
                                       , "Main.hs"
                                       , "-odir"
                                       , "./build-artifacts/objects"
                                       , "-hidir"
                                       , "./build-artifacts/interfaces"
                                       ]

