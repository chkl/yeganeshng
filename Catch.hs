{-# LANGUAGE CPP #-}

module Catch (catch) where

#if MIN_VERSION_base(4,0,0)
import qualified Control.Exception
import Prelude

catch :: IO a -> (Control.Exception.IOException -> IO a) -> IO a
catch = Control.Exception.catch
#endif
