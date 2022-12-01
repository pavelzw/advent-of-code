module Day ( Day(..) ) where

data Day = DayFile (FilePath -> IO ()) | DayIO (IO ())
