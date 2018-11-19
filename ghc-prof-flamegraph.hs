{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Applicative (pure, (<*>), (<|>))
import           Data.Foldable       (traverse_)
import           Data.Functor        ((<$>))
import           Data.List           (intercalate)
import           Data.Monoid         ((<>))
import qualified Options.Applicative as Opts
import qualified ProfFile            as Prof

data Options = Options
  { optionsInput      :: Input
  , optionsReportType :: ReportType
  }
data Input = FileInput FilePath | StdInput

data ReportType = Alloc   -- ^ Report allocations, percent
                | Entries -- ^ Report entries, number
                | Time    -- ^ Report time spent in closure, percent
                | Ticks   -- ^ Report ticks, number
                | Bytes   -- ^ Report bytes allocated, number
                deriving (Eq, Show)

optionsParser :: Opts.Parser Options
optionsParser = Options
  <$> (file <|> pure StdInput)
  <*> (   Opts.flag' Alloc     (Opts.long "alloc" <> Opts.help "Allocations")
      <|> Opts.flag' Entries   (Opts.long "entry" <> Opts.help "Entries")
      <|> Opts.flag' Bytes     (Opts.long "bytes" <> Opts.help "Memory in bytes (+RTS -P -RTS)")
      <|> Opts.flag' Ticks     (Opts.long "ticks" <> Opts.help "Time in ticks (+RTS -P -RTS)")
      <|> Opts.flag  Time Time (Opts.long "time"  <> Opts.help "Use time measurements"))
  where
    file = FileInput <$> Opts.strOption
             (  Opts.long "file"
             <> Opts.short 'f'
             <> Opts.metavar "FILENAME"
             <> Opts.help "Input file")

checkNames :: ReportType -> [String] -> Maybe String
checkNames Alloc   _ = Nothing
checkNames Entries _ = Nothing
checkNames Time    _ = Nothing
checkNames Ticks   n
  | "ticks" `elem` n = Nothing
  | otherwise        = Just "No ticks information, please run program with +RTS -P"
checkNames Bytes n
  | "bytes" `elem` n = Nothing
  | otherwise        = Just "No ticks information, please run program with +RTS -P"

normalize :: ReportType -> Double -> Int
normalize Alloc = round . (10 *)
normalize Time  = round . (10 *)
normalize _     = round

-- FIXME: this makes no sense
addUnknown :: ReportType -> (Int, [String]) -> [String]
addUnknown Time   = \(entries, frames) ->
  let unknown = 1000 - entries
  in if unknown > 0
     then ("UNKNOWN " ++ show unknown) : frames
     else frames
addUnknown Alloc   = \(entries, frames) ->
  let unknown = 1000 - entries
  in if unknown > 0
     then ("UNKNOWN " ++ show unknown) : frames
     else frames
addUnknown _ = snd

generateFrames :: ReportType -> [Prof.Line] -> [String]
generateFrames rt lines0 = addUnknown rt $ go [] lines0
  where
    go :: [String] -> [Prof.Line] -> (Int, [String])
    go _stack [] =
      (0, [])
    go stack (line : lines') =
      let entries = normalize rt (individualMeasure line)
          symbol = Prof.lModule line ++ "." ++ Prof.lCostCentre line
          frame = intercalate ";" (reverse (symbol : stack)) ++ " " ++ show entries
          (childrenEntries, childrenFrames) = go (symbol : stack) (Prof.lChildren line)
          (restEntries, restFrames) = go stack lines'
      in (entries + childrenEntries + restEntries, frame : childrenFrames ++ restFrames)

    individualMeasure = case rt of
      Alloc   -> Prof.lIndividualAlloc
      Time    -> Prof.lIndividualTime
      Entries -> fromIntegral . Prof.lEntries
      Ticks   -> fromIntegral . Prof.lTicks
      Bytes   -> fromIntegral . Prof.lBytes

main :: IO ()
main = do
  Options{..} <- Opts.execParser $
                   Opts.info (Opts.helper <*> optionsParser) Opts.fullDesc
  s <- case optionsInput of
            FileInput file -> readFile file
            StdInput       -> getContents
  case Prof.parse s of
    Left err -> error err
    Right (names, ls) ->
      case checkNames optionsReportType names of
        Just problem -> fail problem
        Nothing      -> do
          traverse_ putStrLn $ generateFrames optionsReportType ls
