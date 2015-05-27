{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Language.Atom
import Control.Monad
import Data.Int
import System.Directory

import Enumerate (hourMin5Colorings)

led5Pins = [2..4]
led3Pins = [5..7]
led2Pins = [8..10]
led1aPins = [15..17]
led1bPins = [21..23]

pinLists = [ led5Pins, led3Pins, led2Pins, led1aPins, led1bPins ]

allPins :: (Num a, Enum a) => [a]
allPins = concat pinLists

numPins = fromIntegral $ length allPins

blink :: Atom ()
blink = do
  hour <- int8 "hour" 0
  min5 <- int8 "min5" 0
  pinIdx  <- int8 "pinIdx" 0
  --
  leds <- array "leds" $ replicate (length pinLists) 0 -- :: Atom (A Int8)

  on <- bool "on" False


  period 100 $ phase 0 $ atom "writeToPins" $ do
    call "zero_pins"
    forM (zip (map (fromIntegral . head) pinLists) [0..]) $ \(pIdx, offset) -> atom ("led" ++ show offset) $ do
      let offset' :: E Int8
          offset' = Const $ fromIntegral offset
          lightVal = leds !. offset'
      cond $ not_ $ Eq lightVal 0
      pinIdx <== pIdx + (mod_ (lightVal - 1) 3)
      on <== true
      call "avr_blink"

  period 10000 $ phase 0 $ atom "updateLEDs" $ do
    zeroArray leds (fromIntegral $ length pinLists)
    forM hourMin5Colorings $ \(h,m5,css) -> atom ("hm5_" ++ show h ++ "_" ++ show m5) $ do
      cond $ (value hour) `Eq` (Const (fromIntegral h))
      cond $ (value min5) `Eq` (Const (fromIntegral m5))
      let cs = head css
      forM (zip cs [0..]) $ \(c,offset) -> do
        (leds ! (Const offset :: E Int8)) <== Const (fromIntegral c)
      

  period 100 $ phase 0 $ atom "readPin" $ do
    call "check_buttons"

  where
    ph = 50000

zeroArray :: A Int8 -> Int8 -> Atom ()
zeroArray arr len = forM_ [0..len] $ \off -> do
  (arr ! (Const off)) <== 0



-- | Invoke the Atom compiler.
main :: IO ()
main = do
  (sch, _, _, _, _) <- compile atomName defaults {cCode = prePostCode} blink
  putStrLn $ reportSchedule sch
  createDirectoryIfMissing False dirName
  copyFile (atomName ++ ".c") (dirName ++ "/" ++ atomName ++ ".ino")
  copyFile (atomName ++ ".h") (dirName ++ "/" ++ atomName ++ ".h")
  removeFile (atomName ++ ".c")
  removeFile (atomName ++ ".h")


dirName = atomName
atomName = "FibClock"

varInit t var val = cType t ++ " " ++ var ++ " = " ++ val ++ ";"
pinOutput i = "  pinMode(" ++ show i ++ ", OUTPUT);"


prePostCode _ _ _ =
  ( unlines
    [ "#include <Bounce.h>"
    , (varInit Int32 "minButtonPin" "12")
    , (varInit Int32 "hourButtonPin" "11")
    , "Bounce minButton = Bounce(minButtonPin, 1);"
    , "Bounce hourButton = Bounce(hourButtonPin, 1);"
    , (varInit Int16 "ledPin" "13")
    , "void avr_blink(void);"
    , "void check_buttons(void);"
    ]
  , unlines (
    [ "void setup() {"
    , "  // initialize the digital pin as an output:"
    , "  pinMode(ledPin, OUTPUT);"
    , "  pinMode(minButtonPin, INPUT);"
    , "  pinMode(hourButtonPin, INPUT);"
    ]
    ++ map pinOutput allPins ++
    [ "}"
    , ""
    , "// set the LED on or off"
    , "void avr_blink() { " ++ digitalWrite "state.FibClock.pinIdx" "state.FibClock.on" ++ " }"
    , "void check_buttons() {"
    , "  if (hourButton.update() && hourButton.fallingEdge()) {"
    , "    state.FibClock.hour = (state.FibClock.hour+1) % 12;"
    , "  }"
    , "  if (minButton.update() && minButton.fallingEdge()) {"
    , "    state.FibClock.min5 = (state.FibClock.min5+1) % 12;"
    , "  }"
    , "}"
    , "void zero_pins() {" ]
    ++ map (\p -> "  " ++ digitalWrite (show p) "LOW") allPins ++
    [ "}"
    , ""
    , "void loop() {"
    ,  "  " ++ atomName ++ "();"
    , "}"
    ])
  )

digitalWrite :: String -> String -> String
digitalWrite p o = "digitalWrite(" ++ p ++ ", " ++ o ++ ");"

