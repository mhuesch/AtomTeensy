{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Language.Atom
import Control.Monad
import Data.Int
import System.Directory

bLo = 2
bHi = 10

mLo = 15
mHi = 17

tLo = 21
tHi = 23

allPins = [bLo..bHi]++[mLo..mHi]++[tLo..tHi]
numPins = fromIntegral $ length allPins

blink :: Atom ()
blink = do
  muckIdx <- int32 "muckIdx" 0
  dispIdx <- int32 "dispIdx" 0
  pinIdx  <- int32 "pinIdx" 0
  leds <- array "leds" $ replicate numPins True

  -- How much we step by
  muckInc <- int32 "muckInc" 0

  buttonPushed <- bool "buttonPushed" False

  on <- bool "on" False

  period 100 $ phase 0 $ atom "displayArray" $ do
    forM (zip allPins [0..]) $ \(p,offset) -> atom ("pin" ++ show p) $ do
      cond $ value dispIdx ==. Const offset
      pinIdx <== Const p
      on     <== leds !. (value dispIdx)
      call "avr_blink"
      incr dispIdx

    atom "loopDown" $ do
      cond $ value dispIdx >=. Const numPins
      dispIdx <== 0

  period ph $ phase 0 $ atom "muckArray" $ do
    atom "loopDown" $ do
      cond $ value muckIdx >=. Const numPins
      muckIdx <== (value muckIdx) - numPins
    --
    atom "muck" $ do
      cond $ value muckIdx <. Const numPins
      zeroArray leds numPins
      let tmp = leds ! (value muckIdx)
      tmp <== true
      muckIdx <== (value muckIdx) + (value muckInc)

  period 10 $ phase 0 $ atom "readPin" $ do
    call "check_buttons"

  where
    ph = 50000

zeroArray :: A Bool -> Int32 -> Atom ()
zeroArray arr len = forM_ [0..len] $ \off -> do
  (arr ! (Const off)) <== false

negateV :: V Bool -> Atom ()
negateV x = (x <== not_ (value x))



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
    , (varInit Int32 "upButtonPin" "12")
    , (varInit Int32 "downButtonPin" "11")
    , "Bounce upButton = Bounce(upButtonPin, 1);"
    , "Bounce downButton = Bounce(downButtonPin, 1);"
    , (varInit Int16 "ledPin" "13")
--    , "// No debounce"
--    , (varInit Int32 "current_state" "1")
--    , (varInit Int32 "prev_state" "1")
    , "void avr_blink(void);"
    , "void check_buttons(void);"
    ]
  , unlines (
    [ "void setup() {"
    , "  // initialize the digital pin as an output:"
    , "  pinMode(ledPin, OUTPUT);"
    , "  pinMode(upButtonPin, INPUT);"
    , "  pinMode(downButtonPin, INPUT);"
    ]
    ++ map pinOutput allPins ++
    [ "}"
    , ""
    , "// set the LED on or off"
    , "void avr_blink() { digitalWrite(state.FibClock.pinIdx, state.FibClock.on); }"
    , "void check_buttons() {"
--    , "  current_state = digitalRead(upButtonPin);"
--    , "  if ((prev_state != current_state) && (current_state == HIGH)) {"
--    , "    state.FibClock.muckInc += 1;"
--    , "  }"
--    , "  prev_state = current_state;"
    , "  if (upButton.update() && upButton.fallingEdge()) {"
    , "    state.FibClock.muckInc += 1;"
    , "  }"
    , "  if (downButton.update() && downButton.fallingEdge()) {"
    , "    state.FibClock.muckInc -= 1;"
    , "  }"
    , "}"
    , ""
    , "void loop() {"
    ,  "  " ++ atomName ++ "();"
    , "}"
    ])
  )
