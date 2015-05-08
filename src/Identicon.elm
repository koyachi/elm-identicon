module Identicon where
{-|
-}


import Color
import Graphics.Collage as C
import Array
import Maybe
import Bitwise

patch0 = Array.fromList [0, 4, 24, 20]
patch1 = Array.fromList [0, 4, 20]
patch2 = Array.fromList [2, 24, 20]
patch3 = Array.fromList [0, 2,  20, 22]
patch4 = Array.fromList [2, 14, 22, 10]
patch5 = Array.fromList [0, 14, 24, 22]
patch6 = Array.fromList [2, 24, 22, 13, 11, 22, 20]
patch7 = Array.fromList [0, 14, 22]
patch8 = Array.fromList [6, 8, 18, 16]
patch9 = Array.fromList [4, 20, 10, 12, 2]
patch10 = Array.fromList [0, 2, 12, 10]
patch11 = Array.fromList [10, 14, 22]
patch12 = Array.fromList [20, 12, 24]
patch13 = Array.fromList [10, 2, 12]
patch14 = Array.fromList [0, 2, 10]

patchTypes = Array.fromList [patch0, patch1, patch2, patch3, patch4, patch5, patch6, patch7, patch8, patch9, patch10, patch11, patch12, patch13, patch14, patch0]

centerPatchTypes = Array.fromList [0, 4, 8, 15]

renderIdenticonPatch : Float -> Float -> Float -> Int -> Int -> Bool -> Color.Color -> Color.Color -> List C.Form
renderIdenticonPatch x y size patchTypeIndex turn invert foreColor backColor =
  let patchTypeIndex' = patchTypeIndex % (Array.length patchTypes)
      turn' = turn % 4
      invert' = if patchTypeIndex' == 15 then not invert else invert
      offset = size / 2
      scale = size / 4
      patchType = Maybe.withDefault (Array.fromList []) (patchTypes |> Array.get patchTypeIndex')

      flipSign = -1.0
      adjust v = v * scale - offset
      patchToXY p =
        ((round p) % 5 |> toFloat |> adjust,
         (floor (p / 5) |> toFloat |> adjust) * flipSign)

      vertices =
        patchType
          |> Array.map patchToXY
          |> Array.toList

      bgColor = if invert' then foreColor else backColor
      fgColor = if invert' then backColor else foreColor
  in
    [
     -- background
     C.rect size size
       |> C.filled bgColor
       |> C.move (x, y)
       |> flipVertically,
     -- build patch path
     C.polygon vertices
       |> C.filled fgColor
       |> C.rotate ((turn' |> toFloat) * pi / 2 * flipSign)
       |> C.move (x, y)
       |> flipVertically
    ]

renderIdenticon : Int -> Int -> C.Form
renderIdenticon code size =
  let patchSize = (size |> toFloat) / 3
      shiftRightAnd shiftBit andBit = (Bitwise.shiftRight code shiftBit) |> (Bitwise.and andBit)
      isShiftRightAndNotZero shiftBit andBit = ((Bitwise.shiftRight code shiftBit) |> (Bitwise.and andBit)) /= 0

      middleType = Maybe.withDefault 0 (centerPatchTypes |> Array.get (Bitwise.and code 3))
      middleInvert = isShiftRightAndNotZero 2 1
      cornerType = shiftRightAnd 3 15
      cornerInvert = isShiftRightAndNotZero 7 1
      cornerTurn = shiftRightAnd 8 3
      sideType = shiftRightAnd 10 15
      sideInvert = isShiftRightAndNotZero 14 1
      sideTurn = shiftRightAnd 15 3
      blue = shiftRightAnd 16 31
      green = shiftRightAnd 21 31
      red = shiftRightAnd 27 31
      foreColor = Color.rgba (Bitwise.shiftLeft red 3) (Bitwise.shiftLeft green 3) (Bitwise.shiftLeft blue 3) 1.0
      backColor = Color.rgba 0xff 0xff 0xff 1.0

      renderIdenticonPatch' x y size patch turn invert = renderIdenticonPatch x y size patch turn invert foreColor backColor
  in
    List.concat [
           -- middle patch
           renderIdenticonPatch' patchSize patchSize patchSize middleType 0 middleInvert,

           -- side patches, starting from top and moving clock-wise
           renderIdenticonPatch' patchSize 0 patchSize sideType (sideTurn + 0) sideInvert,
           renderIdenticonPatch' (patchSize*2) patchSize patchSize sideType (sideTurn + 1) sideInvert,
           renderIdenticonPatch' patchSize (patchSize*2) patchSize sideType (sideTurn + 2) sideInvert,
           renderIdenticonPatch' 0 patchSize patchSize sideType (sideTurn + 3) sideInvert,
           -- corner paths, starting from top left and moving clock-wise
           renderIdenticonPatch' 0 0 patchSize cornerType (cornerTurn + 0) cornerInvert,
           renderIdenticonPatch' (patchSize*2) 0 patchSize cornerType (cornerTurn + 1) cornerInvert,
           renderIdenticonPatch' (patchSize*2) (patchSize*2) patchSize cornerType (cornerTurn + 2) cornerInvert,
           renderIdenticonPatch' 0 (patchSize*2) patchSize cornerType (cornerTurn + 3) cornerInvert
          ]
      -- centering vertically fliped forms
      |> List.map (\f -> f |> C.move (-patchSize, patchSize))
      |> C.group

renderGuide : Float -> Float -> C.Form
renderGuide width height = 
  let bgColor = Color.rgba 0x00 0xb4 0xf5 1.0
      lineColor = Color.rgba 0x00 0x00 0x00 1.0
  in
    [C.rect width height
       |> C.filled bgColor,
     C.rect width 1.0
       |> C.filled lineColor,
     C.rect 1.0 height
       |> C.filled lineColor
    ] |> C.group

flipVertically : C.Form -> C.Form
flipVertically f =
  { f | y <- -f.y }