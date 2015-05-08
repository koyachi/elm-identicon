import Identicon exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

code = -2044886870
size = 200
width = 200
height = 200

main : Element
main = collage width height [
--        renderGuide width height,
        renderIdenticon code size
       ]
