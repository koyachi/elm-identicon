import Identicon exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

code = -2044886870
size = 200
width = 400
height = 400

main : Element
main = collage width height [
        renderGuide width height,
        renderIdenticon code size
       ]
