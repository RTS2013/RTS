import Graphics.Gloss
import Control.Monad.Writer.Strict
import Data.Map.Strict

data Button a = Button {btn_x, btn_y, btn_w, bth_h :: Float, btn_clicked :: (Float,Float) -> a)}

mkButton :: (Picture,Picture) -> (Either (Float,Float) Bool -> a) -> (Float,Float) -> (Float,Float) -> Button a
mkButton = undefined

main = display (InWindow "Nice Window" (200, 200) (0,0)) white (translate 40 40 $ Circle 80)