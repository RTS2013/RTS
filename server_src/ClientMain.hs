{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import qualified Local.Matrices.UnboxedMatrix2D as Mat
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)
import Graphics.Gloss.Interface.IO.Game hiding (shift,ctrl,alt)
import Graphics.Gloss
import Control.Concurrent.STM.TVar
import Local.WindowSize (getWindowSize)
import Control.Monad.Primitive (PrimState)
import ClientNetwork (connectToServer)
import GUI
import Data

data UserState = UserState
    { shift  :: Bool
    , alt    :: Bool
    , ctrl   :: Bool
    , left   :: Bool
    , middle :: Bool
    , right  :: Bool
    } 

data UserAction = NoAction 
                | ClickingButton String
                | MovingScreen (Float,Float)
                | DragSelecting (Float,Float)
                | SelectingPoint String
                deriving (Eq)

data ClientActor = ClientActor
    { ca_id :: Int
    , ca_team :: Word8
    , ca_x :: Float
    , ca_y :: Float
    } 

data ClientWorld = CW
    { cw_terrain :: Mat.Matrix (V.MVector (PrimState IO) Int)
    , cw_entities :: IM.IntMap ClientActor
    , cw_effects :: M.Map String (ClientWorld -> IO ClientWorld)
    , cw_buttons :: [Button]
    , cw_userState :: UserState
    , cw_userAction :: UserAction
    , cw_viewport :: (Float,Float,Float,Float)
    } 

defaultUserState = UserState False False False False False False

main = do 
    -- (tcpSock,udpSock) <- connectToServer

    winDims <- getWindowSize
    let renderButtons btns = pictures $ map button2Picture btns
        buttons = fst $ arrange Southeast West winDims
            [ makeColorButton (64,64) red "red"
            , makeColorButton (64,64) blue "blue"
            , makeColorButton (64,64) yellow "yellow"
            , makeColorButton (64,64) green "green"
            ] 

    mapMatrix <- Mat.make 1024 1024 0

    playIO (FullScreen winDims) 
        -- Background color
        black
         -- FPS
        60
         -- Starting world state
        (CW mapMatrix IM.empty M.empty buttons defaultUserState NoAction (0,0,0,0))
        renderWorld
        eventHandler
        (\_ w -> return w)

renderWorld :: ClientWorld -> IO Picture
renderWorld w = do
    let buttons = pictures . map button2Picture $ cw_buttons w
        entities = pictures . fmap actor2Picture $ IM.elems $ cw_entities w
    return $ pictures [buttons,entities]

actor2Picture :: ClientActor -> Picture
actor2Picture act =
    let team = ca_team act
        colr = if team == 0 then red else
               if team == 1 then blue else
               if team == 2 then green else
               if team == 3 then magenta else white 
        (x,y) = (ca_x act, ca_y act) in
    translate x y $ color colr $ thickCircle 0.1 0.25

applyMouseLeft :: (Float,Float) -> ClientWorld -> IO ClientWorld
applyMouseLeft (x,y) w@(CW {cw_userState=(UserState {..}),..})
    -- Button has been pressed. It's action will be performed when it is depressed :(
    | left && not right && noAction = return $ 
        maybe w (\name -> w {cw_userAction = ClickingButton name}) (selectButton x y cw_buttons)
    -- Depress Button and (possibly) perform its action
    | not left && not right && clicking = 
        maybe (return w) 
            (\name -> if name == clickedBtn 
                      then M.findWithDefault (return . id) name cw_effects $ w 
                      else return w
            ) 
            (selectButton x y cw_buttons)
    -- Start dragging screen around
    | left && right && noAction = return $ w {cw_userAction = MovingScreen (x,y)}
    | otherwise = return w
    where
    noAction = cw_userAction == NoAction
    clicking = case cw_userAction of { ClickingButton _ -> True; _ -> False }
    clickedBtn = case cw_userAction of { ClickingButton str -> str }

applyMouseRight :: (Float,Float) -> ClientWorld -> IO ClientWorld
applyMouseRight (x,y) w@(CW {cw_userState=(UserState {..}),..}) = return w

applyMouseMiddle :: (Float,Float) -> ClientWorld -> IO ClientWorld
applyMouseMiddle (x,y) w@(CW {cw_userState=(UserState {..}),..}) = return w





eventHandler :: Event -> ClientWorld -> IO ClientWorld
eventHandler (EventKey (MouseButton LeftButton) leftS (Modifiers shiftS ctrlS altS) (x,y)) w =
    applyMouseLeft (x,y) $ w { cw_userState = (cw_userState w) 
                                                        { left  = leftS  == Down
                                                        , shift = shiftS == Down
                                                        , ctrl  = ctrlS  == Down
                                                        , alt   = altS   == Down }}
eventHandler (EventKey (MouseButton RightButton) rightS (Modifiers shiftS ctrlS altS) (x,y)) w =
    applyMouseRight (x,y) $ w { cw_userState = (cw_userState w) 
                                                        { right = rightS == Down
                                                        , shift = shiftS == Down
                                                        , ctrl  = ctrlS  == Down
                                                        , alt   = altS   == Down }}
eventHandler (EventKey (MouseButton MiddleButton) middleS (Modifiers shiftS ctrlS altS) (x,y)) w =
    applyMouseMiddle (x,y) $ w { cw_userState = (cw_userState w) 
                                                        { middle = middleS == Down
                                                        , shift  = shiftS  == Down
                                                        , ctrl   = ctrlS   == Down
                                                        , alt    = altS    == Down }}
eventHandler (EventMotion (x,y)) w = return w
eventHandler _ w = return w