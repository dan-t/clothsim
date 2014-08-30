{-# LANGUAGE PatternGuards #-}

import System.Exit (exitSuccess)
import qualified Debug.Trace as T
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Control.Applicative ((<$>))
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as VM
import Data.Vector ((!), (//))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as GFX
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Matrix as M
import qualified Gamgine.Math.Utils as MU
import Gamgine.Math.Vect
import Gamgine.Gfx ((<<<))


type Point  = V.Vect
type Points = Vec.Vector Point

gridWidth   = 20 :: Int
gridHeight  = 20 :: Int

dGridWidth  = fromIntegral gridWidth
dGridHeight = fromIntegral gridHeight

borderWidth = (fromIntegral $ max gridWidth gridHeight)
edgeLength  =  1 :: Double

fixedPoint :: Int -> Bool
fixedPoint i =
   i == gridWidth * (gridHeight - 1)
      || i == (gridWidth * gridHeight) - 1


makeGrid :: Int -> Int -> Double -> Points
makeGrid width length edgeLength =
   Vec.generate (width * length) $ \i ->
      let (quot, rem) = i `quotRem` width
          x           = fromIntegral rem * edgeLength
          y           = fromIntegral quot * edgeLength
          in (x:.y:.0)


main :: IO ()
main = do
   GLFW.init
   GLFW.windowHint $ GLFW.WindowHint'Resizable True
   GLFW.swapInterval 1
   Just win <- GLFW.createWindow 800 800 "" Nothing Nothing
   initCallbacks win
   GLFW.makeContextCurrent (Just win)

   appLoop win Nothing $ makeGrid gridWidth gridHeight edgeLength


appLoop :: GLFW.Window -> Maybe Int -> Points -> IO ()
appLoop win currIdx points = do
   GLFW.pollEvents

   GL.glClearColor 0 0 0 0
   GL.glClear (fromIntegral GL.gl_COLOR_BUFFER_BIT)
   renderLines points
   let pts' = applyGravity . applyConstraints $ points

   mworld    <- mousePosInWorldCoords win
   gridTrans <- gridTranslation win
   let mgrid = mworld - gridTrans

   pressed <- isButtonPressed win GLFW.MouseButton'1
   let currIdx' | not pressed        = Nothing
                | Nothing <- currIdx = findClosestPoint mgrid pts'
                | otherwise          = currIdx

   case currIdx' of
        Just idx -> do
           GL.glPointSize 10
           GL.glColor3f <<< ((1,0,0) :: GFX.RGB)
           GFX.withPrimitive GL.gl_POINTS $
              GL.glVertex3f <<< (pts' ! idx)

           let pts'' | pressed   = pts' // [(idx, mgrid)]
                     | otherwise = pts'

           GLFW.swapBuffers win
           appLoop win currIdx' pts''

        _        -> do
           GLFW.swapBuffers win
           appLoop win currIdx' pts'
   where
      isButtonPressed win button = (== GLFW.MouseButtonState'Pressed) <$> GLFW.getMouseButton win button


renderLines :: Points -> IO ()
renderLines points = do
   GL.glColor3f <<< ((1,1,1) :: GFX.RGB)
   -- verticals
   forM_ [0 .. gridWidth - 1] $ \x ->
      GFX.withPrimitive GL.gl_LINE_STRIP $
         forM_ [0 .. gridHeight - 1] $ \y ->
            GL.glVertex3f <<< (points ! (y * gridWidth + x))

   -- horizontals
   forM_ [0 .. gridHeight - 1] $ \y ->
      GFX.withPrimitive GL.gl_LINE_STRIP $
         forM_ [0 .. gridWidth - 1] $ \x ->
            GL.glVertex3f <<< (points ! (y * gridWidth + x))


findClosestPoint :: V.Vect -> Points -> Maybe Int
findClosestPoint pos points =
   let (_, idx, _) = Vec.foldl' compDist (0, -1, edgeLength) points
       in if idx == -1 then Nothing else Just idx
   where
      compDist (i, idx, dist) p =
        let dist' = V.len $ pos - p
            in if dist' < dist
                  then (i + 1, i, dist')
                  else (i + 1, idx, dist)


applyGravity :: Points -> Points
applyGravity points = runST (apply points)
   where
      apply points = do
         mutPts <- Vec.thaw points
         forM_ [0 .. (gridWidth * gridHeight - 1)] $ \i -> do
            when (not $ fixedPoint i) $ do
               pt <- VM.read mutPts i
               VM.write mutPts i (pt - gravity)

         Vec.freeze mutPts

      gravity = (0:. (edgeLength * 0.02) :.0:.())


applyConstraints :: Points -> Points
applyConstraints points = runST (apply points)
   where
      apply points = do
         mutPts <- Vec.thaw points
         forM_ [0 .. (gridWidth * gridHeight - 1)] $ \i -> do
            let (y, x) = i `quotRem` gridWidth
            when (x > 0)                $ constrain i (i - 1) mutPts
            when (x < (gridWidth - 1))  $ constrain i (i + 1) mutPts
            when (y > 0)                $ constrain i (i - gridWidth) mutPts
            when (y < (gridHeight - 1)) $ constrain i (i + gridWidth) mutPts

         Vec.freeze mutPts

      constrain i j pts = do
         iPt <- VM.read pts i
         jPt <- VM.read pts j
         let diff    = iPt - jPt
             norm    = V.normalize diff
             len     = V.len diff
             iFix    = fixedPoint i
             jFix    = fixedPoint j
             ptsAway = len >= edgeLength
             disp    = abs (len - edgeLength)

             iT | iFix      = 0
                | jFix      = disp
                | otherwise = disp * 0.5

             jT | jFix      = 0
                | iFix      = disp
                | otherwise = disp * 0.5

             iVec = norm * V.v3 iT iT iT
             jVec = norm * V.v3 jT jT jT

         VM.write pts i (if ptsAway then iPt - iVec else iPt + iVec)
         VM.write pts j (if ptsAway then jPt + jVec else jPt - jVec)


initCallbacks :: GLFW.Window -> IO ()
initCallbacks win = do
   GLFW.setWindowSizeCallback win (Just resize)
   GLFW.setWindowCloseCallback win (Just quit)
   where
      resize win width height = do
         let M.Frustum {M.right = right, M.top = top} = frustum (width, height)

	 GL.glViewport 0 0 (fromIntegral width) (fromIntegral height)
	 GL.glMatrixMode GL.gl_PROJECTION
	 GL.glLoadIdentity
         GL.glOrtho 0 (GFX.floatToFloat right) 0 (GFX.floatToFloat top) (-1) 1

         GL.glMatrixMode GL.gl_MODELVIEW
         GL.glLoadIdentity
         gridTrans <- gridTranslation win
         GL.glTranslatef <<< gridTrans

      quit win = GLFW.destroyWindow win >> GLFW.terminate >> exitSuccess


mousePosInWorldCoords :: GLFW.Window -> IO V.Vect
mousePosInWorldCoords win = do
   winDims  <- GLFW.getWindowSize win
   (cx, cy) <- GLFW.getCursorPos win
   let winToWorldMtx = M.mkWinToWorldMatrix winDims (frustum winDims)
   return $ V.setElem 2 0 $ M.winToWorld winToWorldMtx (floor cx, floor cy)


gridTranslation :: GLFW.Window -> IO V.Vect
gridTranslation win = do
   M.Frustum {M.right = r, M.top = t} <- frustum <$> GLFW.getWindowSize win
   let transX = (r - dGridWidth) / 2
       transY = (t - dGridHeight) / 2
   return (transX:.transY:.0)


frustum :: (Int, Int) -> M.Frustum
frustum (width, height) =
   let dWidth  = fromIntegral width  :: Double
       dHeight = fromIntegral height :: Double
       maxSide = max dGridWidth dGridHeight + (2 * borderWidth)
       (right, top) | dWidth < dHeight = (maxSide, maxSide * (dHeight / dWidth))
                    | otherwise        = (maxSide * (dWidth / dHeight), maxSide)
       in M.Frustum 0 right 0 top (-1) 1
