-- data Canvas = Canvas Int Int [Row]

-- initCanvas :: Int -> Int -> Canvas
-- initCanvas w h =
--   let
--     row = replicate w base
--     canvas = replicate h row
--   in
--     Canvas (w*brailleWidth) (h*brailleHeight) canvas


-- printCanvas (Canvas _ _ canvas) =
--   mapM_ (\r -> mapM_ (\r' -> putStr $ [chr r']) r >>= \_ -> putStrLn "") canvas
-- 
-- canvasToString :: Canvas -> String
-- canvasToString (Canvas _ _ canvas) =
--   foldl (\s r -> s ++ (map (\r' -> chr r') r) ++ "\n") "" canvas

-- setDot (Canvas w h canvas) x y =
--   let
--     (x',xDot)  = x `quotRem` brailleWidth
--     (y',yDot)  = y `quotRem` brailleHeight
--     (rh,r:rs)  = splitAt y' canvas -- Get the row to modify
--     (ch,c:cs)  = splitAt x' r -- Get the column to modify
--     c'         = setBit c xDot yDot
--     r'         = ch ++ [c'] ++ cs
--     canvas'    = rh ++ [r'] ++ rs
--   in
--     if x < w && y < h then
--       Canvas w h canvas'
--     else
--       Canvas w h canvas


-- basicTest :: IO ()
-- basicTest = do
--   let c = initCanvas 20 5
--   let ds = [(0,0), (1,0), (0,1), (39,0), (38,0), (39,1), (39,19), (39,18), (38,19), (0,19), (1,19), (0,18)]
--   let c' = foldl (\c (a,b) -> setDot c a b) c ds
--   printCanvas c'

-- sampleCanvas = 
--   let
--     c = initCanvas 20 5
--     ds = [(0,0), (1,0), (0,1), (39,0), (38,0), (39,1), (39,19), (39,18), (38,19), (0,19), (1,19), (0,18)]
--     canvas = foldl (\c (a,b) -> setDot c a b) c ds
--   in
--     canvas

-- ui :: Widget ()
-- ui = 
--   let
--     c = initCanvas 20 5
--     ds = [(0,0), (1,0), (0,1), (39,0), (38,0), (39,1), (39,19), (39,18), (38,19), (0,19), (1,19), (0,18)]
--     c' = foldl (\c (a,b) -> setDot c a b) c ds
--   in
--     str $ canvasToString $ c'

