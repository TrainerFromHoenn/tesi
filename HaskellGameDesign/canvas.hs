import qualified "GLFW-b" Graphics.UI.GLFW as GLFW

-- funzione utile per creare la finestra
-- gestisce il boilerplate e tutte le cose che non mi interessano
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO()) -> IO()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
            (Just win) -> do
                GLFW.makeContextCurrent m
                f win 
                GLFW setErrorCallback $ Just simpleErrorCallback
                GLFW.destroyWindow win 
            Nothing -> return ()
        GLFW.terminate
    where
        simpleErrorCallback e s = 
            putStrLn $ unwords [show e, show s]

-- ora creare una finestra nella main function è stupidissimo
main :: IO()
main = do
    let width = 640
        height = 480
    withWindow width height "Prima finestra" $ \win -> do -- In fondo lambda function
    -- lambda utile per catturare input o swappare il buffer finestra

