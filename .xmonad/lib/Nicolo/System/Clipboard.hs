module Nicolo.System.Clipboard where

import System.Process
import System.IO (hPutStr, hClose)

setClipboardString :: String -> IO ()
setClipboardString str = do
  (Just inp, _, _, _) <- createProcess
                         (proc "xclip" ["-selection", "clipboard"])
                         { std_in = CreatePipe }
  hPutStr inp str
  hClose inp
