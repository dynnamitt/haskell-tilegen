import Text.XML.HXT.Core
-- import Text.XML.HXT....   -- further HXT packages
  
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit
   
main :: IO ()
main = do
  argv <- getArgs
  let inp = argv !! 0
  let out = argv !! 1
  [rc]  <- runX (application [withValidate no] inp out)
  if rc >= c_err
    then exitWith (ExitFailure (0-1))
    else exitWith ExitSuccess
                                        
-- | the main arrow
application :: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst
  = configSysVars cfg                 
    >>>
    readDocument [] src
    >>>
    processChildren (processDocumentRootElement `when` isElem)
    >>>
    writeDocument [] dst
    >>>
    getErrStatus
                                                           
                                                            
-- | the dummy for the real processing: the identity filter
processDocumentRootElement :: IOSArrow XmlTree XmlTree
processDocumentRootElement
  = this
