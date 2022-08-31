
withTwoFiles ::
  FilePath -> IOMode ->
  FilePath -> IOMode ->
  (Handle -> Handle -> IO r) -> IO r
withTwoFiles fp md fp' md' action =
  withFile fp md (\h -> do
                     withFile fp' md' (action h))

withConfiguration :: (Environment -> IO a) -> (Configuration -> IO a)
withConfiguration action (Config out log dbg) =
  withTwoFiles out WriteMode log WriteMode
               $ \ho hl -> action (Env ho hl dbg)

log :: Int -> Text -> App ()
log lvl msg = do
  (Env outh logh debug) <- ask
  if debug >= lvl
    then liftIO $ T.hPutStr logh msg
    else return ()
  
writeLn :: Text -> App ()
writeLn msg = do
  (Env outh _ _) <- ask
  liftIO $ T.hPutStr outh msg
    
type App a = ReaderT Environment IO a

runApp_ :: App a -> Environment -> IO a
runApp_ = runReaderT

runApp :: Configuration -> App a -> IO a
runApp cnf app = withConfiguration (runApp_ app) cnf
