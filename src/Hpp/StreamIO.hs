{-# LANGUAGE LambdaCase #-}
-- | IO on streams.
module Hpp.StreamIO where
import Control.Monad.IO.Class (MonadIO, liftIO)
import Hpp.Streamer
import Hpp.Types
import System.Directory (getTemporaryDirectory, renameFile, removeFile)
import System.IO (IOMode(ReadMode), hClose, hPutStr, openTempFile, openFile,
                  hGetLine, hIsEOF, hIsClosed, hSetBuffering, BufferMode(..))

-- | @sourceFile registerCleanup filePath@ produces a 'Source' of
-- lines from file @filePath@ after registering an action that closes
-- the file using the provided @registerCleanup@ function.
sourceFile :: (MonadIO m, MonadIO m')
           => (Cleanup -> m' ()) -> FilePath -> m' (Source m String ())
sourceFile register fp =
  do h <- liftIO $ do h <- openFile fp ReadMode
                      hSetBuffering h (BlockBuffering Nothing)
                      return h
     (cleanup,neutralize) <- liftIO $ mkCleanup (hClose h)
     let {-# INLINABLE go #-}
         go :: MonadIO m => Source m String ()
         go = Streamer $
              do closed <- liftIO $ hIsClosed h
                 if closed
                 then return $ Done (Just ())
                 else do eof <- liftIO $ hIsEOF h
                         if eof
                         then Done (Just ()) <$ (liftIO (neutralize >> hClose h))
                         else liftIO (fmap (flip Yield go) (hGetLine h))
                         -- else liftIO (hGetLine h) >>= return . flip Yield go -- . (++"\n")
     register cleanup >> return go
{-# INLINE sourceFile #-}

-- | Incrementally writes 'String's to a temporary file. When all
-- input is exhausted, the temporary file is renamed to the supplied
-- 'FilePath'.
sinkToFile :: MonadIO m
           => (Cleanup -> m ()) -> FilePath -> Streamer m String o ()
sinkToFile register fp = Streamer$
  do (tmp,h) <- liftIO $ getTemporaryDirectory >>= flip openTempFile "hpp.tmp"
     (cleanup, neutralize) <- liftIO $ mkCleanup (hClose h >> removeFile tmp)
     let dunzo = Streamer . liftIO $ do neutralize
                                        hClose h
                                        renameFile tmp fp
                                        return (Done (Just ()))
         go = encase $ Await (\s -> Streamer $
                                    liftIO (hPutStr h s) >> runStream go)
                             dunzo
                             
     register cleanup
     runStream go
{-# INLINE sinkToFile #-}

-- | Sink a stream with a function evaluated only for its
-- side-effects.
sinkTell :: Monad m => (a -> m ()) -> Streamer m a o ()
sinkTell tell = go
  where go = awaits (\i -> Streamer (tell i >> runStream go))
{-# INLINE sinkTell #-}

-- | Sink a stream to 'System.IO.stdout'
sinkToStdOut :: MonadIO m => Streamer m String o ()
sinkToStdOut = sinkTell (liftIO . putStr)
{-# INLINE sinkToStdOut #-}

-- | @sink_ = forever await@ Simply discards all inputs. This may be
-- used to exhaust a stream solely for its effects.
sink_ :: Monad m => Streamer m i o ()
sink_ = awaits (const sink_)
