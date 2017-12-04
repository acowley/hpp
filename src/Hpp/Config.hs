-- | Preprocessor Configuration
module Hpp.Config where
import Data.Functor.Identity
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Format

-- | A 'String' representing a time.
newtype TimeString = TimeString { getTimeString :: String }
  deriving (Eq, Ord, Show)

-- | A 'String' representing a date.
newtype DateString = DateString { getDateString :: String }
  deriving (Eq, Ord, Show)

-- | Pre-processor configuration parameterized over a functor. This is
-- used to normalize partial configurations, @ConfigF Maybe@, and
-- configurations suitable for the pre-processor logic, @ConfigF
-- Identity@. Specifically, the source file name of the file being
-- processed /must/ be set.
data ConfigF f = Config { curFileNameF        :: f FilePath
                          -- ^ Name of the file being
                          -- preprocessed. Hpp will update this as new
                          -- files are included. The user must set it
                          -- manually for the starting input file.
                        , includePathsF       :: f [FilePath]
                        -- ^ Paths to be searched for included files.
                        , spliceLongLinesF    :: f Bool
                        -- ^ A backslash as the last character of a
                        -- line causes the next line to be appended to
                        -- the current one eliding the newline
                        -- character present in the source input.
                        , eraseCCommentsF     :: f Bool
                        -- ^ Erase line comments (starting with @//@)
                        -- and block comments (delimited by @/*@ and
                        -- @*/@).
                        , inhibitLinemarkersF :: f Bool
                        -- ^ Do not emit @#line@ directives.
                        , replaceTrigraphsF   :: f Bool
                        -- ^ Replace trigraph sequences (each of which
                        -- starts with two consecutive question marks
                        -- (@\"??\"@) with the characters they encode.
                        , prepDateF           :: f DateString
                        -- ^ Format string for @\_\_DATE\_\_@.
                        , prepTimeF           :: f TimeString
                        -- ^ Format string for @\_\_TIME\_\_@.
                       }

-- | A fully-populated configuration for the pre-processor.
type Config = ConfigF Identity

-- | Ensure that required configuration fields are supplied.
realizeConfig :: ConfigF Maybe -> Maybe Config
realizeConfig (Config (Just fileName)
                      (Just paths)
                      (Just spliceLines)
                      (Just comments)
                      (Just inhibitLines)
                      (Just trigraphs)
                      (Just pdate)
                      (Just ptime)) =
  Just (Config (pure fileName) (pure paths) (pure spliceLines) (pure comments)
               (pure inhibitLines) (pure trigraphs) (pure pdate) (pure ptime))
realizeConfig _ = Nothing

-- | Extract the current file name from a configuration.
curFileName :: Config -> FilePath
curFileName = runIdentity . curFileNameF

-- | Extract the include paths name from a configuration.
includePaths :: Config -> [FilePath]
includePaths = runIdentity . includePathsF

-- | Determine if continued long lines should be spliced.
spliceLongLines :: Config -> Bool
spliceLongLines = runIdentity . spliceLongLinesF

-- | Determine if C-style comments should be erased.
eraseCComments :: Config -> Bool
eraseCComments = runIdentity . eraseCCommentsF

-- | Determine if generation of linemarkers should be inhibited.
inhibitLinemarkers :: Config -> Bool
inhibitLinemarkers = runIdentity . inhibitLinemarkersF

-- | Determine if trigraph sequences should be replaced.
replaceTrigraphs :: Config -> Bool
replaceTrigraphs = runIdentity . replaceTrigraphsF

-- | The date the pre-processor was run on.
prepDate :: Config -> DateString
prepDate = runIdentity . prepDateF

-- | The time of the active pre-processor invocation.
prepTime :: Config -> TimeString
prepTime = runIdentity . prepTimeF

-- | A default configuration with no current file name set. Note that
-- long line splicing is enabled, C++-style comments are erased, #line
-- markers are inhibited, and trigraph replacement is disabled.
defaultConfigF :: ConfigF Maybe
defaultConfigF = Config Nothing (Just [])
                        (Just True) (Just True) (Just True) (Just False)
                        (Just (DateString "??? ?? ????"))
                        (Just (TimeString "??:??:??"))

-- | Format a date according to the C spec.
formatPrepDate :: UTCTime -> DateString
formatPrepDate = DateString . formatTime defaultTimeLocale "%b %e %Y"

-- | Format a time according to the C spec.
formatPrepTime :: UTCTime -> TimeString
formatPrepTime = TimeString . formatTime defaultTimeLocale "%T"

-- | A default preprocessor configuration with date and time stamps
-- taken from the current system time.
defaultConfigFNow :: IO (ConfigF Maybe)
defaultConfigFNow = do now <- getCurrentTime
                       let d = formatPrepDate now
                           t = formatPrepTime now
                       return $ defaultConfigF { prepDateF = Just d
                                               , prepTimeF = Just t }
