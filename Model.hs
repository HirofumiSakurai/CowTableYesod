module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import qualified Data.Text as T
import           Data.Time (showGregorian)
import           Prelude (read)

--  "%Y-%m-%d" like "2000-01-02"
instance FromJSON Day where
    parseJSON (String t) = return $ fromGregorian y m d
      where
        [y, m', d'] = map (read.(T.unpack)) $ T.split (== '-') t
        m = (fromIntegral m') :: Int
        d = (fromIntegral d') :: Int

instance ToJSON Day where
    toJSON day = String (T.pack $ showGregorian day)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
--share [mkPersist sqlSettings, mkMigrate "migrateAll"]
share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "config/models")
