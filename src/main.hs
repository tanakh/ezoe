{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.ByteString.Char8        as S
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Lazy               as T
import qualified Data.Text.Lazy.Encoding      as T
import           Network.HTTP.Conduit
import           Options.Declarative
import           Text.Feed.Import
import           Text.Feed.Query
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import           Text.Regex.TDFA

highlights :: String
highlights = "質問ではない。?|不?自由"

render :: String -> Doc
render "" = mempty
render ss =
    let (before, cur, after) = ss =~ highlights
    in string before <> green (string cur) <> render after

post :: String -> String -> IO ()
post user question = do
    doc <- simpleHttp $ "http://ask.fm/" <> user
    let token =
            fromMaybe (error "cannot get authenticity_token") $ listToMaybe
            [ fromAttrib "value" tag
            | tag <- parseTags $ T.unpack $ T.decodeUtf8 doc
            , tagOpenAttrLit "input" ("name", "authenticity_token") tag
            ]

    req <- parseUrl ("http://ask.fm/" <> user <> "/questions/create")
    withManager $ \mng -> do
        let req' =
                setQueryString
                [ ("authenticity_token", Just $ S.pack token)
                , ("question[question_text]", Just $ S.pack question)
                , ("question[force_anonymous]", Just "force_anonymous")
                ] req
                { method = "POST"
                , requestHeaders = [("Referer", S.pack $ "http://ask.fm/" <> user)]
                }
        void $ httpLbs req' mng

viewAns :: String -> IO ()
viewAns user = do
    ss <- simpleHttp $ "http://ask.fm/feed/profile/" <> user <> ".rss"
    let Just feed = parseFeedString $ T.unpack $ T.decodeUtf8 ss

    forM_ (reverse $ feedItems feed) $ \item ->
        mapM_ print $ do
            url      <- getItemLink item
            question <- getItemTitle item
            answer   <- getItemDescription item

            return $
                string url <$$>
                indent 2 (red (string question) <$$>
                          render answer) <$$>
                mempty

ezoe :: Flag "u" '["user"] "STRING" "user name" (Def "tanakh184" String)
     -> Arg "QUESTION" [String]
     -> Cmd "Ask.fm tools"
ezoe user question = Cmd $ case get question of
    [] -> viewAns (get user)
    qs -> post (get user) (unwords qs)

main :: IO ()
main = run_ ezoe
