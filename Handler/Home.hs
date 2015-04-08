module Handler.Home where

import Import
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
--                              withSmallInput)
import Yesod.Form.Bootstrap3 ()

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "CowTableYesod"
  [whamlet|
<h1>表示項目の選択と、クエリの入力
<form action=@{ESelectR} method="GET">
    耳標番号:<input name="earNum" type="checkbox" checked value="1">
    名前:<input name="name" type="checkbox" checked value="1">
    誕生日:<input name="birth" type="checkbox" checked value="1">
    性別:<input name="sex" type="checkbox" checked value="1">
    所有者名:<input name="ownerName" type="checkbox" checked value="1">
    <p>
    t1-10:<input name="t1" type="checkbox" checked value="1">
    t11-20:<input name="t2" type="checkbox" checked value="1">
    t21-30:<input name="t3" type="checkbox" checked value="1">
    t31-40:<input name="t4" type="checkbox" checked value="1">
    t41-50:<input name="t5" type="checkbox" checked value="1">
    <p>
    検索条件(where):Owner.name(Default:Owner5)=
    <input name="OwnerName" type="text" value="Owner5">
    <p>
    <input type="submit" value="送信">
                 |]

