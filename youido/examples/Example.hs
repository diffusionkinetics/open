{-# LANGUAGE OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   ExtendedDefaultRules, FlexibleContexts, TemplateHaskell, MultiParamTypeClasses,
   OverloadedLabels, TypeOperators, DataKinds, DeriveGeneric,
   FlexibleInstances, TypeApplications, GeneralizedNewtypeDeriving #-}

module Main where

import Youido.Serve
import Youido.Types
import Youido.Dashdo
import SumTypeExample
import Lucid
import Lucid.Bootstrap
import Lucid.Rdash
import Numeric.Datasets.Gapminder
import Numeric.Datasets
import Control.Monad.Reader
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.List (nub)
import Data.Text (Text, unpack)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Monoid
import Lens.Micro.Platform
--import Graphics.Plotly.Lucid.hs

import Dashdo.Elements
import Dashdo.FlexibleInput
import Dashdo.Types hiding (FormField)
import GHC.Generics
import Text.Digestive.View
import qualified Data.Text.IO as TIO

import qualified Text.Digestive as D
import qualified Text.Digestive.Lucid.Html5 as DL

data TodoR = ListTodos
           | EditTodoList
           | UpdateTodoList (Form TodoList)
  deriving (Show, Generic)

instance MonadIO m => FromRequest m TodoR
instance ToURL TodoR

data TodoList = TodoList {
  title :: Text,
  items :: [Todo],
  category :: Text
  }
  deriving (Show, Generic)

instance MonadIO m => FromForm m TodoList

data TodoTag = TodoTag { tag :: Text } deriving (Show, Generic)
instance  Monad m => FromForm m TodoTag

newtype Assignee = Assignee Int deriving (Generic, Show, Eq, Num)

instance MonadIO m => FormField m Assignee where
  fromFormField chs def = D.monadic $ liftIO $ do
    employees <- getEmployees
    return . D.choice employees $ def

  renderField _ _ fieldName label view = div_ [class_ "form-group"] $ do
    DL.label fieldName view (toHtml label)
    with (DL.inputSelect fieldName (toHtml <$> view)) -- DL.inputWithType typ_ attrs fieldName view)
      [class_ "form-control", autofocus_]
    DL.errorList fieldName (toHtml <$> view)

data Priority = None | Low | High | Custom { note :: Text, priorityNumber :: Int }
  deriving (Show, Generic)
instance (Monad m) => FormField m Priority

data Todo = TodoItem {
 todoID :: Int,
 todo :: Text,
 assignee :: Assignee,
 priority :: Priority,
 done :: Bool,
 tags :: [TodoTag]
 } deriving (Show, Generic)

instance MonadIO m => FromForm m Todo

getEmployees :: IO [(Assignee, Text)] -- in the IO monad to simulate a database call
getEmployees = return [(1, "Jim"), (2, "Sam"), (3, "Lisa")]


--------------------------------------------------
data Countries = Countries
               | Country Text
               deriving (Show, Generic)

instance MonadIO m => FromRequest m Countries

instance ToURL Countries
--------------------------------------------------
type ExampleM = ReaderT (TVar ExampleState) IO
data ExampleState = ExampleState { todoState :: TodoList, quiz :: Quiz }

-- readTodoState :: ExampleM TodoList
readTodoState = ask >>= fmap todoState . liftIO . readTVarIO

--------------------------------------------------

countryH :: [Gapminder] -> Countries -> HtmlT ExampleM ()
countryH gapM Countries = do
  let countries = nub $ map country gapM
  ul_ $ forM_ countries $ \c -> li_ $ a_ [href_ $ toURL $ Country c] $ (toHtml c)
countryH gapM (Country c) = do
  let entries = filter ((==c) . country) gapM
  ul_ $ forM_ entries $ \e ->
    li_ $ "year: " <> toHtml (show $ year e) <> "  population: "<> toHtml (show $ pop e)


data BubblesDD = BubblesDD { _selYear :: Int} deriving Show
makeLenses ''BubblesDD

bubblesDD gapM = do
  let years = nub $ map year gapM
  selYear <<~ select (map showOpt years)
  h2_ "hello world"
  BubblesDD y <- getValue
  p_ (toHtml $ show $ y)
  clickAttrs <- onClickDo $ \dd -> do
    liftIO $ putStrLn $ "current state: "++show dd
    return Reset
  button_ (clickAttrs) "Print state"

--------------------------------------------------

todoListEditForm :: MonadIO m => Maybe TodoList -> View Text -> HtmlT m ()
todoListEditForm mdef view = container_ $ do
  form_ [method_ "post", action_ (toURL $ UpdateTodoList FormLink)] $ do
    renderSumForm Nothing mdef view
    button_ [type_ "submit"] "Save"

todoH :: TodoR -> HtmlT ExampleM ()

todoH ListTodos = container_ $ do
  TodoList titleT todosT _ <- readTodoState
  employees <- liftIO $ getEmployees
  br_ []
  h4_ (toHtml titleT)
  a_ [type_ "button", class_ "btn btn-primary", href_ . toURL $ EditTodoList]
    "Edit List"
  widget_ . widgetBody_ $ forM_ todosT $ \(TodoItem idT nameT assignT priorityT doneT tags) -> do
    let employee = fromMaybe "unknown" $ lookup assignT employees
    container_ $ do
      div_ $ do
        toHtml $
          show idT <> ". "
          <> (if doneT then "DONE: " else "TODO: ")
          <> unpack nameT
          <> " (" <> unpack employee <> ") "
          <> " [Priority: " <> show priorityT <> "]"
          <> unpack (if length tags == 0 then ""
                     else " (" <> T.intercalate ", " (map tag tags) <> ")")

todoH EditTodoList = do
  tdos <- readTodoState
  todoListEditForm (Just tdos) =<< (getView (Just tdos))

todoH (UpdateTodoList (Form tdos)) = do
  atom <- ask
  liftIO . atomically $
    modifyTVar atom (\st -> st { todoState = tdos })
  todoH ListTodos

todoH (UpdateTodoList (FormError v)) = do
  liftIO . putStrLn $ "UpdateTodoList error: " <> show (FormError v)
  todoListEditForm Nothing v

--------------------------------------------------

readQuiz = fmap quiz . liftIO . readTVarIO =<< ask

quizH ShowQuiz = do
  Quiz nm favCol r <- readQuiz
  br_ []
  h4_ (toHtml nm)
  a_ [type_ "button", class_ "btn btn-primary", href_ $ toURL EditQuiz] "Edit"
  br_ []
  a_ [type_ "button", class_ "btn btn-default", href_ $ toURL NewQuiz] "New"
  container_ $ do
    toHtml $ "Favourite colour: " <> show favCol <> ", " <> show r

quizH NewQuiz = do
  liftIO . putStrLn $ "* New quiz *"
  quizEditForm Nothing =<< getView (Nothing :: Maybe Quiz)

quizH EditQuiz = do
  q <- readQuiz
  liftIO . putStrLn $ "\n ****** Getting view for quiz\n"
  quizEditForm (Just q) =<< getView (Just q)

quizH (UpdateQuiz (Form q)) = do
  liftIO . putStrLn $ "UpdateQuiz:" <> show q
  state <- ask
  liftIO . atomically $
    modifyTVar state $ \st -> st { quiz = q }
  quizH ShowQuiz

quizH (UpdateQuiz (FormError v)) = do
  liftIO . putStrLn $ "UpdateQuiz error: " <> show (FormError v)
  quizEditForm Nothing v

quizEditForm :: Monad m => Maybe Quiz -> View Text -> HtmlT m ()
quizEditForm mdef view = container_ $ do
  form_ [method_ "post", action_ (toURL $ UpdateQuiz FormLink)] $ do
    renderSumForm Nothing mdef view
    button_ [type_ "submit"] "Save"

--------------------------------------------------

initialTodos = TodoList "My todos"
  [ TodoItem 1 "Make todo app" 1 High False [TodoTag "dev", TodoTag "work"]
  , TodoItem 2 "Have lunch" 2 None False [TodoTag "personal"]
  , TodoItem 3 "Buy bread" 3 (Custom "eventually" 1) True []]
  "A field after a subform"

initialQuiz = Quiz "A quiz" (Blue "navy" "extra thoughts") PreferNotToSay

sidebar = rdashSidebar "Youido Example" (return ())
    [ ("Bubbles", "fas")  *~ #bubbles :/ Initial
    , ("Counties", "fas") *~ Countries
    , ("Todos", "fas") *~ ListTodos
    , ("Sum Types", "fas") *~ ShowQuiz ]

inHeader :: Text -> Html ()
inHeader js = do
  script_ [] js

main :: IO ()
main = do
  gapM <- getDataset gapminder
  js <- TIO.readFile "youido.js"
  atom <- newTVarIO $ ExampleState initialTodos initialQuiz
  let runIt :: Bool -> ExampleM a -> IO a
      runIt _ todoM = runReaderT todoM atom

  serveY runIt $ do
    dashdoGlobal
    dashdo #bubbles $ Dashdo (BubblesDD 1980) (bubblesDD gapM)
    port .= 3101
    lookupUser .= \_ _ _ -> return $ Just True
    wrapper .= \_ -> rdashWrapper "Youido Example" (inHeader js) sidebar
    hHtmlT $ countryH gapM
    hHtmlT todoH
    hHtmlT quizH
