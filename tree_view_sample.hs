module TreeViewSample1 where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.ModelView as MV

import qualified System.Directory as Dir
import qualified System.IO as IO

import qualified Control.Monad as Monad

import Control.Monad.Trans (liftIO)


main :: IO ()
main = do
  initGUI
  
  window1 <- windowNewWith "Main window" =<< sampleTreeView1
  window1 `on` deleteEvent $ liftIO mainQuit >> return False

  window2 <- windowNewWith "View files in dir" =<< sampleTreeView2

  mainGUI
  where windowNewWith title widget = do
          window <- windowNew
          window `set` [ containerChild := widget
                       , windowTitle := title ]
          widgetShowAll window
          return window

sampleTreeView1 :: IO MV.TreeView
sampleTreeView1 = do
  store    <- MV.listStoreNew [ ("Cocoa", 15), ("Cino",  13), ("Rizz",  16) ]
  treeview <- MV.treeViewNewWithModel store
  treeview `set` [ MV.treeViewHeadersVisible := True ]

  col1 <- mkColumn store "Name" (\(name, _) -> name)
  MV.treeViewAppendColumn treeview col1

  col2 <- mkColumn store "Age" (\(_, age) -> show age)
  MV.treeViewAppendColumn treeview col2

  return treeview
  where mkColumn store title accessor = do
          col <- MV.treeViewColumnNew
          col `set` [ MV.treeViewColumnTitle := title ]
  
          renderer <- MV.cellRendererTextNew
          MV.cellLayoutPackStart col renderer False
          MV.cellLayoutSetAttributes col renderer store $ \val ->
            [ MV.cellText := accessor val ]
          return col

type FileInfo = (IO.FilePath, Integer)

sampleTreeView2 :: IO MV.TreeView
sampleTreeView2 = do
  store     <- MV.listStoreNew ([] :: [FileInfo])
  MV.customStoreSetColumn store (MV.makeColumnIdString 0) fst
  MV.customStoreSetColumn store (MV.makeColumnIdString 1) (show.snd)

  storeSorted <- MV.treeModelSortNewWithModel store
  MV.treeSortableSetSortFunc storeSorted 0 (compareRow store fst)
  MV.treeSortableSetSortFunc storeSorted 1 (compareRow store snd)

  populateStore store

  treeview  <- MV.treeViewNewWithModel storeSorted
  MV.treeViewAppendColumn treeview =<< setRendererAndColumn "Filename" 0
  MV.treeViewAppendColumn treeview =<< setRendererAndColumn "Size in bytes" 1

  return treeview
  where setRendererAndColumn columnTitle columnId = do
          renderer <- MV.cellRendererTextNew
          column <- MV.treeViewColumnNew
          column `set` [ MV.treeViewColumnTitle := columnTitle
                       , MV.treeViewColumnSortColumnId := columnId ]

          MV.cellLayoutPackStart column renderer False
          MV.cellLayoutAddColumnAttribute column renderer MV.cellText $ MV.makeColumnIdString columnId
          
          return column
        populateStore :: MV.ListStore FileInfo -> IO ()
        populateStore store = do
          files <- Monad.filterM Dir.doesFileExist =<< Dir.getDirectoryContents "."
          Monad.forM_ files $ \filename -> do
            size <- IO.withFile filename IO.ReadMode IO.hFileSize 
            store `MV.listStoreAppend` (filename, size)
        compareRow :: Ord b => MV.ListStore a -> (a -> b) -> TreeIter -> TreeIter -> IO Ordering
        compareRow store acc iter1 iter2 = do
          v1 <- MV.treeModelGetRow store iter1
          v2 <- MV.treeModelGetRow store iter2
          return $ (acc v1) `compare` (acc v2)
