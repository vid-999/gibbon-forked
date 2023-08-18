module GenerateLayout1 where
import Basics

type Text = Vector Char




mkBlogs_layout1 :: Int -> Blog
mkBlogs_layout1 length =
   if length < 0 then End
   else 
      let --select  = (mod length 10)
          --def     = "default file"
          --fc      = fromMaybe def (nth_plist contentfiles Nothing select)
          --vvv :: Vector (Vector Char) 
          --vvv = valloc 0
          --content_words = fileToContent' fc  (singleton (nth fc  0)) vvv 1 (vlength fc)
          --ft      = fromMaybe def (nth_plist tagfiles Nothing select)
          header = (Header (getRandomString 5))
          id     = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   =  (Date (getRandomString 5))
          content = (Content (Plain (mkRandomInlineList 50)))
          tags    = (TagList (mkSomeTags 10))
          rst     = (mkBlogs_layout1 (length - 1))
         in Layout1 header id author date content tags rst



--- Traversal 1 (Filter blogs)
-- Content -> 500 
-- Tags    -> 5 
-- Blogs   -> 1,000,000
-- Input always going to then branch


-- Traversal 2 (EmphContent)
-- Content -> 50
-- Tags    -> 10
-- Blogs   -> 1,000,000
-- Input always going to then branch 


-- Traversal 3 (SearchTag emph Content)
-- Content -> 500 
-- Tags    -> 5 
-- Blogs -> 400,000
-- Input always going to then branch

--Testing First Touch 
-- Content -> 50
-- Tags    -> 50
-- Blogs -> 400,000
