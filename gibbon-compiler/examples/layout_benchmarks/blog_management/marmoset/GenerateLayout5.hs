module GenerateLayout5 where
import Basics

type Text = Vector Char


mkBlogs_layout5 :: Int -> Blog
mkBlogs_layout5 length =
   if length < 0 then End
   else 
      let --select  = (mod length 10)
          --def     = "default file"
          --fc      = fromMaybe def (nth_plist contentfiles Nothing select)
          --vvv :: Vector (Vector Char) 
          --vvv = valloc 0
          --content_words = fileToContent' fc  (singleton (nth fc  0)) vvv 1 (vlength fc)
          --ft      = fromMaybe def (nth_plist tagfiles Nothing select)  
          rst = (mkBlogs_layout5 (length - 1))
          tags = (TagList (mkSomeTags 10))
          content = (Content (Plain (mkRandomInlineList 50)))
          header  =  (Header (getRandomString 5))
          id      = (ID (10 - (mod length 10)))
          author = (Author (getRandomString 5))
          date   = (Date (getRandomString 5))
         in Layout5 rst tags content header id author date
