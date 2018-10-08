--data Fiction = Fiction deriving Show
--data Nonfiction = Nonfiction deriving Show
data BookType = FictionBook | NonfictionBook deriving Show

type AuthorName = String
--data Author = Author AuthorName BookType deriving Show
data Author = Fiction AuthorName | Nonfiction AuthorName deriving Show
