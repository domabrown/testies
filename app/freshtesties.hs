myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs) 

data BookInfo = Book Int String [String]
  deriving (Show)

data Magazineinfo = Magazine Int String (String)
  deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)