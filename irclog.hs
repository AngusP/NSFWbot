-- IRC LOG ANALYSER
-- Takes Weechat Logs and does stats on dem
-- Yeh.

import Data.Char
import Data.List
import System.Random
import System.IO
import System.Environment
import Network
import Network.Socket
import Network.HTTP


type Nick   = String
type Msg    = String
type Time   = String
type Entry  = (Time,Nick,String)
type Jump   = (Nick,Nick)
type Log    = [Entry]
type RawLog = [String]
type Blame  = [(Nick,Int)]

-- Frontent Features
main = do
  theLog <- readLog
  swrJSON <- getJSON
  putStr $ table $ thin $ swearLeague (jsonToSwears swrJSON) theLog

csvLog = do
  theLog <- readLog
  putStr $ toCSV $ procLog theLog






-- Do the thing that be what we do
swearLeague :: [(String,Int)] -> RawLog -> Blame
swearLeague swears log = sortScore $ score swears (procLog log) (shrinkBlame (getNickJumps (procLog log)) (buildBlame (procLog log)))


jsonURL :: String
jsonURL = "http://enginger.me/irc-wordlist/wordlist.json"

localSwears :: [(String,Int)]
localSwears = [("cunt",20),
          ("fuck",10),
          ("nigger",6),
          ("vista",6),
          ("shit",2),
          ("whore",2),
          ("asp.net",2),
          ("vb.net",2),
          ("slut",1),
          ("wank",1),
          (" windows",1),
          ("@slap",1),
          ("bollocks",1),
          ("tits",1),
          ("dick",1),
          ("bastard",1),
          ("cock",1),
          ("arse",1),
          ("emacs",1),
          ("io monad",1)]
-- Swears with a rank in the tuple - C Bomb is worth money!
-- The more there are, the more computation required

{- Some counterexamples:
Snigger, Vistaed, Shittimwood (so rare I'm allowinf shit infix),
Swanky, Windowsill (Unavoidable (soz)), *asp.net (think of a url),
-}


-- Horrible IO Stuff:
readLog :: IO [String]
readLog = do rlog <- readFile "irc.imaginarynet.#compsoc.weechatlog"
             let log = splitByDelim "\n" rlog
             return log



getJSON :: IO String
getJSON = do json <- simpleHTTP (getRequest jsonURL) >>= getResponseBody
             let swears = json
             return swears


jsonToSwears :: String -> [(String,Int)]
jsonToSwears jason = finish ( map (splitByDelim ":") (splitByDelim "," (tail $ init $ init $ init jason)))
  where
    finish [] = []
    finish (stuff:list) = ( (drop 2 $ init $ init stuff!!0) , numerate (stuff!!1) ) : finish list




-- Split a String into a [String] using delimiter 'delim'
splitByDelim :: String -> String -> [String]
splitByDelim delim str = splat delim "" str
  where
    splat :: String -> String -> String -> [String]
    splat [] _ _ = error "Fecking idiot: Delimiter can't be nada"
    splat _ _ [] = []
    splat delim chop xs
      | not ( delim `isInfixOf` xs) = [xs]
      | delim `isPrefixOf` xs       = chop : splat delim "" (tail xs)
      | otherwise                   = splat delim (chop ++ [head xs]) (tail xs)


-- Process an entry into who said what _OR_ who joined when
procLog :: RawLog -> Log
procLog strs = map splitToEntry strs



-- Parse each line of the log
splitToEntry :: String -> Entry
splitToEntry [] = error "Empty String"
splitToEntry str
  | "Day" `isPrefixOf` str         = ( drop 15 str ,"$world","/date-change")
  | "-->" `isPrefixOf` drop 20 str = ( take 19 str , head (splitByDelim " " (drop 24 str)) , "/joined" )
  | "<--" `isPrefixOf` drop 20 str = ( take 19 str , head (splitByDelim " " (drop 24 str)) , "/left" )
  | "--"  `isPrefixOf` drop 20 str = ( take 19 str , "$notice" , drop 23 str )
  | " *"  `isPrefixOf` drop 20 str = ( take 19 str , head (splitByDelim " " (head $ tail $ tail (splitByDelim "\t" (drop 19 str)))) , concat $ tail $ tail $ splitByDelim "\t" (drop 19 str) ) 
  | otherwise                      = ( take 19 str , head $ tail (splitByDelim "\t" (drop 19 str)) , concat $ tail $ tail $ splitByDelim "\t" (drop 19 str) )

-- We use '$' as a special flag in the Nick field because it's invalif for a nick so no-one can ever be $world
-- And we use / as a special flag for content as any client would interpret that as a command and therefore it wouldn't be in the log
-- Ignore date changes
-- The timestamp and tab (YYY-MM-DD HH:MM:SS\t) have length 20
-- Notices begin with -- (For nick changes, topic...)
-- All others should be posts, which are just a tad messy
-- Possible improvement: revert nick changes so stats are complete



-- Kill lurkers!:
lurkKill :: Log -> Log
lurkKill = undefined
-- Maybe not ^^


-- Find Swears:
-- NB: 'swears' is defined at the top of the file
findSwears :: [(String,Int)] -> Entry -> Int
findSwears swears (t,n,str) = find (map toLower str) (map toLower str) swears
  where
    find :: String -> String -> [(String,Int)] -> Int
    find orig [] (swr:swrs) = find orig orig swrs
    find _ _ [] = 0
    find orig (c:str) (swr:swrs)
      | fst swr `isPrefixOf` (c:str) = snd swr + find orig str (swr:swrs)
      | otherwise = find orig str (swr:swrs)

-- orig holds the original string and is there because I'm too lazy to do multiple level recursion



-- Filter the list retaining only instances containing:
filterLog :: [String] -> Log -> Log
filterLog [] _ = []
filterLog (this:them) log = find (map toUpper this) log ++ filterLog them log
  where
    find :: String -> Log -> Log
    find _ [] = []
    find this ((t,n,c):log)
      | this `isInfixOf` (map toUpper c) = (t,n,c) : find this log
      | otherwise                        = find this log



-- Build Blame from Log
buildBlame :: Log -> Blame
buildBlame log = nub (build log)
  where
    build [] = []
    build ((t,n,c):log)
      | not( "$" `isInfixOf` n) = ((map toLower n),0) : build log
      | otherwise = build log



-- Take all things from the log that match the nick
getNickStuff :: Nick -> Log -> Log
getNickStuff nick log = get (map toUpper nick) log
  where
    get _ [] = []
    get nick ((t,n,c):log)
      | nick == (map toUpper n) = (t,n,c) : get nick log
      | otherwise = get nick log



-- Strip down a log to joins and leaves
getInOut :: Log -> Log
getInOut [] = []
getInOut ((t,n,c):log)
  | c == "/joined" = (t,n,c) : getInOut log
  | c == "/left"   = (t,n,c) : getInOut log
  | otherwise      = getInOut log



-- Score nicks on their swearing
score :: [(String,Int)] -> Log -> Blame -> Blame
score _ _ [] = []
score swears log ((nick,val):blame) = ( nick , val + (foldr (+) 0 (map (findSwears swears) (getNickStuff nick log)))) : score swears log blame



-- Apply built in sorting algorithm to Blame
sortScore :: Blame -> Blame
sortScore blame = sortBy compareBlame blame
  where
    compareBlame a b = snd b `compare` snd a


-- Catch one Nick change
-- A jump looks like ("t2","$notice","nick isnowknown as knack")
-- Jump = (Nick,Nick)
jump :: Entry -> Maybe Jump
jump (t,n,c)
  | n == "$notice" && "is now known as" `isInfixOf` c      = Just (head $ words c , last $ words c )
  | otherwise = Nothing


concatMaybe :: [Maybe a] -> [a]
concatMaybe [] = []
concatMaybe (Nothing:ms)  = concatMaybe ms
concatMaybe ((Just a):ms) = a : concatMaybe ms


getNickJumps :: Log -> [Jump]
getNickJumps log = concatMaybe $ map jump log


shrinkBlame :: [Jump] -> Blame -> Blame
shrinkBlame [] blame = nub blame
shrinkBlame (j:js) blame = shrinkBlame js (replace j blame)
  where
    replace :: Jump -> Blame -> Blame
    replace _ [] = []
    replace (old,new) ((nick,i):bs)
      | nick == old = (new,i)  : replace (old,new) bs
      | otherwise   = (nick,i) : replace (old,new) bs



-- Return the longest nick length
longest :: Blame -> Int
longest blame = count blame 0
  where
    count [] longest = longest
    count ((n,i):blame) longest
      | length n > longest = count blame (length n)
      | otherwise          = count blame longest


-- Remove people who didn't score from the blame
thin :: Blame -> Blame
thin [] = []
thin ((n,i):blame)
  | i == 0 = thin blame
  | otherwise = (n,i) : thin blame



-- Make the results nice
table :: Blame -> String
table blame = tabulate blame (longest blame)
  where
    tabulate [] _ = []
    tabulate ((n,i):blame) long = n ++ (take (long - (length n)) (repeat ' ')) ++ "  --  " ++ (show i) ++ "\n" ++ tabulate blame long



-- Convert the Log to JSON:
toCSV :: Log -> String
toCSV [] = []
toCSV ((t,n,c):log) = "\"" ++ t ++ "\",\"" ++ n ++ "\",\"" ++ c ++ "\"\n" ++ toCSV log


-- Convert a number that's a char to a literal Int
strInt :: Char -> Int
strInt a = (ord a) - 48


-- Convert a string inot a literal Int
numerate :: String -> Int
numerate [] = 0
numerate a@(digit:ds) = ((strInt digit) * 10^(length a -1)) + numerate ds