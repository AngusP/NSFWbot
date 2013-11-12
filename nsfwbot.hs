-- 
-- N S F W    B O T
-- 
-- Sits on IRC, Analyses and Logs


import Data.Char
import Data.List
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Text.Printf
import System.Exit
import System.Random
import System.IO
import System.Process
import System.Environment
import Control.Concurrent
import Network
import Network.HTTP

type Nick   = String
type Msg    = String
type Time   = String
type Entry  = (Time,Nick,String)
type Jump   = (Nick,Nick)
type Log    = [Entry]
type RawLog = [String]
type Blame  = [(Nick,Int)]
type Words  = [(String,Int)]

-- This probably won't work onother networks;
-- Try removing initPing from main
server   = "irc.imaginarynet.org.uk"
srvPort  = 6667
nick     = "Sirius"
channel  = "#compsoc"
realname = "Sirius"
pass     = "lambdalove"
ircLog   = "/home/anguspearson/Haskell/ircbot/" ++ server ++ "."++ channel ++".log"
-- Thsi log needs to already exist ^


-- MAIN PROGRAM LOOP: -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main = do
       irc <- connectTo server (PortNumber (fromIntegral srvPort))
       hSetBuffering irc NoBuffering
       write irc "PASS" pass
       write irc "NICK" nick
       write irc "USER" (nick++" 0 * :"++realname)
       initPing irc
       write irc "JOIN" channel
       listen irc



-- IRC FUNCTIONS -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

write :: Handle -> String -> String -> IO ()
write h s t = do
              hPrintf h "%s %s\n" s t
              printf    "> %s %s\n" s t
              delay 100



listen :: Handle -> IO ()
listen irc = forever $ do
  t <- hGetLine irc
  time <- getPOSIXTime
  let s = init t
  putStrLn s
  addItToThePile ircLog (toCSV $ concatMaybe $ (rfcToEntry (show $ timestamp time) s : []))
  if (ping s) then (pong s) else (eval irc (clean s))
    where
      forever a = a >> forever a
      clean = drop 1 . dropWhile (/= ':') . drop 1
      ping x = "PING :"`isPrefixOf` x
      pong x = write irc "PONG" (':' : drop 6 x)
      
      eval :: Handle -> String -> IO ()
      eval irc "$league" = do
                           leg <- getLeague
                           privmsg irc leg
      eval _   _         = return () -- ignore everything else



-- Handle the initial ping before the JOIN command
initPing :: Handle -> IO ()
initPing irc = do
  t <- hGetLine irc
  if ("PING" `isPrefixOf` t) then write irc "PONG" (':' : drop 6 t) else initPing irc


-- Interface to the PRIVMSG command:
privmsg :: Handle -> String -> IO ()
privmsg irc s = write irc "PRIVMSG" (channel ++ " :" ++ (nlReplace s))


-- Really hacky way to push multiple lines
nlReplace :: String -> String
nlReplace [] = []
nlReplace (c:str)
  | c == '\n' = "\r\n" ++ "PRIVMSG " ++ channel ++ " :" ++ nlReplace str
  | otherwise = c : nlReplace str




-- BACKEND FEATURES  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

league = do
  theLog <- readLog
  swrJSON <- getJSON
  putStr $ table $ thin $ swearLeague (jsonToSwears swrJSON) theLog

getLeague :: IO String
getLeague = do
  theLog <- readLog
  swrJSON <- getJSON
  return $ table $ take 5 $ thin $ swearLeague (jsonToSwears swrJSON) theLog

-- Do the thing that be what we do
swearLeague :: Words -> RawLog -> Blame
swearLeague swears log = sortScore $ score swears (procCSVLog log) (shrinkBlame (getNickJumps (procCSVLog log)) (buildBlame (procCSVLog log)))


jsonURL :: String
jsonURL = "http://enginger.me/irc-wordlist/wordlist.json"

-- Swears with a rank in the tuple - C Bomb is worth money!
-- The more there are, the more computation required

{- Some counterexamples:
Snigger, Vistaed, Shittimwood (so rare I'm allowing shit infix),
Swanky, Windowsill (Unavoidable (soz)), *asp.net (think of a url),
-}



-- IO ()  STUFF -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

readLog :: IO [String]
readLog = do rlog <- readFile ircLog
             let log = splitByDelim "\n" rlog
             return log


getJSON :: IO String
getJSON = do json <- simpleHTTP (getRequest jsonURL) >>= getResponseBody
             let swears = json
             return swears


jsonToSwears :: String -> Words
jsonToSwears jason = finish ( map (splitByDelim ":") (splitByDelim "," (tail $ init $ init $ init jason)))
  where
    finish [] = []
    finish (stuff:list) = ( (drop 2 $ init $ init stuff!!0) , numerate (stuff!!1) ) : finish list


--addItToThePile :: FilePath -> String -> IO ()
addItToThePile location str = appendFile location str

timestamp :: POSIXTime -> UTCTime
timestamp theTime = posixSecondsToUTCTime theTime

delay :: Int -> IO ()
delay time = do threadDelay (time * 1000)


-- ANALYTICAL FUNCTIONS -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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


-- Process a line broken Raw Log in the basic RFC pattern
-- Time is 0 as RFC logs don't contain timestamps
procRFCLog :: RawLog -> Log
procRFCLog log = concatMaybe $ map (rfcToEntry "1970-01-01 00:00:00.000000 UTC") log



-- Process a Log in CSV triples: (Time,Nick,Message)
procCSVLog :: RawLog -> Log
procCSVLog log = concatMaybe $ map csvToEntry log



-- Parse RFC Standard IRC
rfcToEntry :: String -> String -> Maybe Entry
rfcToEntry t str
  | " PRIVMSG" `isPrefixOf` (dropWhile (/=' ') str) && ":ACTION" `isPrefixOf` (dropWhile (/=':') $ dropWhile (/=' ') str)
                                                    = Just ( t , takeWhile (/='!') $ drop 1 str , 
                                                      (takeWhile (/='!') $ drop 1 str) ++ (dropWhile (/=' ') $ tail $ dropWhile (/=':') $ tail str) )
  | " PRIVMSG" `isPrefixOf` (dropWhile (/=' ') str) = Just ( t , takeWhile (/='!') $ drop 1 str , tail $ dropWhile (/=':') $ tail str )
  | " PART" `isPrefixOf`    (dropWhile (/=' ') str) = Just ( t , takeWhile (/='!') $ drop 1 str , "$left" )
  | " QUIT" `isPrefixOf`    (dropWhile (/=' ') str) = Just ( t , takeWhile (/='!') $ drop 1 str , "$left" )
  | " JOIN" `isPrefixOf`    (dropWhile (/=' ') str) = Just ( t , takeWhile (/='!') $ drop 1 str , "$joined" )
  | " KICK" `isPrefixOf`    (dropWhile (/=' ') str) = Just ( t , "$kick" , 
                            (takeWhile (/='!') $ drop 1 str) ++ " kicked " ++ ((splitByDelim " " $ tail str)!!3) )
  | " NICK" `isPrefixOf`    (dropWhile (/=' ') str) = Just ( t , takeWhile (/='!') $ drop 1 str , 
                                                           head $ words $ tail $ dropWhile (/=':') $ tail str )
  | otherwise = Nothing



csvToEntry :: String -> Maybe Entry
csvToEntry str 
  | length (splitByDelim "," str) >= 3 = Just ( init $ tail $ head $ splitByDelim "," str,
                                                init $ tail $ head $ tail $ splitByDelim "," str,
                                                init $ tail $ head $ tail $ tail $ splitByDelim "," str )
  | otherwise = Nothing


-- Kill lurkers!:
lurkKill :: Log -> Log
lurkKill = undefined
-- Maybe not ^^


-- Find Swears:
-- NB: 'swears' is defined at the top of the file
findSwears :: Words -> Entry -> Int
findSwears swears (t,n,str) = find (map toLower str) (map toLower str) swears
  where
    find :: String -> String -> Words -> Int
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
  | c == "$joined" = (t,n,c) : getInOut log
  | c == "$left"   = (t,n,c) : getInOut log
  | otherwise      = getInOut log



-- Score nicks on their swearing
score :: Words -> Log -> Blame -> Blame
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

showEntry :: Entry -> String
showEntry (t,n,c) = "\"" ++ t ++ "\", \"" ++ n ++ "\", \"" ++ c ++ "\"\n"

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
