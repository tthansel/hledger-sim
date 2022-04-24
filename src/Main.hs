{-# LANGUAGE DeriveDataTypeable #-}

module Main where

--import System.Environment
import System.Directory
import System.Console.CmdArgs

import Hledger.Read
import Hledger.Data
import Hledger.Query
import Hledger.Utils
-- import Hledger.Reports
import Data.Time (addGregorianMonthsClip, Day, addDays)
import qualified Data.Text as Text
import Hledger.Cli.Commands.Print (print')
import Hledger.Cli.CliOptions (defcliopts)
import Hledger.Cli.Commands.Prices (prices)

--import Data.Random
--import Data.Random.Distribution.Normal
import System.Random
import Data.Decimal

--import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Config

jprint :: Journal -> IO()
jprint j = do 
    print' defcliopts j
    prices defcliopts j

data SimOpts = SimOpts {file :: FilePath}
    deriving (Show, Data, Typeable)

simOpts :: SimOpts
simOpts = SimOpts{file = def &= help "use a different input file."}

simulatePrices :: Map.Map CommoditySymbol FactorCommodity -> [PriceDirective] -> [FactorReturn] -> [PriceDirective]
simulatePrices _ [] _ = []
simulatePrices commodities (price:remaining) freturns = priceList ++ simulatePrices commodities remaining freturns
    where
        priceList = createReturns fcommodity freturns'
        freturns' = dropWhile (\f -> frdate f < pddate price) freturns 
        fcommodity = fcommodity'{fcamount = pdamount price}
        fcommodity' = Map.findWithDefault defcommodity (pdcommodity price) commodities
        defcommodity = FactorCommodity{
            fcsymbol = pdcommodity price,
            fcamount = nullamt,
            fcmarket = 1,
            fcsmb = 0,
            fchml = 0,
            fcrmw = 0,
            fccma = 0,
            fcalpha = 0
            }

data FactorReturn = FactorReturn{
    frdate :: Day,
    frmarket :: Double,
    frsmb :: Double,
    frhml :: Double,
    frrmw :: Double,
    frcma :: Double,
    frrf :: Double
    } deriving (Show, Eq)

data FactorCommodity = FactorCommodity{
    fcsymbol :: CommoditySymbol,
    fcamount :: Amount,
    fcmarket :: Double,
    fcsmb :: Double,
    fchml :: Double,
    fcrmw :: Double,
    fccma :: Double,
    fcalpha :: Double
    } deriving (Show, Eq)

normals :: StdGen -> [Double]
normals g = x : y : normals g'
    where 
        (x, y, g') = boxmuller g


boxmuller :: StdGen -> (Double, Double, StdGen)
boxmuller g = (x, y, g'')
    where
        x = r * sin theta
        y = r * cos theta
        r = sqrt (-2 * (log u1))
        theta = 2 * pi * u2
        (u1, g') = random g
        (u2, g'') = random g'

createFactorReturns :: StdGen -> Day -> Day -> [FactorReturn] 
createFactorReturns g start end = 
    if (start <= end)
    then freturn : createFactorReturns g' start' end
    else []
    where
        start' = addGregorianMonthsClip 1 start
        freturn = FactorReturn{frdate=start, frmarket=mkt, frsmb=smb, frhml=hml, frrmw=rmw, frcma=cma, frrf=rf}
        (g', g'') = split g
        n = take 6 $ normals g''
        mkt = (n !! 0) * 0.0452425 + 0.005887
        smb = (n !! 1) * 0.03035328 + 0.002319857
        hml = (n !! 2) * 0.02902506 + 0.002700714
        rmw = (n !! 3) * 0.02195069 + 0.002590857
        cma = (n !! 4) * 0.01977664 + 0.002604
        rf  = (n !! 5) * 0.002672766 + (0.003672 - 0.00165) -- minus 2% inflation
        -- Data from Fama French through 2021/10


createReturns :: FactorCommodity -> [FactorReturn] -> [PriceDirective]
createReturns _ [] = []
createReturns fcommodity (freturn:freturns) = 
    price : createReturns fcommodity' freturns
    where
        price = PriceDirective{pddate=(frdate freturn),
            pdcommodity = fcsymbol fcommodity,
            pdamount = new_price}
        fcommodity' = fcommodity{fcamount = new_price}
        old_price = aquantity (fcamount fcommodity)
        new_price = (fcamount fcommodity){aquantity = old_price * (realFracToDecimal 8 r)}
        r = (frmarket freturn) * (fcmarket fcommodity) +
            (frsmb freturn) * (fcsmb fcommodity) +
            (frhml freturn) * (fchml fcommodity) +
            (frrmw freturn) * (fcrmw fcommodity) +
            (frcma freturn) * (fccma fcommodity) +
            (frrf freturn) + (fcalpha fcommodity) + 1

mostRecentPriceDirectives :: [PriceDirective] -> Map.Map CommoditySymbol PriceDirective
mostRecentPriceDirectives directives = foldr (\d price_map -> Map.insertWith update (pdcommodity d) d price_map) Map.empty directives
    where update = \new old -> if (pddate new > pddate old) then new else old 

main :: IO()
main = do
    opts <- cmdArgs simOpts
    g <- getStdGen
    let 
        inputopts = definputopts{forecast_ = Just (DateSpan (Just (startDate)) (Just endDate))}
        startDate = Config.startDate
        endDate = Config.endDate
        -- reportspec = reportOptsToSpec (fromGregorian 2021 09 30) defreportopts{period_ = MonthPeriod 2031 9, balanceaccum_ = Historical}
    d <- getCurrentDirectory
    jpath <- if file opts == "" then defaultJournalPath else expandPath d $ file opts
    journal <- readJournalFile inputopts jpath
    -- print $ createReturns g startDate endDate
    case journal of
        Left _ -> return()
        Right j -> do
            jprint j'
            where
                recentPrices  = Map.elems $ mostRecentPriceDirectives $ jpricedirectives j
                (g1, _) = split g
                sim_factors = createFactorReturns g1 startDate endDate
                sim_prices =  simulatePrices commodityRegressions recentPrices sim_factors
                sim_journal = foldr addPriceDirective j sim_prices
                j' = simulateAccount rules startDate endDate sim_journal
                rules = Config.rules ++ [price_rule price_oracle | price_rule <- Config.price_rules]
                price_oracle = journalPriceOracle True sim_journal

simulateAccount :: [Ledger -> Day -> [Transaction]] -> Day -> Day -> Journal -> Journal
simulateAccount rules startDate endDate journal = journal''
    where 
        journal'' = if endDate > startDate
            then simulateAccount rules (addGregorianMonthsClip 1 startDate) endDate journal'
            else ljournal ledger'
        datespan = DateSpan Nothing (Just (addDays 1 startDate))
        ledger = ledgerFromJournal (Date datespan) journal
        (ledger', txns) = applyRules startDate ledger rules
        journal' = foldr addTransaction journal txns

applyRule :: Ledger -> (Ledger -> [Transaction]) -> (Ledger, [Transaction])
applyRule ledger rule = (ledger', txn)
    where
        ledger' = foldr laddTransaction ledger txn
        txn = rule ledger

applyRules :: Day -> Ledger -> [Ledger -> Day -> [Transaction]] -> (Ledger, [Transaction])
applyRules _ ledger [] = (ledger,[])
applyRules date ledger (rule:rules) = (ledger', txns)
    where
        (l, ts) = applyRule ledger (flip rule date)
        (ledger', txns') = applyRules date l rules
        txns = txns' ++ ts

laddTransaction :: Transaction -> Ledger -> Ledger
laddTransaction txn ledger = ledger'
    where
        ledger' = ledger{ljournal = journal', laccounts = accounts'}
        journal' = addTransaction txn (ljournal ledger)
        diff = accountsFromPostings (tpostings txn)
        accounts' = mergeAccounts (laccounts ledger) diff

mergeAccounts :: [Account] -> [Account] -> [Account]
mergeAccounts [] old = old
mergeAccounts (x:xs) old = mergeAccounts xs new
    where
         m_index = List.findIndex (\acct -> (aname x) == (aname acct)) old
         new = case m_index of
            Nothing -> old ++ [x]
            Just i -> start ++ (a':end) 
                where 
                (start,(a:end)) = splitAt i old
                a' =  a{anumpostings = anumpostings a + anumpostings x,
                    aebalance = aebalance a + aebalance x,
                    aibalance = aibalance a + aibalance x}


commodityRegressions :: Map.Map CommoditySymbol FactorCommodity
commodityRegressions = Map.fromList [
    (Text.pack "AVUV", 
    FactorCommodity{
        fcsymbol = Text.pack "AVUV",
        fcamount = nullamt,
        fcmarket = 1.04,
        fcsmb = 0.96,
        fchml = 0.54,
        fcrmw = 0.35,
        fccma = -0.22,
        fcalpha = 0
    }),
    (Text.pack "VTI", 
    FactorCommodity{
        fcsymbol = Text.pack "VTI",
        fcamount = nullamt,
        fcmarket = 1.00,
        fcsmb = 0,
        fchml = 0.02,
        fcrmw = 0.03,
        fccma = 0,
        fcalpha = 0
    })
    ]
