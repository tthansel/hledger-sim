module Rules where

import qualified Data.Text as Text
import Hledger.Data
import Data.Decimal
import Data.Time (Day)

testrule :: Ledger -> Day -> [Transaction]
testrule ledger date = case m1 of
            Nothing -> []
            Just account -> if balance > mixed [usd 30000] 
                then [transaction date [post (Text.pack "assets:Checking") (-excess), post (Text.pack "assets:IRA") (excess)]]
                else []
                where
                    balance = aibalance account
                    excess' = unifyMixedAmount (balance - mixed [(usd 30000)])
                    excess = case excess' of
                        Nothing -> usd 0
                        Just x -> x
            where m1 = ledgerAccount ledger (Text.pack "assets:Checking")

closeAccountRule :: String -> String -> Day -> Ledger -> Day -> [Transaction]
closeAccountRule from to close_date ledger date = case from_acct of
    Nothing -> []
    Just account -> if date < close_date then []
        else marked_txns
        where 
            close_balance = aibalance account
            txns = [transaction date [post (Text.pack from) (-bal), post (Text.pack to) (bal)] | bal <- amounts close_balance]
            marked_txns = [txn {ttags = [tag], tcomment = commentAddTagNextLine (tcomment txn) tag} | txn <- txns]
            tag = (Text.pack "Sim: Close Account", Text.pack (show from ++ " -> " ++ show to))
    where from_acct = ledgerAccount ledger (Text.pack from)

interestRule :: String -> String -> Quantity -> Ledger -> Day -> [Transaction]
interestRule from to rate ledger date = case to_acct of
    Nothing -> []
    Just account -> marked_txns
        where
            txns = [transaction date [post (Text.pack from) (-i), post (Text.pack to) (i)] | i <- amounts interest]
            marked_txns = [txn {ttags = [tag], tcomment = commentAddTagNextLine (tcomment txn) tag} | txn <- txns]
            tag = (Text.pack "Sim: Monthly Interest", Text.pack ( show (aibalance account) ++ show rate ++ " -> " ++ show to))
            interest = multiplyMixedAmount (rate*30/365) (aibalance account)

            -- Very rough approximation, should take into account actual days, average balance.
    
    where to_acct = ledgerAccount ledger (Text.pack to)

deferredInterestRule :: Day -> String -> String -> Quantity -> Ledger -> Day -> [Transaction]
deferredInterestRule active_date from to rate ledger date = if active_date > date then []
    else interestRule from to rate ledger date

debtPayoffRule :: String -> [String] -> MixedAmount -> Ledger -> Day -> [Transaction]
debtPayoffRule _ [] _ _ _ = []
debtPayoffRule from (to:rest) max_bal ledger date = case from_acct of
    Nothing -> []
    Just account -> if balance > max_bal
        then if not (amountIsZero payment)
            then [marked_txn] ++ debtPayoffRule from rest (max_bal + (mixedAmount payment)) ledger date
            else debtPayoffRule from rest (max_bal + (mixedAmount payment)) ledger date
        else []
        where 
            txn = transaction date [post (Text.pack from) (-payment), post (Text.pack to) (payment)]
            marked_txn = txn {ttags = [tag], tcomment = commentAddTagNextLine (tcomment txn) tag}
            tag = (Text.pack "Sim: Debt Payoff", Text.pack (show(from) ++ " -> " ++ show(to)))
            balance = aibalance account
            excess' = unifyMixedAmount (balance - max_bal)
            excess = case excess' of
                Nothing -> usd 0
                Just x -> x
            to_acct = ledgerAccount ledger (Text.pack to)
            debt_balance = case to_acct of
                Nothing -> usd 0
                Just x -> case unified_balance of
                    Nothing -> usd 0
                    Just y -> y
                    where unified_balance = unifyMixedAmount $ aibalance x
            payment = minimum [maximum [usd 0, -debt_balance], excess]

    where from_acct = ledgerAccount ledger (Text.pack from)
    

excessBalanceRule :: String -> String -> MixedAmount -> Ledger -> Day -> [Transaction]
excessBalanceRule from to max_bal ledger date = case from_acct of
            Nothing -> []
            Just account -> if balance > max_bal
                then [marked_txn]
                else []
                where 
                    txn = transaction date [post (Text.pack from) (-excess), post (Text.pack to) (excess)]
                    marked_txn = txn {ttags = [tag], tcomment = commentAddTagNextLine (tcomment txn) tag}
                    tag = (Text.pack "Sim: Excess Balance", Text.pack (show(from) ++ " -> " ++ show(to)))
                    balance = aibalance account
                    excess' = unifyMixedAmount (balance - max_bal)
                    excess = case excess' of
                        Nothing -> usd 0
                        Just x -> x
            where from_acct = ledgerAccount ledger (Text.pack from)

buyWithCashRule :: String -> String -> CommoditySymbol -> PriceOracle -> Ledger -> Day -> [Transaction]
buyWithCashRule acct_name equity_name commodity price_oracle ledger date = case macct of
    Nothing -> []
    Just acct -> if cash > 0
        then [marked_txn]
        else []
        where
            balance = aibalance acct
            cash_balance = (amounts $ filterMixedAmountByCommodity (Text.pack "$") balance) !! 0
            cash = aquantity cash_balance
            marked_txn = buy_txn {ttags = [tag], tcomment = commentAddTagNextLine (tcomment buy_txn) tag}
            tag = (Text.pack "Sim: Buy with Cash", Text.pack (show (commodity)))
            buy_txn = transaction date postings
            postings = [
                nullposting{
                    pdate = Just date,
                    paccount = Text.pack acct_name,
                    pamount = mixedAmount (-cash_balance)
                    },
                nullposting{
                    pdate = Just date,
                    paccount = Text.pack acct_name,
                    pamount = mixedAmount comm_amount
                    },
                nullposting{
                    pdate = Just date,
                    paccount = Text.pack equity_name,
                    pamount = mixedAmount cash_balance
                    },
                nullposting{
                    pdate = Just date,
                    paccount = Text.pack equity_name,
                    pamount = mixedAmount (-comm_amount)
                    }]
            comm_price = price_oracle (date, commodity, Just (Text.pack "$"))
            comm_amount = case comm_price of
                Nothing -> nullamt
                Just (_,comm_price') -> amount{acommodity = commodity, 
                    aquantity = realFracToDecimal 8 (cash / comm_price'),
                    astyle = AmountStyle{ascommodityside = R,
                        ascommodityspaced = True,
                        asprecision = Precision 6,
                        asdecimalpoint = Nothing,
                        asdigitgroups = Nothing
                        }}

    where macct = ledgerAccount ledger (Text.pack acct_name)
