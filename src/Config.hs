module Config where
import Hledger.Data
import Data.Time (fromGregorian)
import qualified Data.Text as Text
import Rules

startDate = fromGregorian 2022 05 01
endDate = fromGregorian 2062 12 01

rules = [
    excessBalanceRule "assets:Savings" "assets:Checking" (mixed [usd 10000]),
    deferredInterestRule (fromGregorian 2022 09 01) "expenses:Student Loan Interest" "liabilities:Student Loans" 0.06,
    interestRule "income:Interest" "assets:Savings" 0.01,
    (debtPayoffRule "assets:Checking"
        [
            "liabilities:Student Loans"
        ]
        (mixed [usd 5000])),
    excessBalanceRule "assets:Checking" "assets:IRA" (mixed [usd 7500])
    ]

price_rules = [
    buyWithCashRule "assets:401k" "equity:trades:VTI" (Text.pack "VTI"),
    buyWithCashRule "assets:IRA" "equity:trades:VTI" (Text.pack "VTI")
    ]
