module EventSpec(spec) where

import Test.Hspec

spec :: Spec
spec = do

    describe "Receiving update events" $ do

        it "Combines update messages from XMonad and the current system time" $ do
            let xmonadInfo = (return $ "1 [2] 3 : Tall : urxvt") :: IO String
            let systemTime = undefined
            True `shouldBe` True
