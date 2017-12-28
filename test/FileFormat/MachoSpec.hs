module FileFormat.MachoSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B

import FileFormat.Macho

spec :: Spec
spec = do
  describe "executableFromText" $ do
    it "creates an executable Mach-O binary from text" $ do
      want <- B.readFile "test/FileFormat/data/binary"
      (executableFromText $ B.singleton 0xc3) `shouldBe` want

    it "emits a binary whose size is more than or equal to 4096 bytes" $ do
      B.length (executableFromText $ B.singleton 0xc3) `shouldBe` 4096
