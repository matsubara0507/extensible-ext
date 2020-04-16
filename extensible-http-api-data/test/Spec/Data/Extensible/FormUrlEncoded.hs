module Spec.Data.Extensible.FormUrlEncoded
    ( spec
    ) where

import           Data.Extensible
import           Data.Extensible.FormUrlEncoded ()
import           Data.Int                       (Int64)
import           Data.Text                      (Text)
import           Test.Tasty
import           Test.Tasty.Hspec
import           Web.FormUrlEncoded             (urlDecodeAsForm)

spec :: Spec
spec =
  describe "urlDecodeAsForm" $ do
    let expect
            = #int @= 1
           <: #float @= 0.5
           <: #bool @= True
           <: #char @= 'a'
           <: #int64 @= 2
           <: #textX @= Just "abc"
           <: #textY @= Nothing
           <: nil :: TestCaseType
        form = "int=1&float=0.5&bool=true&char=a&int64=2&textX=abc"
    it "return decoded form" $
      urlDecodeAsForm form `shouldBe` Right expect


type TestCaseType = Record
  '[ "int"   >: Int
   , "float" >: Float
   , "bool"  >: Bool
   , "char"  >: Char
   , "int64" >: Int64
   , "textX" >: Maybe Text
   , "textY" >: Maybe Text
   ]
