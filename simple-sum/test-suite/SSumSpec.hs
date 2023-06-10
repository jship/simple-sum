{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
module SSumSpec
  ( spec
  ) where

import Data.Data (Proxy(Proxy))
import Data.Foldable (Foldable(fold), asum)
import Data.Functor.Const (Const(Const))
import Data.Void (absurd)
import Prelude
import SSum ((/\), SSum, build, ematch, match)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified SSum.Internal as Internal

spec :: Spec
spec = do
  describe "build" do
    it "one type" do
      Internal.unSSum (build @Int 42 :: SSum '[Int]) `shouldBe` Left 42
    it "two types" do
      Internal.unSSum (build @Int 42 :: SSum '[Int, String])
        `shouldBe` Left 42
      Internal.unSSum (build @String "abc" :: SSum '[Int, String])
        `shouldBe` Right (Left "abc")
    it "three types" do
      Internal.unSSum (build @Int 42 :: SSum '[Int, String, Char])
        `shouldBe` Left 42
      Internal.unSSum (build @String "abc" :: SSum '[Int, String, Char])
        `shouldBe` Right (Left "abc")
      Internal.unSSum (build @Char 'a' :: SSum '[Int, String, Char])
        `shouldBe` Right (Right $ Left 'a')

  describe "match" do
    it "one type" do
      match (build @Int 42 :: SSum '[Int]) `shouldBe` Just @Int 42
    it "two types" do
      match (build @Int 42 :: SSum '[Int, String]) `shouldBe` Just @Int 42
      match (build @String "abc" :: SSum '[Int, String]) `shouldBe` Just "abc"
    it "three types" do
      match (build @Int 42 :: SSum '[Int, String, Char])
        `shouldBe` Just @Int 42
      match (build @String "abc" :: SSum '[Int, String, Char])
        `shouldBe` Just "abc"
      match (build @Char 'a' :: SSum '[Int, String, Char]) `shouldBe` Just 'a'
    it "case approximation" do
      let matcher :: SSum '[Int, String, Char] -> String
          matcher ssum =
            fold $ asum
              [ match @Int ssum >>= \x ->
                  Just $ show x
              , match @String ssum >>= \s ->
                  Just s
              , match @Char ssum >>= \c ->
                  Just [c]
              ]
      matcher (build @Int 42) `shouldBe` "42"
      matcher (build @String "abc") `shouldBe` "abc"
      matcher (build @Char 'a') `shouldBe` "a"

  describe "ematch" do
    it "case approximation" do
      let matcher :: SSum '[Int, String, Char] -> String
          matcher ssum =
            ematch $ ssum
              /\ do
                   \x -> show x
              /\ do
                   \s -> s
              /\ do
                   \c -> [c]
              /\ absurd
      matcher (build @Int 42) `shouldBe` "42"
      matcher (build @String "abc") `shouldBe` "abc"
      matcher (build @Char 'a') `shouldBe` "a"

  describe "nullary labeled branches" do
    it "demo" do
      let matcher :: SSum '[Proxy "Foo", Proxy "Bar", Proxy "Baz"] -> String
          matcher ssum =
            fold $ asum
              [ match @(Proxy "Foo") ssum >>= \Proxy ->
                  Just "Foo"
              , match @(Proxy "Bar") ssum >>= \Proxy ->
                  Just "Bar"
              , match @(Proxy "Baz") ssum >>= \Proxy ->
                  Just "Baz"
              ]
      matcher (build @(Proxy "Foo") Proxy) `shouldBe` "Foo"
      matcher (build @(Proxy "Bar") Proxy) `shouldBe` "Bar"
      matcher (build @(Proxy "Baz") Proxy) `shouldBe` "Baz"

  describe "nullary and unary labeled branches" do
    it "demo" do
      let matcher
            :: SSum '[Proxy "Foo", Const String "Bar", Const Char "Baz"]
            -> (String, Maybe String)
          matcher ssum =
            fold $ asum
              [ match @(Proxy "Foo") ssum >>= \Proxy ->
                  Just ("Foo", Nothing)
              , match @(Const String "Bar") ssum >>= \(Const s) ->
                  Just ("Bar", Just s)
              , match @(Const Char "Baz") ssum >>= \(Const c) ->
                  Just ("Baz", Just [c])
              ]
      matcher (build @(Proxy "Foo") Proxy) `shouldBe` ("Foo", Nothing)
      matcher (build @(Const String "Bar") $ Const "abc")
        `shouldBe` ("Bar", Just "abc")
      matcher (build @(Const Char "Baz") $ Const 'a')
        `shouldBe` ("Baz", Just "a")
