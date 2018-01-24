module Test.Aws.Dynamo.Query where

import Prelude

import Aws.Dynamo (DYNAMO)
import Aws.Dynamo.Query as DQ
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Array as Array
import Data.Foldable (and)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, encode)
import Test.QuickCheck (Result, quickCheck, (===))
import Unsafe.Coerce (unsafeCoerce)


type Aff_ eff a = Aff
                  ( console :: CONSOLE
                  , random :: RANDOM
                  , exception :: EXCEPTION
                  , dynamo :: DYNAMO
                  | eff) a

test :: forall eff. Aff_ eff Unit
test = do
  testParams
  testExpression


newtype TestTableKey = TestTableKey Int
derive instance eqTableKey :: Eq TestTableKey
derive newtype instance encodeTableKey :: Encode TestTableKey
derive newtype instance decodeTableKey :: Decode TestTableKey
instance tablekeyTableKey :: DQ.TableKey TestTableKey

encode' :: DQ.Builder TestTableKey Unit -> Foreign
encode' = encode

testParams :: forall eff. Aff_ eff Unit
testParams = liftEff $ do
  quickCheck checkTableName
  quickCheck checkIndexName
  quickCheck checkAscending
  quickCheck checkDescending
  quickCheck checkLimit
  quickCheck checkKeyCondition
  quickCheck checkFilter

checkTableName :: String -> Result
checkTableName name = params."TableName" === name
  where
    params = unsafeCoerce $ encode' do
      DQ.tableName name

checkIndexName :: String -> Result
checkIndexName name = params."IndexName" === name
  where
    params = unsafeCoerce $ encode' do
      DQ.indexName name

checkAscending :: Unit -> Result
checkAscending _ = params."ScanIndexForward" === true
  where
    params = unsafeCoerce $ encode' do
      DQ.ascending

checkDescending :: Unit -> Result
checkDescending _ = params."ScanIndexForward" === false
  where
    params = unsafeCoerce $ encode' do
      DQ.descending

checkLimit :: Int -> Result
checkLimit i = params."Limit" === i
  where
    params = unsafeCoerce $ encode' do
      DQ.limit i

checkExclusiveStartKey :: Int -> Result
checkExclusiveStartKey i = params."ExclusiveStartKey" === i
  where
    params = unsafeCoerce $ encode' do
      DQ.exclusiveStartKey $ TestTableKey i

checkKeyCondition :: String -> Number -> Boolean
checkKeyCondition s n =
  and
  [ params."ExpressionAttributeNames"."#name" == "name"
  , params."ExpressionAttributeNames"."#age" == "age"
  , params."ExpressionAttributeValues".":1" == s
  , params."ExpressionAttributeValues".":2" == n
  , params."KeyConditionExpression" == "(#name = :1) AND (#age < :2)"
  ]
  where
    params = unsafeCoerce $ encode' do
      DQ.keyCondition $
        DQ.and_
        [ DQ.eq_ "#name" s
        , DQ.lt_ "#age" n
        ]

checkFilter :: String -> Number -> Boolean
checkFilter s n =
  and
  [ params."ExpressionAttributeNames"."#name" == "name"
  , params."ExpressionAttributeNames"."#age" == "age"
  , params."ExpressionAttributeValues".":1" == s
  , params."ExpressionAttributeValues".":2" == n
  , params."FilterExpression" == "(#name = :1) AND (#age < :2)"
  ]
  where
    params = unsafeCoerce $ encode' do
      DQ.filter $
        DQ.and_
        [ DQ.eq_ "#name" s
        , DQ.lt_ "#age" n
        ]

testExpression :: forall eff. Aff_ eff Unit
testExpression = liftEff do
  quickCheck \s -> check "#name = :1" (DQ.eq_ "#name" (s :: String))
  quickCheck \s -> check "#name <> :1" (DQ.ne_ "#name" (s :: String))
  quickCheck \i -> check "#age < :1" (DQ.lt_ "#age" (i :: Int))
  quickCheck \i -> check "#age <= :1" (DQ.le_ "#age" (i :: Int))
  quickCheck \i -> check "#age > :1" (DQ.gt_ "#age" (i :: Int))
  quickCheck \i -> check "#age >= :1" (DQ.ge_ "#age" (i :: Int))
  quickCheck \i j -> check "#age BETWEEN :1 AND :2" (DQ.between_ "#age" (i :: Int) (j :: Int))
  quickCheck \is ->
    check ("#age IN ("
           <> (Array.intercalate "," $
               (":" <> _) <<< show <<< (_ + 1) <$> Array.mapWithIndex const is
              )
           <> ")") $
    DQ.in_ "#age" (is :: Array Int)

  quickCheck \x y ->
    check "(#name = :1) AND (#age < :2)" $
    DQ.and_
    [ DQ.eq_ "#name" (x :: String)
    , DQ.lt_ "#age" (y :: Int)
    ]

  quickCheck \x y ->
    check "(#name = :1) OR (#age < :2)" $
    DQ.or_
    [ DQ.eq_ "#name" (x :: String)
    , DQ.lt_ "#age" (y :: Int)
    ]

  quickCheck \x ->
    check "NOT #name = :1" $
    DQ.not_ $
    DQ.eq_ "#name" (x :: String)

  where
    check str exp = str === (_."FilterExpression" <<< unsafeCoerce <<< encode' $ DQ.filter exp)
