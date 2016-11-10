module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import FluxChallenge exposing (..)
import DarkJedi exposing (..)

someJedi = 
  { id= 1
  , name="jasnira"
  , homeworld = {id=1,name="world"}
  , master = {id=Just 2,url=Just "mextri"}
  , apprentice = {id=Just 3, url=Just "prendiz"}
  }

all : Test
all =
  describe "A Test Suite"
    [ test "adds apprentice to empty list" <|
      \() ->
        updateList FluxChallenge.Apprentice someJedi [Empty, Empty, Empty, Empty, Empty] 
        |> Expect.equal [Far someJedi, Empty, Empty, Empty, Empty] 
    ,  test "adds apprentice to non-empty list" <|
      \() ->
        updateList FluxChallenge.Apprentice someJedi [Far someJedi, Empty, Empty, Empty, Empty] 
        |> Expect.equal [Far someJedi, Far someJedi, Empty, Empty, Empty] 
    , test "adds master to empty list" <|
      \() ->
        updateList FluxChallenge.Master someJedi [Empty, Empty, Empty, Empty, Empty] 
        |> Expect.equal [Empty, Empty, Empty, Empty, Far someJedi] 
    ,  test "adds master to non-empty list" <|
      \() ->
        updateList FluxChallenge.Master someJedi [Empty, Empty, Empty, Empty, Far someJedi] 
        |> Expect.equal [Empty, Empty, Empty, Far someJedi, Far someJedi] 
 
   ]
