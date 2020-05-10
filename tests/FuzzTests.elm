module FuzzTests exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Expect
import Fuzz exposing (Fuzzer)
import Point2d exposing (Point2d)
import QuadTree exposing (Bounded, QuadTree)
import Quantity exposing (Quantity, Unitless)
import Test exposing (Test, describe, fuzz)


type alias Limits =
    { minX : Float, maxX : Float, minY : Float, maxY : Float }


type YUpCoordinates
    = YUpCoordinates


fuzzQuantityRange : Quantity Float units -> Quantity Float units -> Fuzzer (Quantity Float units)
fuzzQuantityRange from to =
    Fuzz.floatRange 0.0 0.1
        |> Fuzz.map (Quantity.interpolateFrom from to)


treeRegion : BoundingBox2d Unitless YUpCoordinates
treeRegion =
    BoundingBox2d.fromExtrema
        { minX = Quantity.float -10.0
        , maxX = Quantity.float 10.0
        , minY = Quantity.float -10.0
        , maxY = Quantity.float 10.0
        }


emptyTree : QuadTree Unitless YUpCoordinates (Bounded Unitless YUpCoordinates a)
emptyTree =
    QuadTree.init treeRegion 16


fuzzPoint2d : Fuzzer (Point2d Unitless YUpCoordinates)
fuzzPoint2d =
    fuzzPoint2dInRegion treeRegion


fuzzPoint2dInRegion : BoundingBox2d Unitless YUpCoordinates -> Fuzzer (Point2d Unitless YUpCoordinates)
fuzzPoint2dInRegion region =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema region
    in
    Fuzz.map2
        Point2d.xy
        (fuzzQuantityRange minX maxX)
        (fuzzQuantityRange minY maxY)


fuzzBoundingBox : Fuzzer (BoundingBox2d Unitless YUpCoordinates)
fuzzBoundingBox =
    Fuzz.map2
        BoundingBox2d.from
        fuzzPoint2d
        fuzzPoint2d


fuzzBoundedPointItem =
    Fuzz.map (\point -> { boundingBox = BoundingBox2d.singleton point }) fuzzPoint2d


fuzzBoundedBoxItem =
    Fuzz.map (\box -> { boundingBox = box }) fuzzBoundingBox



---- Tests


insertListTest : Test
insertListTest =
    describe "Should not produce stack overflow on insertion"
        [ fuzz (Fuzz.list fuzzBoundedPointItem) "Inserting points" <|
            \list ->
                let
                    _ =
                        QuadTree.insertList list emptyTree
                in
                Expect.pass
        , fuzz (Fuzz.list fuzzBoundedPointItem) "Can insert and retrieve variables number of points" <|
            \list ->
                let
                    actual =
                        QuadTree.length (QuadTree.insertList list emptyTree)

                    expected =
                        List.length list
                in
                Expect.equal actual expected
        ]
