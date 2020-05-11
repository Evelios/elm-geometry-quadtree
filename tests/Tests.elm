module Tests exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Expect
import Point2d
import QuadTree
    exposing
        ( Bounded
        , QuadTree
        )
import Quantity exposing (Unitless)
import Test exposing (Test, describe, test)



---- Tests


quadTreeInsertTest : Test
quadTreeInsertTest =
    describe "Insert to Quad Tree"
        [ test "Valid after insert to empty" <|
            \_ ->
                let
                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }
                in
                QuadTree.insert bounded emptyTree
                    |> QuadTree.isValid
                    |> Expect.ok
        , test "Insert single element length is 1" <|
            \_ ->
                let
                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }
                in
                Expect.equal (QuadTree.length (QuadTree.insert bounded emptyTree)) 1
        , test "Inserted element to list returns the inserted item" <|
            \_ ->
                let
                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }

                    treeWithElements =
                        QuadTree.insert bounded emptyTree
                in
                Expect.equalLists (QuadTree.toList treeWithElements) [ bounded ]
        , test "Multi-level tree should be valid" <|
            \_ ->
                let
                    boundeds =
                        [ { boundingBox = boundingBox 0 1 0 1 }
                        , { boundingBox = boundingBox -1 0 0 1 }
                        , { boundingBox = boundingBox 0 1 -1 0 }
                        , { boundingBox = boundingBox -1 0 -1 0 }
                        , { boundingBox = boundingBox 1 2 1 2 }
                        ]
                            |> List.sortWith sortXthenY

                    testTree =
                        List.foldl QuadTree.insert emptyTree boundeds
                in
                Expect.ok <| QuadTree.isValid testTree
        , test "Multi-level tree should return inserted items" <|
            \_ ->
                let
                    boundeds =
                        [ { boundingBox = boundingBox 0 1 0 1 }
                        , { boundingBox = boundingBox -1 0 0 1 }
                        , { boundingBox = boundingBox 0 1 -1 0 }
                        , { boundingBox = boundingBox -1 0 -1 0 }
                        , { boundingBox = boundingBox 1 2 1 2 }
                        ]
                            |> List.sortWith sortXthenY

                    testTree =
                        List.foldl QuadTree.insert emptyTree boundeds
                in
                QuadTree.toList testTree
                    |> List.sortWith sortXthenY
                    |> Expect.equalLists boundeds
        ]


treeLookupTest : Test
treeLookupTest =
    describe "Find in tree"
        [ test "Find in 1 sized tree" <|
            \_ ->
                let
                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }

                    testTree =
                        QuadTree.insert bounded emptyTree

                    searchBox =
                        { boundingBox = boundingBox 0.5 0.5 0.5 0.5 }
                in
                Expect.equalLists [ bounded ] <| QuadTree.findItems searchBox testTree
        , test "Find in a multi-level tree" <|
            \_ ->
                let
                    items =
                        [ boundedItem 0 1 0 1
                        , boundedItem -1 0 -2 -1
                        , boundedItem 1 2 0 1
                        , boundedItem 2 4 -1 0
                        , boundedItem -3 -1 -1 0
                        ]

                    testTree =
                        QuadTree.insertList items emptyTree

                    searchBox =
                        boundedItem -2 -2 -0.5 -0.5
                in
                QuadTree.findIntersecting searchBox testTree
                    |> Expect.equalLists (List.take 1 (List.reverse items))
        ]


neighborsWithinTest : Test
neighborsWithinTest =
    describe "Find all the neighbors within a particular distance"
        [ test "Empty tree" <|
            \_ ->
                let
                    queryItem =
                        boundingBox 0 1 0 1

                    result =
                        QuadTree.neighborsWithin (Quantity.float 1) queryItem emptyTree
                in
                Expect.equal [] result
        , test "Single item tree no match" <|
            \_ ->
                let
                    item =
                        boundedItem 2 3 2 3

                    tree =
                        QuadTree.insert item emptyTree

                    queryItem =
                        boundingBox 0 1 0 1

                    result =
                        QuadTree.neighborsWithin (Quantity.float 1) queryItem tree
                in
                Expect.equal [] result
        , test "Single item tree expecting one match" <|
            \_ ->
                let
                    item =
                        boundedItem 1 2 1 2

                    tree =
                        QuadTree.insert item emptyTree

                    queryItem =
                        boundingBox 0 1 0 1

                    result =
                        QuadTree.neighborsWithin (Quantity.float 2) queryItem tree
                in
                Expect.equalLists [ item ] result
        , test "Multi-level tree search expecting three items" <|
            \_ ->
                let
                    closeItems =
                        [ boundedItem -1 0 -2 -1
                        , boundedItem 2 4 -1 0
                        ]
                            |> List.sortWith sortXthenY

                    distantItems =
                        [ boundedItem 0 1 0 1
                        , boundedItem 1 2 0 1
                        , boundedItem -3 -1 -1 0
                        ]

                    tree =
                        emptyTree
                            |> QuadTree.insertList closeItems
                            |> QuadTree.insertList distantItems

                    queryItem =
                        boundingBox 1 2 -3 -2

                    result =
                        QuadTree.neighborsWithin (Quantity.float 1.5) queryItem tree
                            |> List.sortWith sortXthenY
                in
                Expect.equalLists closeItems result
        ]



---- Helper Functions


boundedItem minX maxX minY maxY =
    { boundingBox = boundingBox minX maxX minY maxY }


boundingBox : Float -> Float -> Float -> Float -> BoundingBox2d Unitless coordinates
boundingBox minX maxX minY maxY =
    BoundingBox2d.from
        (Point2d.unitless minX minY)
        (Point2d.unitless maxX maxY)


emptyTree : QuadTree Unitless coordinates (Bounded Unitless coordinates a)
emptyTree =
    let
        treeLimits =
            boundingBox -10 10 -10 10
    in
    QuadTree.init treeLimits 4


sortXthenY : Bounded Unitless coordinates a -> Bounded Unitless coordinates a -> Order
sortXthenY box1 box2 =
    let
        ( x1, y1 ) =
            Point2d.toTuple Quantity.toFloat <| BoundingBox2d.centerPoint box1.boundingBox

        ( x2, y2 ) =
            Point2d.toTuple Quantity.toFloat <| BoundingBox2d.centerPoint box2.boundingBox
    in
    case x1 == x2 of
        True ->
            case y1 == y2 of
                True ->
                    EQ

                False ->
                    case y1 < y2 of
                        True ->
                            LT

                        False ->
                            GT

        False ->
            case x1 < x2 of
                True ->
                    LT

                False ->
                    GT
