module Tests exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Expect
import Point2d
import QuadTree
    exposing
        ( Bounded
        , QuadTree
        , findIntersecting
        , findItems
        , init
        , insert
        , insertList
        , length
        , toList
        )
import Quantity exposing (Unitless)
import Test exposing (Test, describe, test)


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
    init treeLimits 4


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


quadTreeInsertTest : Test
quadTreeInsertTest =
    describe "Insert to Quad Tree"
        [ test "insert to empty" <|
            \_ ->
                let
                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }
                in
                Expect.equal (length (insert bounded emptyTree)) 1
        , test "find in 1 element quad tree" <|
            \_ ->
                let
                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }

                    treeWithElements =
                        insert bounded emptyTree
                in
                Expect.equalLists (toList treeWithElements) [ bounded ]
        , test "Add multiple items" <|
            \_ ->
                let
                    boundeds =
                        [ { boundingBox = boundingBox 0 1 0 1 }
                        , { boundingBox = boundingBox -1 0 0 1 }
                        , { boundingBox = boundingBox 0 1 -1 0 }
                        , { boundingBox = boundingBox -1 0 -1 0 }
                        ]
                            |> List.sortWith sortXthenY

                    testTree =
                        List.foldl insert emptyTree boundeds
                in
                toList testTree
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
                        insert bounded emptyTree

                    searchBox =
                        { boundingBox = boundingBox 0.5 0.5 0.5 0.5 }
                in
                Expect.equalLists [ bounded ] <| findItems searchBox testTree
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
                        insertList items emptyTree

                    searchBox =
                        boundedItem -2 -2 -0.5 -0.5
                in
                Expect.equalLists (List.take 1 <| List.reverse items) <| findIntersecting searchBox testTree
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
                in
                Expect.equalLists closeItems result
        ]
