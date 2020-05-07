module Tests exposing (..)

import Array
import BoundingBox2d exposing (BoundingBox2d)
import Expect
import Point2d
import QuadTree
    exposing
        ( Bounded
        , findIntersecting
        , findItems
        , getAllItems
        , init
        , insert
        , insertMany
        , length
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


treeLimits : BoundingBox2d Unitless coordinates
treeLimits =
    boundingBox -10 10 -10 10


quadTreeInsertTest : Test
quadTreeInsertTest =
    describe "Insert to Quad Tree"
        [ test "insert to empty" <|
            \() ->
                let
                    tree =
                        init treeLimits 4

                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }
                in
                Expect.equal (length (insert bounded tree)) 1
        , test "find in 1 element quad tree" <|
            \() ->
                let
                    tree =
                        init treeLimits 4

                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }

                    treeWithElements =
                        insert bounded tree
                in
                Expect.equalLists (Array.toList <| getAllItems treeWithElements) [ bounded ]
        , test "Add multiple items" <|
            \() ->
                let
                    tree =
                        init treeLimits 4

                    boundeds =
                        [ { boundingBox = boundingBox 0 1 0 1 }
                        , { boundingBox = boundingBox -1 0 0 1 }
                        , { boundingBox = boundingBox 0 1 -1 0 }
                        , { boundingBox = boundingBox -1 0 -1 0 }
                        ]

                    testTree =
                        List.foldl insert tree boundeds
                in
                Expect.equalLists (Array.toList <| getAllItems testTree) boundeds
        ]


treeLookupTest : Test
treeLookupTest =
    describe "Find in tree"
        [ test "Find in 1 sized tree" <|
            \() ->
                let
                    tree =
                        init treeLimits 4

                    bounded =
                        { boundingBox = boundingBox 0 1 0 1 }

                    testTree =
                        insert bounded tree

                    searchBox =
                        { boundingBox = boundingBox 0.5 0.5 0.5 0.5 }
                in
                Expect.equalLists [ bounded ] (Array.toList <| findItems searchBox testTree)
        , test "Find in a massive tree" <|
            \() ->
                let
                    tree =
                        init treeLimits 4

                    items =
                        [ boundedItem 0 1 0 1
                        , boundedItem -1 0 -2 -1
                        , boundedItem 1 2 0 1
                        , boundedItem 2 4 -1 0
                        , boundedItem -3 -1 -1 0
                        ]

                    testTree =
                        insertMany (Array.fromList items) tree

                    searchBox =
                        boundedItem -2 -2 -0.5 -0.5
                in
                Expect.equalLists (List.take 1 <| List.reverse items) (Array.toList <| findIntersecting searchBox testTree)
        ]
