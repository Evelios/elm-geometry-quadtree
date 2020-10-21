module QuadTree exposing
    ( QuadTree, init
    , Bounded
    , getMaxSize, getBoundingBox, length
    , insert, insertList
    , remove
    , findItems, findIntersecting, toList, neighborsWithin
    , update
    , apply, applySafe, map, mapSafe
    , reset
    , isValid
    )

{-| QuadTree implementation in Elm.


# QuadTree

@docs QuadTree, init


# Bounding Boxes

@docs Bounded


# Properties

@docs getMaxSize, getBoundingBox, length


# Inserting items

@docs insert, insertList


# Removing items

@docs remove


# Querying

@docs findItems, findIntersecting, toList, neighborsWithin


# Updating items

@docs update


# Applying functions

@docs apply, applySafe, map, mapSafe


# Reset a QuadTree

@docs reset


# Testing functions

@docs isValid


-}

import BoundingBox2d exposing (BoundingBox2d)
import EverySet
import Point2d
import Quantity exposing (Quantity)
import Result.Extra


dropIf : (a -> Bool) -> List a -> List a
dropIf predicate =
    List.filter (not << predicate)


flippedMap : (a -> List a -> a) -> List a -> List a
flippedMap f array =
    let
        g y x =
            f x y
    in
    List.map (g array) array


loop : a -> (a -> Bool) -> (a -> a) -> (a -> b) -> b
loop start condition updateFn return =
    case condition start of
        True ->
            return start

        False ->
            loop (updateFn start) condition updateFn return



--------


type alias Quadrants units coordinates =
    { northEast : BoundingBox2d units coordinates
    , northWest : BoundingBox2d units coordinates
    , southEast : BoundingBox2d units coordinates
    , southWest : BoundingBox2d units coordinates
    }


quadrants : BoundingBox2d units coordinates -> List (BoundingBox2d units coordinates)
quadrants box =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema box

        quads =
            [ Point2d.xy minX maxY
            , Point2d.xy maxX maxY
            , Point2d.xy maxX minY
            , Point2d.xy minX minY
            ]
    in
    List.map (\extrema -> BoundingBox2d.scaleAbout extrema 0.5 box) quads



---------


{-| Extend this record type in order to use the QuadTree.
-}
type alias Bounded units coordinates a =
    { a | boundingBox : BoundingBox2d units coordinates }


{-| QuadTree type. Keeps its elements in the leaves and
keeps track of the maximum number of items that
can be inserted in each leaf.
-}
type QuadTree units coordinates a
    = Leaf (BoundingBox2d units coordinates) Int (List a)
    | Node (BoundingBox2d units coordinates) (List (QuadTree units coordinates a))


type Error
    = LeafItemsExceedsMaxSize
    | ItemsInWrongLeaves


{-|-}
isValid :
    QuadTree units coordinates (Bounded units coordinates a)
    -> Result Error ()
isValid quadTree =
    case quadTree of
        Leaf box maxSize items ->
            if maxSize < List.length items then
                Err LeafItemsExceedsMaxSize

            else if
                not <|
                    List.all
                        (\item -> BoundingBox2d.intersects box item.boundingBox)
                        items
            then
                Err ItemsInWrongLeaves

            else
                Ok ()

        Node _ theQuadrants ->
            List.map isValid theQuadrants
                |> Result.Extra.combine
                |> Result.map (\_ -> ())


{-| Construct an empty QuadTree given a bounding box and
a maxSize. The maxSize limits the number of elements
that each leaf of the QuadTree can hold.
-}
init : BoundingBox2d units coordinates -> Int -> QuadTree units coordinates a
init theBoundingBox maxSize =
    Leaf theBoundingBox maxSize []


{-| Find the number of items in a quadTree. If elements are
duplicated in different leaves, they will be counted
multiple times.
-}
length : QuadTree units coordinates a -> Int
length quadTree =
    case quadTree of
        Leaf _ _ items ->
            List.length items

        Node _ quadTrees ->
            List.foldl (\quad len -> len + length quad) 0 quadTrees


{-| Insert an item into a quadTree.
-}
insert :
    Bounded units coordinates a
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
insert newItems theQuadTree =
    case theQuadTree of
        Leaf leafBoundingBox maxSize items ->
            if BoundingBox2d.intersects newItems.boundingBox leafBoundingBox then
                if List.length items < maxSize then
                    Leaf leafBoundingBox maxSize (newItems :: items)

                else
                    Node leafBoundingBox
                        (List.map
                            (\boundingBox ->
                                List.foldl
                                    (\item quadTree -> insert item quadTree)
                                    (init boundingBox maxSize)
                                    (newItems :: items)
                            )
                            (quadrants leafBoundingBox)
                        )

            else
                theQuadTree

        Node box theQuadrants ->
            if BoundingBox2d.intersects newItems.boundingBox box then
                Node box
                    (List.foldl
                        (\subQuad quads -> insert newItems subQuad :: quads)
                        []
                        theQuadrants
                    )

            else
                theQuadTree


{-|-}
insertList :
    List (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
insertList theItems theQuadtree =
    List.foldl insert theQuadtree theItems


{-| Remove an item from a quadTree and return the new quadTree.
If an item is found in multiple leaves, then the item will
be removed from all leaves.
-}
remove : a -> QuadTree units coordinates a -> QuadTree units coordinates a
remove item quadTree =
    case quadTree of
        Leaf box maxSize items ->
            Leaf box maxSize (dropIf (\it -> it == item) items)

        Node box theQuadrants ->
            theQuadrants
                |> List.map (remove item)
                |> Node box


{-| Update an item in a quadTree. This is useful if you just want to
update a single item. This removes the item from the quadTree,
applies the given updateFunction, and then inserts the updated
item into the quadTree.
-}
update :
    (Bounded units coordinates a -> Bounded units coordinates a)
    -> Bounded units coordinates a
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
update updateFunction item quadTree =
    insert (updateFunction item) (remove item quadTree)


{-| Get the bounding box of a quadTree.
-}
getBoundingBox : QuadTree units coordinates a -> BoundingBox2d units coordinates
getBoundingBox quadTree =
    case quadTree of
        Leaf box _ _ ->
            box

        Node box _ ->
            box


{-| Get the maxSize of a quadTree.
-}
getMaxSize : QuadTree units coordinates a -> Int
getMaxSize quadTree =
    case quadTree of
        Leaf _ maxSize _ ->
            maxSize

        Node _ quadrant ->
            case List.head quadrant of
                Just quadrantItem ->
                    getMaxSize quadrantItem

                Nothing ->
                    0


{-| Get all items from a quadTree. Conserves duplicates.
-}
toList : QuadTree units coordinates a -> List a
toList quadTree =
    case quadTree of
        Leaf _ _ items ->
            items

        Node _ theQuadrants ->
            theQuadrants
                |> List.map toList
                |> List.concat
                |> EverySet.fromList
                |> EverySet.toList


{-| Reset a quadTree. This function gets all items
in a quadTree and pours them
into an empty quadTree. Useful if the items in
the quadTree find themselves in the wrong
leaves.
-}
reset :
    QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
reset quadTree =
    insertList (toList quadTree)
        (init (getBoundingBox quadTree) (getMaxSize quadTree))


{-| Find all items in the quadTree which share a leaf with the given
item or would share a leaf with the given item were the item in
the quadTree. Useful for finding items close to the given item.
-}
findItems :
    Bounded units coordinates a
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> List (Bounded units coordinates a)
findItems item quadTree =
    case quadTree of
        Leaf box _ items ->
            if BoundingBox2d.intersects item.boundingBox box then
                items

            else
                []

        Node _ theQuadrants ->
            theQuadrants
                |> List.map (findItems item)
                |> List.concat


{-| Find all items that actually intersect with the given item.

Similar to `findItems` but will return only intersection items, without neighboring items.

-}
findIntersecting :
    Bounded units coordinates a
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> List (Bounded units coordinates a)
findIntersecting bounded quadTree =
    List.filter
        (\listItem -> BoundingBox2d.intersects bounded.boundingBox listItem.boundingBox)
    <|
        findItems bounded quadTree


{-| Apply a function, that takes an item and an array of items
and returns an item, to a quadTree. This function is
a useful helper for collision detection and response
where the input function updates an object after colliding
it with an array of objects.
-}
apply : (a -> List a -> a) -> QuadTree units coordinates a -> QuadTree units coordinates a
apply f quadTree =
    case quadTree of
        Leaf box maxSize items ->
            Leaf box maxSize (flippedMap f items)

        Node box theQuadrants ->
            theQuadrants
                |> List.map (apply f)
                |> Node box


{-| Safe version of apply. Automatically calls reset after applying
the function on the quadTree.
-}
applySafe :
    (Bounded units coordinates a -> List (Bounded units coordinates a) -> Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
applySafe f quadTree =
    reset <| apply f quadTree


{-| The good 'ol map function.
Maps a function over a quadTree and returns a new quadTree.
Note: If your function modifies in any way the items'
bounding boxes, consider using `mapSafe` or calling reset
after you are done as the quadTree may have items in the
wrong place. This function doesn't do the clean-up
automatically. If you want such functionality, use `mapSafe`.
-}
map : (a -> b) -> QuadTree units coordinates a -> QuadTree units coordinates b
map f quadTree =
    case quadTree of
        Leaf box maxSize items ->
            Leaf box maxSize (List.map f items)

        Node box theQuadrants ->
            theQuadrants
                |> List.map (map f)
                |> Node box


{-| Version of `map` where the quadTree is reset
automatically after applying the function.
-}
mapSafe :
    (Bounded units coordinates a -> Bounded units coordinates b)
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates b)
mapSafe f quadTree =
    reset <| map f quadTree


{-| Find all the neighbors that are within a particular distance from an input object.
-}
neighborsWithin :
    Quantity Float units
    -> BoundingBox2d units coordinates
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> List (Bounded units coordinates a)
neighborsWithin distance box quadTree =
    case quadTree of
        Leaf _ _ items ->
            items
                |> List.filter
                    (\item ->
                        not <|
                            BoundingBox2d.separatedByAtLeast
                                distance
                                box
                                item.boundingBox
                    )

        Node _ theQuadrants ->
            theQuadrants
                |> List.filter
                    (\quad ->
                        not <|
                            BoundingBox2d.separatedByAtLeast
                                distance
                                box
                                (getBoundingBox quad)
                    )
                |> List.map (neighborsWithin distance box)
                |> List.concat
                |> EverySet.fromList
                |> EverySet.toList
