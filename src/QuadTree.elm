module QuadTree exposing
    ( QuadTree, init
    , Bounded
    , getMaxSize, getBoundingBox, length
    , insert, insertMany
    , remove
    , update
    , findItems, findIntersecting, toArray
    , apply, applySafe, map, mapSafe
    , reset
    )

{-| QuadTree implementation in Elm.


# QuadTree

@docs QuadTree, init


# Bounding Boxes

@docs Bounded


# Properties

@docs getMaxSize, getBoundingBox, length


# Inserting items

@docs insert, insertMany


# Removing items

@docs remove


# Updating items

@docs update


# Querying

@docs findItems, findIntersecting, toArray


# Applying functions

@docs apply, applySafe, map, mapSafe


# Reset a QuadTree

@docs reset

-}

import Array
import BoundingBox2d exposing (BoundingBox2d)
import Point2d


dropIf : (a -> Bool) -> Array.Array a -> Array.Array a
dropIf predicate =
    Array.filter (not << predicate)


flippedMap : (a -> Array.Array a -> a) -> Array.Array a -> Array.Array a
flippedMap f array =
    let
        g y x =
            f x y
    in
    Array.map (g array) array


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


quadrants : BoundingBox2d units coordinates -> Quadrants units coordinates
quadrants box =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema box

        extrema =
            { ne = Point2d.xy minX maxY
            , nw = Point2d.xy maxX maxY
            , sw = Point2d.xy maxX minY
            , se = Point2d.xy minX minY
            }
    in
    { northEast = BoundingBox2d.scaleAbout extrema.ne 0.5 box
    , northWest = BoundingBox2d.scaleAbout extrema.nw 0.5 box
    , southEast = BoundingBox2d.scaleAbout extrema.se 0.5 box
    , southWest = BoundingBox2d.scaleAbout extrema.sw 0.5 box
    }



---------


{-| Extend this record type in order to use the QuadTree.
-}
type alias Bounded units coordinates a =
    { a | boundingBox : BoundingBox2d units coordinates }



---------


{-| QuadTree type. Keeps its elements in the leaves and
keeps track of the maximum number of items that
can be inserted in each leaf.
-}
type QuadTree units coordinates a
    = Leaf (BoundingBox2d units coordinates) Int (Array.Array a)
    | Node (BoundingBox2d units coordinates) (QuadTree units coordinates a) (QuadTree units coordinates a) (QuadTree units coordinates a) (QuadTree units coordinates a)


{-| Construct an empty QuadTree given a bounding box and
a maxSize. The maxSize limits the number of elements
that each leaf of the QuadTree can hold.
-}
init : BoundingBox2d units coordinates -> Int -> QuadTree units coordinates a
init theBoundingBox maxSize =
    Leaf theBoundingBox maxSize Array.empty


{-| Find the number of items in a quadTree. If elements are
duplicated in different leaves, they will be counted
multiple times.
-}
length : QuadTree units coordinates a -> Int
length quadTree =
    case quadTree of
        Leaf _ _ items ->
            Array.length items

        Node _ quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
            length quadTreeNE
                + length quadTreeNW
                + length quadTreeSW
                + length quadTreeSE


{-| Insert an item into a quadTree.
-}
insert :
    Bounded units coordinates a
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
insert item quadTree =
    case quadTree of
        Leaf box maxSize items ->
            if BoundingBox2d.intersects item.boundingBox box then
                let
                    allItems =
                        Array.push item items

                    insertNew quadrant =
                        Array.foldr (\i qt -> insert i qt)
                            (init quadrant maxSize)
                            allItems

                    subQuadrants =
                        quadrants box
                in
                if Array.length items < maxSize then
                    Leaf box maxSize (Array.push item items)

                else
                    Node box
                        (insertNew subQuadrants.northEast)
                        (insertNew subQuadrants.northWest)
                        (insertNew subQuadrants.southWest)
                        (insertNew subQuadrants.southEast)

            else
                quadTree

        Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
            if BoundingBox2d.intersects item.boundingBox box then
                Node box
                    (insert item quadTreeNE)
                    (insert item quadTreeNW)
                    (insert item quadTreeSW)
                    (insert item quadTreeSE)

            else
                quadTree


{-| Insert an array of items into a quadTree.
-}
insertMany :
    Array.Array (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates a)
insertMany theItems theQuadTree =
    let
        stoppingCondition { items } =
            Array.get 0 items == Nothing

        loopBody ({ items, quadTree } as variables) =
            case Array.get 0 items of
                Nothing ->
                    variables

                Just item ->
                    { items = Array.slice 1 (Array.length items) items, quadTree = insert item quadTree }

        returnFunction =
            .quadTree
    in
    loop { items = theItems, quadTree = theQuadTree }
        stoppingCondition
        loopBody
        returnFunction


{-| Remove an item from a quadTree and return the new quadTree.
If an item is found in multiple leaves, then the item will
be removed from all leaves.
-}
remove : a -> QuadTree units coordinates a -> QuadTree units coordinates a
remove item quadTree =
    case quadTree of
        Leaf box maxSize items ->
            Leaf box maxSize (dropIf (\it -> it == item) items)

        Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
            Node box
                (remove item quadTreeNE)
                (remove item quadTreeNW)
                (remove item quadTreeSW)
                (remove item quadTreeSE)


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

        Node box _ _ _ _ ->
            box


{-| Get the maxSize of a quadTree.
-}
getMaxSize : QuadTree units coordinates a -> Int
getMaxSize quadTree =
    case quadTree of
        Leaf _ maxSize _ ->
            maxSize

        Node _ quadrant _ _ _ ->
            getMaxSize quadrant


{-| Get all items from a quadTree. Conserves duplicates.
-}
toArray : QuadTree units coordinates a -> Array.Array a
toArray quadTree =
    case quadTree of
        Leaf _ _ items ->
            items

        Node _ quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
            toArray quadTreeNE
                |> Array.append (toArray quadTreeNW)
                |> Array.append (toArray quadTreeSW)
                |> Array.append (toArray quadTreeSE)


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
    insertMany (toArray quadTree)
        (init (getBoundingBox quadTree) (getMaxSize quadTree))


{-| Find all items in the quadTree which share a leaf with the given
item or would share a leaf with the given item were the item in
the quadTree. Useful for finding items close to the given item.
-}
findItems :
    Bounded units coordinates a
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> Array.Array (Bounded units coordinates a)
findItems item quadTree =
    case quadTree of
        Leaf box _ items ->
            if BoundingBox2d.intersects item.boundingBox box then
                items

            else
                Array.empty

        Node _ quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
            findItems item quadTreeNE
                |> Array.append (findItems item quadTreeNW)
                |> Array.append (findItems item quadTreeSW)
                |> Array.append (findItems item quadTreeSE)


{-| Find all items that actually intersect with the given item.

Similar to `findItems` but will return only intersection items, without neighboring items.

-}
findIntersecting :
    Bounded units coordinates a
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> Array.Array (Bounded units coordinates a)
findIntersecting bounded quadTree =
    Array.filter
        (\listItem -> BoundingBox2d.intersects bounded.boundingBox listItem.boundingBox)
    <|
        findItems bounded quadTree


{-| Apply a function, that takes an item and an array of items
and returns an item, to a quadTree. This function is
a useful helper for collision detection and response
where the input function updates an object after colliding
it with an array of objects.
-}
apply : (a -> Array.Array a -> a) -> QuadTree units coordinates a -> QuadTree units coordinates a
apply f quadTree =
    case quadTree of
        Leaf box maxSize items ->
            Leaf box maxSize (flippedMap f items)

        Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
            Node box
                (apply f quadTreeNE)
                (apply f quadTreeNW)
                (apply f quadTreeSW)
                (apply f quadTreeSE)


{-| Safe version of apply. Automatically calls reset after applying
the function on the quadTree.
-}
applySafe :
    (Bounded units coordinates a -> Array.Array (Bounded units coordinates a) -> Bounded units coordinates a)
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
            Leaf box maxSize (Array.map f items)

        Node box quadTreeNE quadTreeNW quadTreeSW quadTreeSE ->
            Node box
                (map f quadTreeNE)
                (map f quadTreeNW)
                (map f quadTreeSW)
                (map f quadTreeSE)


{-| Version of `map` where the quadTree is reset
automatically after applying the function.
-}
mapSafe :
    (Bounded units coordinates a -> Bounded units coordinates b)
    -> QuadTree units coordinates (Bounded units coordinates a)
    -> QuadTree units coordinates (Bounded units coordinates b)
mapSafe f quadTree =
    reset <| map f quadTree
