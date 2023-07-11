module BST (
    BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
) where

data BST a = Empty | Leaf a | Node (BST a) a (BST a) deriving (Eq, Show)

instance (Ord a) => Monoid (BST a) where
    mempty = Empty

instance (Ord a) => Semigroup (BST a) where
    Empty <> tree = tree
    tree <> Empty = tree
    l1@(Leaf x) <> (Leaf y)
        | x <= y = Node l1 y mempty
        | otherwise = Node mempty y l1
    l@(Leaf x) <> (Node left y right)
        | x <= y = Node (l <> left) y right
        | otherwise = Node left y (l <> right)
    (Node left y right) <> (Leaf x) = Leaf x <> Node left y right
    n1@(Node{}) <> n2@(Node{}) = foldr (<>) n2 (singleton <$> toList n1)

instance Foldable BST where
    foldMap _ Empty = mempty
    foldMap f (Leaf x) = f x
    foldMap f (Node left x right) = foldMap f left <> f x <> foldMap f right

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Leaf _) = Just Empty
bstLeft (Node left _ _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Leaf _) = Just Empty
bstRight (Node _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Leaf x) = Just x
bstValue (Node _ x _) = Just x

empty :: BST a
empty = Empty

fromList :: (Ord a) => [a] -> BST a
fromList = foldr insert mempty . reverse

insert :: (Ord a) => a -> BST a -> BST a
insert x tree = singleton x <> tree

singleton :: a -> BST a
singleton = Leaf

toList :: BST a -> [a]
toList = foldMap (: [])
