module YAML ( Node(..), YAML(..),
              -- buildTree, unEscape, tokenize, tokenizeBytes,
              getScalar, getMapping, getSequence,
              getMappingValues, getMappingList,
              showYaml, readYaml, parseYaml ) where

import YAML.Reference ( Code(..), tokenize ) -- , tokenizeBytes )
import Data.Maybe ( catMaybes )
import Data.Char ( isHexDigit, isSpace, isAlphaNum )

data Node = Leaf String
          | List [Node]
          | Map [(Node,Node)]
          | Null
          deriving ( Eq, Show )

singleT :: (YAML k,YAML v) => k -> v -> [(Node,Node)]
singleT k v = [(toNode k,toNode v)]

class YAML a where
    toNode :: a -> Node
    fromNode :: Node -> Maybe a
    toString :: [a] -> Maybe String
    toString _ = Nothing
    fromString :: String -> [a]
    fromString _ = []

instance YAML Node where
    toNode x = x
    fromNode x = Just x

instance YAML Char where
    toNode c = Leaf [c]
    fromNode (Leaf [c]) = Just c
    fromNode _ = Nothing
    toString = Just
    fromString s = s

instance YAML Int where
    toNode = Leaf . show
    fromNode (Leaf s) = case readsPrec 0 s of
                          [(x,"")] -> Just x
                          _        -> Nothing
    fromNode _ = Nothing

instance YAML a => YAML [a] where
    toNode ns = maybe (List $ map toNode ns) Leaf $ toString ns
    fromNode (List ns) = Just $ catMaybes $ map fromNode ns
    fromNode (Leaf x) = Just $ fromString x
    fromNode _ = Just []

newtype Assoc a b = Assoc { unAssoc :: [(a,b)] }
instance (YAML a, YAML b) => YAML (Assoc a b) where
    toNode (Assoc ns) = Map $ map (\(a,b) -> (toNode a,toNode b)) ns
    fromNode (Map t) = Just $ Assoc $ catMaybes $ map cm $
                       map (\(a,b) -> (fromNode a,fromNode b)) t
        where cm (Just a,Just b) = Just (a,b)
              cm _ = Nothing
    fromNode _ = Just $ Assoc []

instance YAML a => YAML (Maybe a) where
    toNode Nothing = Null
    toNode (Just x) = toNode x
    fromNode Null = Just Nothing
    fromNode n = case fromNode n of
                   Just x -> Just (Just x)
                   Nothing -> Nothing

instance (YAML a, YAML b) => YAML (Either a b) where
    toNode (Left  x) = Map $ singleT "Left" $ toNode x
    toNode (Right x) = Map $ singleT "Right" $ toNode x
    fromNode (Map t) = case t of
                         [(Leaf "Left",n)] -> Left `fmap` fromNode n
                         [(Leaf "Right",n)] -> Right `fmap` fromNode n
                         _ -> Nothing
    fromNode _ = Nothing

instance (YAML a, YAML b) => YAML (a,b) where
    toNode (a,b) = List [toNode a, toNode b]
    fromNode n = do [a,b] <- fromNode n
                    aa <- fromNode a
                    bb <- fromNode b
                    Just (aa,bb)

instance YAML Bool where
    toNode True  = Leaf "true"
    toNode False = Leaf "false"
    fromNode (Leaf "true") = Just True
    fromNode (Leaf "false") = Just False
    fromNode _ = Nothing

instance YAML () where
    toNode () = Null
    fromNode Null = Just ()
    fromNode _ = Nothing

-- Use the reference implementation to parse the stupid thing

type Aliases = [(String,Node)]
type Tokens = [(Code,String)]

data TokenTree = TextT String
               | MetaT String
               | LineFoldT  -- BreakT, IndT String, WhiteT String
               | LineFeedT
               | EscapeT [TokenTree]
               | CommentT [TokenTree]
               | DirectiveT [TokenTree]
               | TagT [TokenTree]
               | HandleT [TokenTree]
               | AnchorT [TokenTree]
               | PropertiesT [TokenTree]
               | AliasT [TokenTree]
               | ScalarT [TokenTree]
               | SequenceT [TokenTree]
               | MappingT [TokenTree]
               | NodeT [TokenTree]
               | PairT [TokenTree]
               | DocumentT [TokenTree]
               deriving ( Show )

buildTree :: Tokens -> [TokenTree]
buildTree = fst . btu Nothing
    where btu :: Maybe Code -> Tokens -> ([TokenTree],Tokens)
          btu _ [] = ([],[])
          btu e ((c,s):xs) =
              case c of
                e' | e == Just e' -> ([],xs)
                Text -> prepend $ TextT s
                Meta -> prepend $ MetaT s
                LineFold -> prepend LineFoldT
                LineFeed -> prepend LineFeedT
                BeginEscape -> branch EscapeT EndEscape
                BeginComment -> branch CommentT EndComment
                BeginDirective -> branch DirectiveT EndDirective
                BeginTag -> branch TagT EndTag
                BeginHandle -> branch HandleT EndHandle
                BeginAnchor -> branch AnchorT EndAnchor
                BeginProperties -> branch PropertiesT EndProperties
                BeginAlias -> branch AliasT EndAlias
                BeginScalar -> branch ScalarT EndScalar
                BeginSequence -> branch SequenceT EndSequence
                BeginMapping -> branch MappingT EndMapping
                BeginNode -> branch NodeT EndNode
                BeginPair -> branch PairT EndPair
                BeginDocument -> branch DocumentT EndDocument
                _ -> btu e xs
              where prepend :: TokenTree -> ([TokenTree],Tokens)
                    prepend t = let (ts,xs') = btu e xs
                                in (t:ts,xs')
                    branch :: ([TokenTree] -> TokenTree) -> Code
                           -> ([TokenTree],Tokens)
                    branch f e' = let (ts,xs') = btu (Just e') xs
                                      (ts',xs'') = btu e xs'
                                  in (f ts:ts',xs'')

-- parse :: Tokens -> [Node]
-- parse = stre
--     where stre [] = []
--           stre ((BeginDocument,_):xs) = docu (\n ts -> n:stre ts) xs
--           stre (_:xs) = stre xs
--           stre [] = []
--           docu :: (Node -> Tokens -> [Node]) -> Tokens -> [Node]
--           docu f ((BeginNode,_):xs) = node f xs
--           docu f ((EndDocument,_):xs) = f xs
--           docu f (_:xs) = docu f xs
--           docu _ [] = []                  -- impossible
--           node f ((BeginScalar,_):xs) = text "" (\n ts -> n:f ts) xs
--           node f ((BeginSequence,_):xs) = seq f xs
--           -- scalars - read text
--           text ss f ((Text,s):xs) = text (ss++s) f xs
--           text ss f ((BeginEscape,_):xs) = escText ss f xs
--           text ss f ((EndScalar,_):xs) = f (Leaf ss) xs
--           text ss f (_:xs) = text ss f xs -- impossible
--           text ss _ [] = [Scalar ss]      -- impossible
--           escText ss f ((Meta,s):xs) = escText (ss++unEscape s) f xs
--           escText ss f ((EndEscape,_):xs) = text ss f xs
--           escText ss f (_:xs) = escText ss f xs -- impossible
--           escText ss _ [] = [Leaf ss]         -- impossible
--           -- sequences
--           seq ys f ((BeginNode,_):xs) = node (\n ts -> seq (n:ys) f ts) xs
--           seq ys f ((EndSequence,_):xs) = f (List ys) xs
--           seq ys f (_:xs) = seq ys f xs

-- -- It looks like this might work, but it definitely waits until EndSequence
-- -- before it even makes a List...  Can we do better?  Probably not.

parseStream :: [TokenTree] -> [Node]
parseStream (DocumentT n:xs) = snd (parseNodes [] n) ++ parseStream xs
parseStream (_:xs) = parseStream xs
parseStream [] = []

parseNodes :: Aliases -> [TokenTree] -> (Aliases,[Node])
parseNodes as (NodeT n:xs) = let (as',n') = parseNode as n
                                 (as'',ns) = parseNodes as' xs
                             in (as'',n':ns)
parseNodes as (_:xs) = parseNodes as xs
parseNodes as [] = (as,[])

parseNode :: Aliases -> [TokenTree] -> (Aliases,Node)
parseNode as (PropertiesT ps:xs) = applyProp ps $ parseNode as xs
parseNode as (AliasT a:_) = (as,maybe Null id $ lookup (getMeta a) as)
parseNode as (ScalarT s:_) = (as,Leaf $ getText s)
parseNode as (SequenceT s:_) = let (as',ns) = parseNodes as s in (as',List ns)
parseNode as (MappingT m:_) = let (as',ps) = parsePairs as m in (as',Map ps)
parseNode as (_:xs) = parseNode as xs
parseNode as [] = (as,Null)

applyProp :: [TokenTree] -> (Aliases,Node) -> (Aliases,Node)
applyProp ((AnchorT a):xs) = addAlias (getMeta a) . applyProp xs
    where addAlias :: String -> (Aliases,Node) -> (Aliases,Node)
          addAlias k (as,n) = ((k,n):as,n)
applyProp (_:xs) = applyProp xs
applyProp [] = id

parsePairs :: Aliases -> [TokenTree] -> (Aliases,[(Node,Node)])
parsePairs as (PairT p:xs) = case parseNodes as p of
                               (as',[k,v]) -> let (as'',ps) = parsePairs as' xs
                                              in (as'',(k,v):ps)
                               (as',_) -> parsePairs as' xs
parsePairs as (_:xs) = parsePairs as xs
parsePairs as [] = (as,[])

getText :: [TokenTree] -> String
getText (TextT s:xs) = s++getText xs
getText (EscapeT s:xs) = unEscape (getMeta s)++getText xs
getText (_:xs) = getText xs
getText [] = ""

unEscape :: String -> String
unEscape d | length d `elem` [2,4] && all isHexDigit d
               = [toEnum $ read $ "0x"++d]
           | length d == 8 && all isHexDigit d
               = [toEnum $ read $ "0x"++take 4 d,toEnum $ read $ "0x"++drop 4 d]
unEscape [c] = search "\0\0a\ab\bt\tn\nv\vf\fr\re\x1bN\x85_\xa0L\x2028P\x2029"
    where search (a:b:xs) | a==c = [b]
                          | otherwise = search xs
          search _ = [c] -- space, ", /, \ (and anything else)
unEscape x = ""

getMeta :: [TokenTree] -> String
getMeta (MetaT s:xs) = s++getMeta xs
getMeta (_:xs) = getMeta xs
getMeta [] = ""

parseYaml :: String -> [Node]
parseYaml = parseStream . buildTree . tokenize "-"

readYaml :: YAML a => String -> Maybe a
readYaml s = case parseYaml s of
               [] -> Nothing
               (n:_) -> fromNode n



dumpNode :: Node -> String
dumpNode node0 = f False 0 node0 "\n" where
    f nn _ Null     = foo nn . showString "null"
    f nn _ (Leaf x) = foo nn . showString' x
    f nn i (List ns) =
        nl nn [ g i . showString "-" . f True (i + 1) n | n <- ns ]
    f nn i (Map ns) =
        nl nn [ g i . showString x . showString ":" . f True (i + 1) y
                    | (Leaf x,y) <- ns ] -- need to use ? if needed...
    g i = showString $ replicate i ' '
    nl _ [] = id
    nl nn xs = (if nn then ('\n':) else id) .
               foldr1 (\x y -> x . showChar '\n' . y ) xs
    foo True  = showChar ' '
    foo False = id

showYaml :: YAML a => a -> String
showYaml n = dumpNode (toNode n)

showString' :: String -> String -> String
showString' a b = if all isGood a then a ++ b else '"':f a b where
    f [] y = '"':y
    f (x:xs) ys | x == '\n'  = '\\':'n':f xs ys
                | isQuoteGood x = x:f xs ys
                | otherwise  = '\\':x:f xs ys
    isQuoteGood x = isGood x || isSpace x || x `elem` "!@#$%^&*(){}/"

isGood :: Char -> Bool
isGood x = isAlphaNum x || x `elem` "_-.@/"


getScalar :: Node -> Maybe String
getScalar (Leaf s) = Just s
getScalar (Map t) = lookup (Leaf "=") t >>= getScalar
getScalar _ = Nothing

-- | find an element from a mapping.

getMapping :: String -> Node -> Maybe Node
getMapping k (Map t) = lookup (Leaf k) t
getMapping "=" (Leaf x) = Just $ Leaf x
getMapping _ _ = Nothing

-- | read a mapping into a 'Trie'.

getMappingList :: Node -> [(Node,Node)]
getMappingList (Map t) = t
getMappingList l = singleT "=" l

-- | read a mapping into a list.

getMappingValues :: Node -> Maybe [Node]
getMappingValues (Map t) = Just $ map snd t
getMappingValues _ = Nothing

-- | read a sequence.

getSequence :: Node -> Maybe [Node]
getSequence (List ns) = Just ns
getSequence _ = Nothing
