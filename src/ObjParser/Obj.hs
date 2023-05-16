{-# LANGUAGE 
      OverloadedStrings
    , TemplateHaskell
#-}


module ObjParser.Obj (
    -- * .Obj parsing
      Obj(..)
    , parse
    , VertexCoords
    , TextureCoords
    , VertexNormal

    -- * Face
    , Face(..)

    -- * Vertex
    , Vertex(..)

    -- * FaceEnv
    , FaceEnv(..)
    , faceEnvObject
    , faceEnvMtlLibs
    , faceEnvGroups
    , faceEnvSmoothingGroup
    , faceEnvUseMtl
    , Object
    , MtlLib
    , Group
    , SmoothingGroup
    , MaterialId
) where

import ObjParser.Mtl               (parseMaterialId, MaterialId)
import ObjParser.Utils             (trimLineR, skipHSpace, fileName, vector3f, float, decimal1)
import Control.Applicative         ((<|>))
import Control.Lens
import Data.Attoparsec.Text        (Parser, parseOnly, manyTill, endOfInput, (<?>), string, isEndOfLine,
                                    takeWhile1, decimal, sepBy, sepBy1, char)
import Data.Char                   (isAlpha, isDigit)
import Data.Functor                (($>))
import Data.List                   (foldl')
import Data.Maybe                  (catMaybes)
import Data.Word                   (Word32)
import GHC.Generics                (Generic)
import qualified Data.Attoparsec.Text as A (takeWhile)
import qualified Data.Text            as T (Text)
import qualified Data.Vector          as V (Vector, fromList)
import qualified Data.DList           as D (snoc, empty, toList)
import Linear


-- | Parsed .obj file.
data Obj = Obj !(V.Vector VertexCoords) 
               !(V.Vector TextureCoords)
               !(V.Vector VertexNormal)
               !(V.Vector (Face, FaceEnv))
    deriving (Eq, Show, Generic)

type VertexCoords  = V3 Float
type TextureCoords = V3 Float
type VertexNormal  = V3 Float

-- | Polygon with at least 3 vertices.
data Face = Face !Vertex !Vertex !Vertex ![Vertex]
    deriving (Eq, Show, Ord, Generic)

-- | Vertex does not contain position, texture coords or normals, instead it contains indexes of `v`, `vt`, `vn` commands
--   which contain these data. Vertex must have index of `v` command but not necessarily have indexes of `vt` or `vn`.
data Vertex = Vertex {
        vertexPositionIndex      :: !Word32,         -- ^ Index of the `v` command containing vertex' position.
        vertexTextureCoordsIndex :: !(Maybe Word32), -- ^ Index of the `vt` command containing vertex' texture coords.
        vertexNormalIndex        :: !(Maybe Word32)  -- ^ Index of the `vn` command containing vertex' normal.
    } deriving (Eq, Show, Ord, Generic)

type MtlLib         = FilePath
type Object         = T.Text
type Group          = T.Text 
type SmoothingGroup = Word32

-- | Environment in which face was defined.
data FaceEnv = FaceEnv {
        _faceEnvObject         :: !(Maybe Object),          -- ^ Name of the object.
        _faceEnvMtlLibs        :: !(Maybe [MtlLib]),        -- ^ List of .mtl files in use.
        _faceEnvGroups         :: !(Maybe [Group]),         -- ^ Names of the groups containing the face. Face may belong to multiple groups in the object.
        _faceEnvSmoothingGroup :: !(Maybe SmoothingGroup),  -- ^ Index of smoothing group.
        _faceEnvUseMtl         :: !(Maybe MaterialId)       -- ^ Id of the face's material.
    } deriving (Eq, Show, Ord, Generic)
makeLenses ''FaceEnv

-- | Parse .obj format from text.
parse :: T.Text -> Either String Obj
parse t = parseOnly parseObj (t <> "\n")  -- parser expects newline at the end of input

---------------------------------------------------------------------------------------------------
-- PRIVATE:
---------------------------------------------------------------------------------------------------

-- | Obj parser.
parseObj :: Parser Obj
parseObj = do
    fileLines <- manyTill parseLine endOfInput

    let commands = catMaybes fileLines
        (vs, ts, ns, fs) = processCommands commands
    return $ Obj (V.fromList vs) (V.fromList ts) (V.fromList ns) (V.fromList fs)
    <?> "Obj"

-- | .Obj file is essentialy a list of commands, which can:
--   * specify vertex attributes such as position, texture coordinates, normal
--   * specify polygons made of vertices
--   * modify global state of a scene.
data Command = 
      VertexPositionCommand      !VertexCoords
    | VertexTextureCoordsCommand !TextureCoords
    | VertexNormalCommand        !VertexNormal
    | FaceCommand                !Face
    | MtlLibCommand              ![MtlLib]
    | ObjectCommand              !Object
    | GroupCommand               ![Group]
    | SmoothingGroupCommand      !(Maybe SmoothingGroup)
    | UseMtlCommand              !(Maybe MaterialId)
    deriving (Eq, Show, Ord, Generic)

-- | Process a list of commands and return lists of vertex attributes and faces created by those commands.
processCommands :: [Command] -> ([VertexCoords], [TextureCoords], [VertexNormal], [(Face, FaceEnv)])
processCommands commands = 
    case foldl' go (D.empty, D.empty, D.empty, D.empty, zState) commands of
        (vs, ts, ns, fs, _st) -> (D.toList vs, D.toList ts, D.toList ns, D.toList fs)
    where
        zState = FaceEnv Nothing Nothing Nothing Nothing Nothing
        go accs@(_vs, _ts, _ns, _fs, st) command =
            case command of
                VertexPositionCommand      v -> over _1 (-: v)       accs
                VertexTextureCoordsCommand t -> over _2 (-: t)       accs
                VertexNormalCommand        n -> over _3 (-: n)       accs
                FaceCommand                f -> over _4 (-: (f, st)) accs
                ObjectCommand              o -> set (_5 . faceEnvObject        ) (Just o) accs
                MtlLibCommand              m -> set (_5 . faceEnvMtlLibs       ) (Just m) accs
                GroupCommand               g -> set (_5 . faceEnvGroups        ) (Just g) accs
                SmoothingGroupCommand      s -> set (_5 . faceEnvSmoothingGroup) s        accs
                UseMtlCommand              u -> set (_5 . faceEnvUseMtl        ) u        accs
            where 
                xs -: x = D.snoc xs x

-- | Parse single line of a file, returning `Nothing` for empty lines and comments.
parseLine :: Parser (Maybe Command)
parseLine =
        (emptyLine $> Nothing)
    <|> (comment   $> Nothing)
    <|> (Just <$> parseCommand)    
    <?> "Command"
    where
        emptyLine  = trimLineR
        comment    = parsePrefLine "#" parseComment

-- | Parse single command of an .obj file.
parseCommand :: Parser Command
parseCommand = 
        (VertexPositionCommand      <$> parsePrefLine "v"      parseVertexCoords  )
    <|> (VertexTextureCoordsCommand <$> parsePrefLine "vt"     parseTextureCoords )
    <|> (VertexNormalCommand        <$> parsePrefLine "vn"     parseVertexNormal  )
    <|> (FaceCommand                <$> parsePrefLine "f"      parseFace          )
    <|> (MtlLibCommand              <$> parsePrefLine "mtllib" parseMtlLib        )
    <|> (ObjectCommand              <$> parsePrefLine "o"      parseObject        )
    <|> (GroupCommand               <$> parsePrefLine "g"      parseGroup         )
    <|> (SmoothingGroupCommand      <$> parsePrefLine "s"      parseSmoothingGroup)
    <|> (UseMtlCommand              <$> parsePrefLine "usemtl" parseUseMtl        )
    <?> "Command"

-- | Parse a line of text beginnig with given prefix text.
parsePrefLine :: T.Text -> Parser a -> Parser a
parsePrefLine prefix parser = skipHSpace *> string prefix *> skipHSpace *> parser <* trimLineR
    <?> "Line"

-- | Comments are lines starting with `#`
parseComment :: Parser T.Text
parseComment = A.takeWhile (not . isEndOfLine)
    <?> "Comment"

-- | Name of an object
-- 
-- > o object_name
parseObject :: Parser Object
parseObject = takeWhile1 (not . isEndOfLine)
    <?> "Object"

-- | Smoothing group can be set to an integer greater than 0, or turned of
--   by providing '0' or "off"
--
-- > s 1
-- > s 0 
-- > s off
parseSmoothingGroup :: Parser (Maybe SmoothingGroup)
parseSmoothingGroup = do
    dec <|> (string "off" $> Nothing)
    <?> "SmoothingGroup"
    where
        dec = do
            d <- decimal
            return $ if d == 0 
                then Nothing
                else Just d    

-- | Name of the material to use.
--
-- > usemtl material1
parseUseMtl :: Parser (Maybe MaterialId)
parseUseMtl = (Just <$> parseMaterialId) <|> pure Nothing
    <?> "UseMtl"

-- | Specifies the group name for the elements that follow it. Elements can
--   have multiple group names.
--
-- > g group_name1 group_name2 ...
parseGroup :: Parser [Group]
parseGroup = identifier `sepBy1` skipHSpace
    <?> "Group"
    where
        identifier = takeWhile1 (\c -> isAlpha c || isDigit c || c == '_')

-- | Specifies one or more material library files
--
-- > mtllib filename1.mtl filename2.mtl ... 
parseMtlLib :: Parser [MtlLib]
parseMtlLib = fileName `sepBy1` skipHSpace
    <?> "MtlLib"

-- | Specifies vertex position
--
-- > v 1.0 1.0 1.0
parseVertexCoords :: Parser VertexCoords
parseVertexCoords = vector3f
    <?> "VertexCoords"

-- | Specifies vertex' texture coordinates. May contain one, two or three float numbers
--   corresponding to coordinates in one-, two- or three-dimensional texture.
--   However, this parser always returns 3d vector with unused coordinates set to zero.
--
--   > vt 1.0 1.0 1.0  -- parses to TextureCoords (Vector3f 1 1 1)
--   > vt 1.0 1.0      -- parses to TextureCoords (Vector3f 1 1 0)
--   > vt 1.0          -- parses to TextureCoords (Vector3f 1 0 0)
--   Unspecified texture coordinates are set to default value 0.
parseTextureCoords :: Parser TextureCoords
parseTextureCoords =
    (   vector3f
    <|> (V3 <$> (skipHSpace *> float) <*> (skipHSpace *> float) <*> pure 0)
    <|> (V3 <$> (skipHSpace *> float) <*> pure 0                <*> pure 0))
    <?> "TextureCoords"

-- | Specifies vertex' normal
--
-- > vn 1.0 1.0 1.0
parseVertexNormal :: Parser VertexNormal
parseVertexNormal = vector3f
    <?> "VertexNormal"

-- | Specifies single polygon of a model. Must have at least 3 vertices, and each vertex
--   must have position and may also have texture coordinates and normal.
parseFace :: Parser Face
parseFace = face vertexVTN <|> face vertexVT <|> face vertexVN <|> face vertexV
    <?> "Face"
    where 
        face p  =  Face 
               <$> (skipHSpace *> p)
               <*> (skipHSpace *> p)
               <*> (skipHSpace *> p)
               <*> (skipHSpace *> (p `sepBy` skipHSpace))

        vertexV   = Vertex <$> decimal1 <*> pure Nothing                      <*> pure Nothing
        vertexVT  = Vertex <$> decimal1 <*> (char '/' *> (Just <$> decimal1)) <*> pure Nothing
        vertexVTN = Vertex <$> decimal1 <*> (char '/' *> (Just <$> decimal1)) <*> (char '/' *>             (Just <$> decimal1))
        vertexVN  = Vertex <$> decimal1 <*> pure Nothing                      <*> (char '/' *> char '/' *> (Just <$> decimal1))