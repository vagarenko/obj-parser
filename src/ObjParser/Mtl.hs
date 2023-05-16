{-# LANGUAGE 
      OverloadedStrings
    , TemplateHaskell
#-}

module ObjParser.Mtl (
    -- * Parsing
      parse
    , parseMaterialId

    -- * Material
    , Mtl
    , MaterialId
    , Material(..)

    -- ** Material lenses
    , materialId
    , materialAmbientColor
    , materialDiffuseColor
    , materialSpecularColor
    , materialEmissionColor
    , materialTransmissionFilter
    , materialIlluminationModel
    , materialTransparency
    , materialDissolve
    , materialSpecularExponent
    , materialOpticalDensity
    , materialAmbientMap
    , materialDiffuseMap
    , materialSpecularMap
    , materialEmissionMap
    , materialSpecularExponentMap
    , materialDissolveMap
    , materialBumpMap
) where

import ObjParser.Utils           (skipHSpace, trimLineR, vector3f, float, fileName)
import Control.Applicative       (many, (<|>))
import Control.Lens              (makeLenses, (^.), (&), set)
import Data.Attoparsec.Text      (parseOnly, Parser, manyTill, endOfInput, (<?>), string,
                                  endOfLine, decimal, char, skipWhile, isEndOfLine, takeWhile1)
import Data.Char                 (isAlpha, isDigit)
import Data.List                 (foldl')
import GHC.Generics              (Generic)
import qualified Data.Map.Strict as M (Map, empty, insert)
import qualified Data.Text       as T (Text)
import Linear

-- | Content of an .mtl file
type Mtl = M.Map MaterialId Material

-- | Id of a material.
type MaterialId = T.Text

-- | Model's material.
data Material = Material {
        _materialId                  :: MaterialId,          -- ^ Material's id.
        _materialAmbientColor        :: Maybe (V3 Float),      -- ^ Ka command.
        _materialDiffuseColor        :: Maybe (V3 Float),      -- ^ Kd command.
        _materialSpecularColor       :: Maybe (V3 Float),      -- ^ Ks command.
        _materialEmissionColor       :: Maybe (V3 Float),      -- ^ Ke command.
        _materialTransmissionFilter  :: Maybe (V3 Float),      -- ^ Tf command.
        _materialIlluminationModel   :: Maybe Int,           -- ^ illum  command.
        _materialTransparency        :: Maybe Float,         -- ^ Tr  command.
        _materialDissolve            :: Maybe Float,         -- ^ d  command.
        _materialSpecularExponent    :: Maybe Float,         -- ^ Ns  command.
        _materialOpticalDensity      :: Maybe Float,         -- ^ Ni  command.
        _materialAmbientMap          :: Maybe FilePath,      -- ^ map_Ka  command.
        _materialDiffuseMap          :: Maybe FilePath,      -- ^ map_Kd  command.
        _materialSpecularMap         :: Maybe FilePath,      -- ^ map_Ks  command.
        _materialEmissionMap         :: Maybe FilePath,      -- ^ map_Ke  command.
        _materialSpecularExponentMap :: Maybe FilePath,      -- ^ map_Ns  command.
        _materialDissolveMap         :: Maybe FilePath,      -- ^ map_d  command.
        _materialBumpMap             :: Maybe FilePath       -- ^ map_bump  command.
    } deriving (Eq, Show, Generic, Ord)
makeLenses ''Material

-- | Parse .mtl text and return a map of materials.
parse :: T.Text -> Either String Mtl
parse t = foldr go M.empty <$> parseOnly parseMtl (t <> "\n")
    where
        go mat = M.insert (mat ^. materialId) mat

-- | Material name must consist of alphabetical characters, digits, underscore or peroid.
parseMaterialId :: Parser MaterialId
parseMaterialId = takeWhile1 (\c -> isAlpha c || isDigit c || c == '_' || c == '.') <?> "parseMaterialId"

---------------------------------------------------------------------------------------------------
-- PRIVATE:
---------------------------------------------------------------------------------------------------

-- | Parse material definitions.
parseMtl :: Parser [Material]
parseMtl = manyTill parseMaterial endOfInput

-- | Parse single material.
parseMaterial :: Parser Material
parseMaterial = do
    _ <- many (emptyLine <|> commentLine)
    name <- material
    let emptyMaterial = Material name Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
                                 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    -- modifiers :: [Material -> Material]
    modifiers <- many 
        (   line (prefVector3f "Ka")       materialAmbientColor
        <|> line (prefVector3f "Kd")       materialDiffuseColor
        <|> line (prefVector3f "Ks")       materialSpecularColor
        <|> line (prefVector3f "Ke")       materialEmissionColor
        <|> line (prefVector3f "Tf")       materialTransmissionFilter
        <|> line (prefDecimal  "illum")    materialIlluminationModel
        <|> line (prefFloat    "Tr")       materialTransparency
        <|> line (prefFloat    "d" )       materialDissolve
        <|> line (prefFloat    "Ns")       materialSpecularExponent
        <|> line (prefFloat    "Ni")       materialOpticalDensity
        <|> line (prefFileName "map_Ka")   materialAmbientMap
        <|> line (prefFileName "map_Kd")   materialDiffuseMap
        <|> line (prefFileName "map_Ks")   materialSpecularMap
        <|> line (prefFileName "map_Ke")   materialEmissionMap
        <|> line (prefFileName "map_Ns")   materialSpecularExponentMap
        <|> line (prefFileName "map_d")    materialDissolveMap
        <|> line (prefFileName "map_bump") materialBumpMap
        <|> commentLine
        <|> emptyLine)

    return $ foldl' (&) emptyMaterial modifiers
    <?> "Material"
    where 
        line parser setter = do
            skipHSpace
            x <- parser
            trimLineR
            return (set setter (Just x))

        material       = skipHSpace *> string "newmtl" *> skipHSpace *> parseMaterialId <* endOfLine <?> "material"
        prefVector3f s = string s *> skipHSpace *> vector3f <?> "prefVector3f"
        prefFloat    s = string s *> skipHSpace *> float    <?> "prefFloat"
        prefDecimal  s = string s *> skipHSpace *> decimal  <?> "prefDecimal"
        prefFileName s = string s *> skipHSpace *> fileName <?> "prefFileName"
        emptyLine      = trimLineR *> pure id <?> "emptyLine"
        commentLine    = char '#' *> skipWhile (not . isEndOfLine) *> endOfLine *> pure id <?> "commentLine"