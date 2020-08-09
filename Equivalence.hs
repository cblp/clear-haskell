import           RIO

import           Data.Bifunctor (Bifunctor, bimap)
import           Data.Generics.Text (gshow)
import           DynFlags (defaultDynFlags, xopt_set)
import           GHC.LanguageExtensions (Extension (EmptyCase, LambdaCase))
import           Language.Haskell.GhclibParserEx.Config (fakeLlvmConfig,
                                                         fakeSettings)
import           Language.Haskell.GhclibParserEx.GHC.Parser (parseFile)
import           Lexer (ParseResult (..), getErrorMessages, getMessages)
import qualified RIO.Text as Text
import           Text.Pretty.Simple (pPrintString)

main :: IO ()
main =
  do
    content <- readFileUtf8 file
    pPrintString $
      showParseResult $ parseFile file dynFlags $ Text.unpack content
  where
    file = "Equivalence.hs"

    dynFlags =
      defaultDynFlags fakeSettings fakeLlvmConfig
      & (`xopt_set` EmptyCase)
      & (`xopt_set` LambdaCase)

    showParseResult = \case
      POk state result ->
        "POk {state_messages = "
        ++ show (bimapEach toList $ getMessages state dynFlags)
        ++ ", result = " ++ gshow result ++ "}"
      PFailed state ->
        "PFailed{state_messages = "
        ++ show (bimapEach toList $ getMessages state dynFlags)
        ++ ", state_errorMessages = "
        ++ show (toList $ getErrorMessages state dynFlags) ++ "}"

bimapEach :: Bifunctor bi => (a -> b) -> bi a a -> bi b b
bimapEach f = bimap f f
