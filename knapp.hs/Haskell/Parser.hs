{-# LANGUAGE OverloadedStrings #-}
module Parser
  (runParser)
  where

import Prelude hiding (Show(..))

import Control.Monad.State.Strict
import Data.Array.Unboxed
import Data.Conduit
import Data.Dynamic
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Error
import Knapp.Show
import Token
import Wording

import Debug.Trace


data GrammarSpecification =
  GrammarSpecification {
      grammarSpecificationTokenInterpretations :: Token -> Set Text,
      grammarSpecificationTerminals :: Set Text,
      grammarSpecificationStartSymbols :: Set Text,
      grammarSpecificationProductions :: Map Text [([Text], Dynamic)]
    }


data GrammarSymbol = GrammarSymbol Text
instance Eq GrammarSymbol where
  (==) (GrammarSymbol a) (GrammarSymbol b) = (==) a b
instance Ord GrammarSymbol where
  compare (GrammarSymbol a) (GrammarSymbol b) = compare a b
instance Show GrammarSymbol where
  show (GrammarSymbol text) = text


data Production
  = Production {
        productionLeftHandSide :: GrammarSymbol,
        productionRightHandSide :: [GrammarSymbol],
        productionReducer :: Dynamic
      }
  | StartProduction {
        startProductionSymbol :: GrammarSymbol
      }
instance Eq Production where
  (==) a@(Production { }) b@(Production { }) =
    case on (==) productionLeftHandSide a b of
      True -> on (==) productionRightHandSide a b
      False -> False
  (==) a@(StartProduction { }) b@(StartProduction { }) =
    on (==) startProductionSymbol a b
  (==) _ _ = False
instance Ord Production where
  compare a@(Production { }) b@(Production { }) =
    case on compare productionLeftHandSide a b of
      EQ -> on compare productionRightHandSide a b
      result -> result
  compare a@(StartProduction { }) b@(StartProduction { }) =
    on compare startProductionSymbol a b
  compare (Production { }) (StartProduction { }) = GT
  compare (StartProduction { }) (Production { }) = LT
instance Show Production where
  show production@(Production { }) =
    let shown = map show $ productionRightHandSide production
    in T.intercalate " "
         $ [show $ productionLeftHandSide production, "->"] ++ shown
  show production@(StartProduction { }) =
    T.concat ["(start) -> ", show $ startProductionSymbol production]


data Item =
  Item {
      itemProduction :: Production,
      itemIndex :: Int
    }
instance Eq Item where
  (==) a b =
    case on (==) itemProduction a b of
      True -> on (==) itemIndex a b
      False -> False
instance Ord Item where
  compare a b =
    case on compare itemProduction a b of
      EQ -> on compare itemIndex a b
      result -> result
instance Show Item where
  show item =
    let production = itemProduction item
        index = itemIndex item
        shownLeft =
          case production of
            Production { } -> show $ productionLeftHandSide production
            StartProduction { } -> "(start)"
        rightHandSide =
          case production of
            Production { } -> productionRightHandSide production
            StartProduction { } -> [startProductionSymbol production]
        shownRight = map show rightHandSide
        withDot = take index shownRight ++ ["."] ++ drop index shownRight
    in T.intercalate " "
         $ [shownLeft, "->"] ++ withDot


data GrammarState =
  GrammarState {
      grammarStateItems :: Set Item,
      grammarStateGotoMap :: Map GrammarSymbol (Set Item),
      grammarStateShiftMap :: Map GrammarSymbol (Set Item),
      grammarStateReductions :: Set Production
    }
instance Show GrammarState where
  show state =
    T.intercalate "\n" $ map show
      $ Set.toList $ grammarStateItems state


data Grammar =
  Grammar {
      grammarTerminals :: Set GrammarSymbol,
      grammarNonterminals :: Set GrammarSymbol,
      grammarTokenInterpretations :: Token -> Set GrammarSymbol,
      grammarProductions :: Set Production,
      grammarStates :: Map (Set Item) GrammarState,
      grammarInitialStateMap :: Map Text (Set Item)
    }
instance Show Grammar where
  show grammar =
    let line text = T.concat ["  ", text, "\n"]
        section (header, lines) = T.concat $ [header, "\n"] ++ (map line lines)
        sections sections = T.intercalate "\n" $ map section sections
    in sections $ [("TERMINALS",
                    map show $ Set.toList $ grammarTerminals grammar),
                   ("NONTERMINALS",
                    map show $ Set.toList $ grammarNonterminals grammar),
                   ("PRODUCTIONS",
                    map show $ Set.toList $ grammarProductions grammar)]
                  ++ (zipWith (\index (_, state) ->
                                  (T.concat ["STATE ", show index],
                                   T.lines $ show state))
                              ([0 ..] :: [Int])
                              (Map.toList $ grammarStates grammar))


data TablifiedGrammar =
  TablifiedGrammar {
      tablifiedGrammarTokenInterpretations :: Token -> Set Word,
      tablifiedGrammarInitialStateMap :: Map Text Word,
      tablifiedGrammarGotoTable :: UArray (Word, Word) Word,
      tablifiedGrammarShiftTable :: UArray (Word, Word) Word,
      tablifiedGrammarReductionTable :: UArray (Word, Word) Word,
      tablifiedGrammarProductionRightHandSideLengths :: UArray Word Word,
      tablifiedGrammarReducers :: Array Word Dynamic,
      tablifiedGrammarOriginal :: Grammar,
      tablifiedGrammarOriginalSymbolMap :: Map Word GrammarSymbol,
      tablifiedGrammarOriginalProductionMap :: Map Word Production,
      tablifiedGrammarOriginalStateMap :: Map Word GrammarState
    }


wordingGrammarSpecification :: GrammarSpecification
wordingGrammarSpecification =
  let headingAssociations = [("CHAPTER", "chapter-heading-type"),
                             ("SECTION", "section-heading-type")]
      terminalAssociations = [(WordTokenType, "word"),
                              (NumberTokenType, "number"),
                              (StringTokenType, "string"),
                              (OperatorTokenType, "operator"),
                              (PeriodTokenType, "period"),
                              (EllipsisTokenType, "ellipsis"),
                              (HyphenTokenType, "hyphen"),
                              (DashTokenType, "dash"),
                              (CommaTokenType, "comma"),
                              (ColonTokenType, "colon"),
                              (SemicolonTokenType, "semicolon"),
                              (TickTokenType, "tick"),
                              (BacktickTokenType, "backtick"),
                              (SpliceTokenType, "splice"),
                              (ListSpliceTokenType, "list-splice"),
                              (OpenParenthesisTokenType, "open-parenthesis"),
                              (CloseParenthesisTokenType, "close-parenthesis"),
                              (SpaceTokenType, "space"),
                              (ParagraphBreakTokenType, "paragraph-break")]
      tokenInterpretations token = 
        Set.fromList
         $ concat [maybeToList $ lookup (tokenType token)
                                        terminalAssociations,
                   case tokenType token of
                     WordTokenType ->
                       maybeToList $ lookup (tokenText token)
                                            headingAssociations
                     _ -> []]
      terminals = Set.union (Set.fromList $ map snd terminalAssociations)
                            (Set.fromList $ map snd headingAssociations)
      startSymbols =
        Set.fromList ["toplevel", "heading", "paragraph", "sentential-form",
                      "phrase", "word-form"]
  in GrammarSpecification {
         grammarSpecificationTokenInterpretations = tokenInterpretations,
         grammarSpecificationTerminals = terminals,
         grammarSpecificationStartSymbols = startSymbols,
         grammarSpecificationProductions =
           Map.fromList
             [("toplevel",
               [(["paragraphs", "sections"],
                 toDyn $ \paragraphs sections ->
                   Toplevel {
                       toplevelIntroduction = paragraphs,
                       toplevelSections = sections
                     })]),
              ("sections",
               [([],
                 toDyn $ ([] :: [Section])),
                (["nonempty-sections"],
                 toDyn $ \sections -> sections :: [Section])]),
              ("nonempty-sections",
               [(["section"],
                 toDyn $ \section ->
                   [section :: Section]),
                (["nonempty-sections", "paragraph-break", "section"],
                 toDyn $ \sections break section ->
                   seq (break :: Token)
                       (sections ++ [section :: Section]))]),
              ("section",
               [(["heading", "paragraph-break", "paragraphs"],
                 toDyn $ \heading break paragraphs ->
                   seq (break :: Token)
                       (Section {
                            sectionHeading = heading,
                            sectionBody = paragraphs
                          }))]),
              ("heading",
               [(["heading-type", "phrase"],
                 toDyn $ \headingType phrase ->
                   Heading {
                       headingType = headingType,
                       headingPhrase = phrase
                     })]),
              ("heading-type",
               [(["chapter-heading-type"],
                 toDyn $ \token ->
                   ChapterHeadingType {
                       chapterHeadingTypeToken = token
                     }),
                (["section-heading-type"],
                 toDyn $ \token ->
                   SectionHeadingType {
                       sectionHeadingTypeToken = token
                     })]),
              ("paragraphs",
               [([],
                 toDyn $ ([] :: [Paragraph])),
                (["nonempty-paragraphs"],
                 toDyn $ \paragraphs ->
                   paragraphs :: [Paragraph])]),
              ("nonempty-paragraphs",
               [(["paragraph"],
                 toDyn $ \paragraph ->
                   [paragraph :: Paragraph]),
                (["nonempty-paragraphs", "paragraph-break", "paragraph"],
                 toDyn $ \paragraphs break paragraph ->
                   seq (break :: Token)
                       (paragraphs ++ [paragraph :: Paragraph]))]),
              ("paragraph",
               [(["sentential-forms"],
                 toDyn $ \sentences ->
                   Paragraph {
                       paragraphBody = sentences
                     })]),
              ("sentential-forms",
               [(["sentential-form"],
                 toDyn $ \sentence ->
                   [sentence :: SententialForm]),
                (["sentential-forms", "sentential-form"],
                 toDyn $ \sentences sentence ->
                   sentences ++ [sentence :: SententialForm])]),
              ("sentential-form",
               [(["sentence"],
                 toDyn $ \sentence ->
                   sentence :: SententialForm),
                (["open-parenthesis", "sentential-forms", "close-parenthesis"],
                 toDyn $ \open sentences close ->
                   SententialParenthetical {
                       sententialParentheticalBody = sentences,
                       sententialParentheticalOpenDelimiter = open,
                       sententialParentheticalCloseDelimiter = close
                     })]),
              ("sentence",
               [(["phrase", "period"],
                 toDyn $ \phrase period ->
                   Sentence {
                       sentenceBody = phrase,
                       sentenceCloseDelimiter = period
                     })]),
              ("phrase",
               [(["word-form"],
                 toDyn $ \word ->
                   Phrase {
                       phraseWords = [word],
                       phrasePunctuators = []
                     }),
                (["phrase", "word-form"],
                 toDyn $ \phrase word ->
                   Phrase {
                       phraseWords = phraseWords phrase ++ [word],
                       phrasePunctuators =
                         phrasePunctuators phrase ++ [Nothing]
                     }),
                (["phrase", "punctuator", "word-form"],
                 toDyn $ \phrase punctuator word ->
                   Phrase {
                       phraseWords = phraseWords phrase ++ [word],
                       phrasePunctuators =
                         phrasePunctuators phrase ++ [Just punctuator]
                     })]),
              ("word-form",
               [(["word"],
                 toDyn $ \word ->
                   Word {
                       wordToken = word
                     }),
                (["number"],
                 toDyn $ \number ->
                   Number {
                       numberToken = number
                     }),
                (["operator"],
                 toDyn $ \operator ->
                   Operator {
                       operatorToken = operator
                     }),
                (["string"],
                 toDyn $ \string ->
                   String {
                       stringToken = string
                     }),
                (["open-parenthesis", "phrase", "close-parenthesis"],
                 toDyn $ \open phrase close ->
                   WordParenthetical {
                       wordParentheticalBody = phrase,
                       wordParentheticalOpenDelimiter = open,
                       wordParentheticalCloseDelimiter = close
                     }),
                (["tick", "word-form"],
                 toDyn $ \tick word ->
                   QuotedWord {
                       quotedWordBody = word,
                       quotedWordOpenDelimiter = tick
                     }),
                (["backtick", "word-form"],
                 toDyn $ \backtick word ->
                   QuasiquotedWord {
                       quasiquotedWordBody = word,
                       quasiquotedWordOpenDelimiter = backtick
                     }),
                (["splice", "word-form"],
                 toDyn $ \splice word ->
                   SplicedWord {
                       splicedWordBody = word,
                       splicedWordOpenDelimiter = splice
                     }),
                (["list-splice", "word-form"],
                 toDyn $ \listSplice word ->
                   ListSplicedWord {
                       listSplicedWordBody = word,
                       listSplicedWordOpenDelimiter = listSplice
                     })]),
              ("punctuator",
               [(["hyphen"],
                 toDyn $ \hyphen ->
                   Hyphen {
                       hyphenToken = hyphen
                     }),
                (["dash"],
                 toDyn $ \dash ->
                   Dash {
                       dashToken = dash
                     }),
                (["comma"],
                 toDyn $ \comma ->
                   Comma {
                       commaToken = comma
                     }),
                (["colon"],
                 toDyn $ \colon ->
                   Colon {
                       colonToken = colon
                     }),
                (["semicolon"],
                 toDyn $ \semicolon ->
                   Semicolon {
                       semicolonToken = semicolon
                     })])]
       }


makeGrammar :: GrammarSpecification -> Maybe Grammar
makeGrammar grammarSpecification = do
  let tokenInterpretations = \token ->
        Set.map GrammarSymbol
                $ grammarSpecificationTokenInterpretations
                    grammarSpecification token
      terminals =
        Set.map GrammarSymbol
                $ grammarSpecificationTerminals grammarSpecification
      startSymbols =
        Set.map GrammarSymbol
                $ grammarSpecificationStartSymbols grammarSpecification
      productionSpecifications =
        grammarSpecificationProductions grammarSpecification
      nonterminalsDefined =
        Set.fromList $ map (GrammarSymbol . fst)
                           $ Map.toList productionSpecifications
      symbolsReferenced =
        foldl' Set.union
               Set.empty
               $ map Set.fromList
                     $ concat $ map (map (map GrammarSymbol . fst) . snd)
                                    $ Map.toList productionSpecifications
      nonterminalsReferenced =
        Set.difference symbolsReferenced terminals
      nonterminalsMissing =
        Set.difference nonterminalsReferenced nonterminalsDefined
      productions =
        Set.fromList $ concat $ map (\(leftHandSide, rightHandSides) ->
          map (\(rightHandSide, reducer) ->
                  Production {
                      productionLeftHandSide =
                        GrammarSymbol leftHandSide,
                      productionRightHandSide =
                        map GrammarSymbol rightHandSide,
                      productionReducer = reducer
                    })
              rightHandSides)
        $ Map.toList productionSpecifications
      ((), stateMap) = runState (computeStates startSymbols productions)
                                Map.empty
  if Set.null nonterminalsMissing
    then return ()
    else Nothing
  grammar <- return $ Grammar {
                          grammarTerminals = terminals,
                          grammarNonterminals = nonterminalsDefined,
                          grammarTokenInterpretations = tokenInterpretations,
                          grammarProductions = productions,
                          grammarStates = stateMap,
                          grammarInitialStateMap = Map.empty
                        }
  return grammar


computeStates
  :: Set GrammarSymbol
  -> Set Production
  -> State (Map (Set Item) GrammarState) ()
computeStates startSymbols productions = do
  let putState grammarState = do
        monadicState <- get
        let itemSet = grammarStateItems grammarState
            monadicState' = Map.insert itemSet grammarState monadicState
        put monadicState'
      getState itemSet = do
        monadicState <- get
        return $ Map.lookup itemSet monadicState
      initialQueue =
        map (\startSymbol ->
               let kernel =
                     Set.singleton
                      $ Item {
                            itemProduction =
                              StartProduction {
                                  startProductionSymbol = startSymbol
                                },
                            itemIndex = 0
                          }
               in closeItemSet productions kernel)
            (Set.toList startSymbols)
      processQueue [] = return ()
      processQueue (itemSet : rest) = do
        let state = GrammarState {
                        grammarStateItems = itemSet,
                        grammarStateGotoMap = Map.empty,
                        grammarStateShiftMap = Map.empty,
                        grammarStateReductions = Set.empty
                      }
        putState state
        processQueue rest
  processQueue initialQueue


closeItemSet :: Set Production -> Set Item -> Set Item
closeItemSet productions itemSet =
  let loop soFar [] = soFar
      loop soFar (item : rest) =
        let maybeNextSymbol =
              case itemProduction item of
                production@(Production { }) ->
                  let index = itemIndex item
                  in if index < (length $ productionRightHandSide production)
                       then Just $ productionRightHandSide production !! index
                       else Nothing
                production@(StartProduction { }) ->
                  if itemIndex item == 0
                    then Just $ startProductionSymbol production
                    else Nothing
            relevantProductions =
              Set.filter (\production ->
                            case (maybeNextSymbol, production) of
                              (Just nextSymbol, Production { }) ->
                                productionLeftHandSide production == nextSymbol
                              _ -> False)
                         productions
            foundItems =
              Set.map (\production -> Item {
                                          itemProduction = production,
                                          itemIndex = 0
                                        })
                      relevantProductions
            newQueue = rest ++ (Set.toList $ Set.difference foundItems soFar)
        in loop (Set.insert item $ Set.union soFar foundItems) newQueue
  in loop Set.empty (Set.toList itemSet)


tablifyGrammar :: Grammar -> TablifiedGrammar
tablifyGrammar grammar = undefined


runParser :: (Monad m) => Conduit Token m (Either Error Token)
runParser = do
  case makeGrammar wordingGrammarSpecification of
    Nothing -> return ()
    Just grammar -> do
      trace (T.unpack $ show $ grammar) $ return ()
      let loop = do
            maybeToken <- await
            case maybeToken of
              Nothing -> return ()
              Just token -> do
                yield $ Right token
                loop
      loop


{-
    'sentence': {
        reservedWordList: [
            'is', 'to', 'of', 'and',
        ],
        
        almostReservedWordList: [
            'type',
        ],
        
        tokensFromInputItem: function(inputItem) {
            var grammar = this;
            
            var results = [];
            
            if(inputItem.type == 'word') {
                results.push('noun');
                results.push('verb');
                
                var string = inputItem.string.toLowerCase();
                
                if(_.any(grammar.reservedWordList, function(foundWord) {
                    return string == foundWord;
                })) {
                    results = [string];
                } else if(_.any(grammar.almostReservedWordList,
                function(foundWord) {
                    return string == foundWord;
                })) {
                    results.push(string);
                }
            } else {
                results.push(inputItem.type);
            }
            
            return results;
        },
        
        terminals: [
            'noun', 'verb',
            'is', 'of', 'type', 'and',
            'number', 'operator', 'string', 'word-parenthetical',
            'quoted-word', 'quasiquoted-word', 'spliced-word',
            'list-spliced-word',
        ],
        
        startSymbols: ['sentence'],
        
        productions: {
            'sentence': [
                [['declarations'], function(rhs) {
                    return rhs[0];
                }],
            ],
            
            'declarations': [
                [['declaration'], function(rhs) {
                    return [rhs[0]];
                }],
                /*
                [['declarations', 'declaration'], function(rhs) {
                    return _.flatten([rhs[0], [rhs[1]]], true);
                }],
                */
            ],
            
            'declaration': [
                [['signature-declaration'], function(rhs) {
                    return rhs[0];
                }],
                [['value-declaration'], function(rhs) {
                    return rhs[0];
                }],
            ],
            
            'signature-declaration': [
                [['pattern', 'is', 'of', 'type', 'expression'],
                function(rhs) {
                    return {
                    type: 'signature-declaration',
                    words: _.flatten([rhs[0].words,
                                      [rhs[1], rhs[2], rhs[3]],
                                      rhs[4].words],
                                     true),
                    subject: rhs[0],
                    subjectType: rhs[4],
                    };
                }],
            ],
            
            'value-declaration': [
                [['pattern', 'is', 'expression'], function(rhs) {
                    return {
                    type: 'value-declaration',
                    words: _.flatten([rhs[0].words,
                                      [rhs[1]],
                                      rhs[2].words],
                                     true),
                    subject: rhs[0],
                    subjectValue: rhs[2],
                    };
                }],
            ],
            
            'pattern': [
                [['noun-phrase-head'], function(rhs) {
                    return {
                    type: 'noun-phrase-pattern',
                    words: rhs[0].words,
                    nounPhrase: rhs[0],
                    };
                }],
                [['number'], function(rhs) {
                    return {
                    type: 'number-pattern',
                    words: [rhs[0]],
                    number: rhs[0],
                    };
                }],
                [['string'], function(rhs) {
                    return {
                    type: 'string-pattern',
                    words: [rhs[0]],
                    string: rhs[0],
                    };
                }],
                [['pattern', 'nonempty-prepositional-pattern-phrases'],
                function(rhs) {
                    return {
                    type: 'application-pattern',
                    words: _.flatten([rhs[0].words,
                                      _.flatten(_.map(rhs[1], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    left: rhs[0],
                    prepositionalPhrases: rhs[1],
                    };
                }],
                [['to', 'verb-phrase-head', 'prepositional-pattern-phrases'],
                function(rhs) {
                    return {
                    type: 'intransitive-verb-phrase-pattern',
                    words: _.flatten([rhs[0].words,
                                      _.flatten(_.map(rhs[1], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    head: rhs[0],
                    prepositionalPhrases: rhs[1],
                    };
                }],
                [['to', 'verb-phrase-head', 'prepositional-pattern-phrases'],
                function(rhs) {
                    return {
                    type: 'intransitive-verb-phrase-pattern',
                    words: _.flatten([rhs[0].words,
                                      _.flatten(_.map(rhs[1], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    head: rhs[0],
                    prepositionalPhrases: rhs[1],
                    };
                }],
                [['to', 'verb-phrase-head', 'pattern',
                  'prepositional-pattern-phrases'],
                function(rhs) {
                    return {
                    type: 'transitive-verb-phrase-pattern',
                    words: _.flatten([rhs[0].words,
                                      rhs[1].words,
                                      _.flatten(_.map(rhs[2], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    head: rhs[0],
                    directObject: rhs[1],
                    prepositionalPhrases: rhs[2],
                    };
                }],
                [['to', 'verb-phrase-head', 'pattern', 'pattern',
                  'prepositional-pattern-phrases'],
                function(rhs) {
                    return {
                    type: 'doubly-transitive-verb-phrase-pattern',
                    words: _.flatten([rhs[0].words,
                                      rhs[1].words,
                                      rhs[2].words,
                                      _.flatten(_.map(rhs[3], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    head: rhs[0],
                    directObject: rhs[1],
                    indirectObject: rhs[2],
                    prepositionalPhrases: rhs[3],
                    };
                }],
                [['pattern', 'operator', 'pattern'], function(rhs) {
                    return {
                    type: 'binary-operator-pattern',
                    words: _.flatten([rhs[0].words,
                                      [rhs[1]],
                                      rhs[2].words],
                                     true),
                    operator: rhs[1],
                    left: rhs[0],
                    right: rhs[2],
                    };
                }],
                [['operator', 'pattern'], function(rhs) {
                    return {
                    type: 'prefix-operator-pattern',
                    words: _.flatten([[rhs[0]],
                                      rhs[1].words],
                                     true),
                    operator: rhs[0],
                    right: rhs[1],
                    };
                }],
                [['pattern', 'operator'], function(rhs) {
                    return {
                    type: 'postfix-operator-pattern',
                    words: _.flatten([rhs[0].words,
                                      [rhs[1]]],
                                     true),
                    operator: rhs[1],
                    left: rhs[0],
                    };
                }],
                [['and-list'], function(rhs) {
                    return {
                    type: 'list-pattern',
                    words: rhs[0].words,
                    items: rhs[0].items,
                    };
                }],
            ],
            
            'expression': [
                [['noun-phrase-head'], function(rhs) {
                    return {
                    type: 'noun-phrase-expression',
                    words: rhs[0].words,
                    nounPhrase: rhs[0],
                    };
                }],
                [['number'], function(rhs) {
                    return {
                    type: 'number-expression',
                    words: [rhs[0]],
                    number: rhs[0],
                    };
                }],
                [['string'], function(rhs) {
                    return {
                    type: 'string-expression',
                    words: [rhs[0]],
                    string: rhs[0],
                    };
                }],
                [['expression', 'nonempty-prepositional-expression-phrases'],
                function(rhs) {
                    return {
                    type: 'application-expression',
                    words: _.flatten([rhs[0].words,
                                      _.flatten(_.map(rhs[1], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    left: rhs[0],
                    prepositionalPhrases: rhs[1],
                    };
                }],
                [['verb-phrase-head', 'prepositional-expression-phrases'],
                function(rhs) {
                    return {
                    type: 'intransitive-verb-phrase-expression',
                    words: _.flatten([rhs[0].words,
                                      _.flatten(_.map(rhs[1], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    head: rhs[0],
                    prepositionalPhrases: rhs[1],
                    };
                }],
                [['verb-phrase-head', 'expression',
                  'prepositional-expression-phrases'],
                function(rhs) {
                    return {
                    type: 'transitive-verb-phrase-expression',
                    words: _.flatten([rhs[0].words,
                                      rhs[1].words,
                                      _.flatten(_.map(rhs[2], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    head: rhs[0],
                    directObject: rhs[1],
                    prepositionalPhrases: rhs[2],
                    };
                }],
                [['verb-phrase-head', 'expression', 'expression',
                  'prepositional-expression-phrases'],
                function(rhs) {
                    return {
                    type: 'doubly-transitive-verb-phrase-expression',
                    words: _.flatten([rhs[0].words,
                                      rhs[1].words,
                                      rhs[2].words,
                                      _.flatten(_.map(rhs[3], function(item) {
                                          return item.words;
                                      }), true)],
                                     true),
                    head: rhs[0],
                    directObject: rhs[1],
                    indirectObject: rhs[2],
                    prepositionalPhrases: rhs[3],
                    };
                }],
                [['expression', 'operator', 'expression'], function(rhs) {
                    return {
                    type: 'binary-operator-expression',
                    words: _.flatten([rhs[0].words,
                                      [rhs[1]],
                                      rhs[2].words],
                                     true),
                    operator: rhs[1],
                    left: rhs[0],
                    right: rhs[2],
                    };
                }],
                [['operator', 'expression'], function(rhs) {
                    return {
                    type: 'prefix-operator-expression',
                    words: _.flatten([[rhs[0]],
                                      rhs[1].words],
                                     true),
                    operator: rhs[0],
                    right: rhs[1],
                    };
                }],
                [['expression', 'operator'], function(rhs) {
                    return {
                    type: 'postfix-operator-expression',
                    words: _.flatten([rhs[0].words,
                                      [rhs[1]]],
                                     true),
                    operator: rhs[1],
                    left: rhs[0],
                    };
                }],
                [['and-list'], function(rhs) {
                    return {
                    type: 'list-expression',
                    words: rhs[0].words,
                    items: rhs[0].items,
                    };
                }],
            ],
            
            'noun-phrase-head': [
                [['noun'], function(rhs) {
                    return {
                    type: 'noun-phrase-head',
                    words: [rhs[0]],
                    };
                }],
                [['noun-phrase-head', 'noun'], function(rhs) {
                    return {
                    type: 'noun-phrase-head',
                    words: _.flatten([rhs[0].words, [rhs[1]]], true),
                    };
                }],
            ],
            
            'verb-phrase-head': [
                [['verb'], function(rhs) {
                    return {
                    type: 'verb-phrase-head',
                    words: [rhs[0]],
                    };
                }],
                [['verb-phrase-head', 'verb'], function(rhs) {
                    return {
                    type: 'verb-phrase-head',
                    words: _.flatten([rhs[0].words, [rhs[1]]], true),
                    };
                }],
            ],
            
            'prepositional-pattern-phrases': [
                [[], function(rhs) {
                    return [];
                }],
                [['prepositional-pattern-phrases',
                  'prepositional-pattern-phrase'],
                function(rhs) {
                    return _.flatten([rhs[0], [rhs[1]]], true);
                }],
            ],
            
            'nonempty-prepositional-pattern-phrases': [
                [['prepositional-pattern-phrase'], function(rhs) {
                    return [rhs[1]];
                }],
                [['nonempty-prepositional-pattern-phrases',
                  'prepositional-pattern-phrase'],
                function(rhs) {
                    return _.flatten([rhs[0], [rhs[1]]], true);
                }],
            ],
            
            'prepositional-pattern-phrase': [
                [['preposition', 'pattern'], function(rhs) {
                    return {
                    type: 'prepositional-pattern-phrase',
                    words: _.flatten([rhs[0].words, rhs[1].words], true),
                    preposition: rhs[0],
                    object: rhs[1],
                    };
                }],
            ],
            
            'prepositional-expression-phrases': [
                [[], function(rhs) {
                    return [];
                }],
                [['prepositional-expression-phrases',
                  'prepositional-expression-phrase'],
                function(rhs) {
                    return _.flatten([rhs[0], [rhs[1]]], true);
                }],
            ],
            
            'nonempty-prepositional-expression-phrases': [
                [['prepositional-expression-phrase'], function(rhs) {
                    return [rhs[1]];
                }],
                [['nonempty-prepositional-expression-phrases',
                  'prepositional-expression-phrase'],
                function(rhs) {
                    return _.flatten([rhs[0], [rhs[1]]], true);
                }],
            ],
            
            'prepositional-expression-phrase': [
                [['preposition', 'expression'], function(rhs) {
                    return {
                    type: 'prepositional-expression-phrase',
                    words: _.flatten([rhs[0].words, rhs[1].words], true),
                    preposition: rhs[0],
                    object: rhs[1],
                    };
                }],
            ],
            
            'preposition': [
            ],
            
            'and-list': [
                [['undelimited-list', 'and', 'expression'],
                function(rhs) {
                    return {
                    type: 'and-list',
                    words: _.flatten([rhs[0].words, [rhs[1]], rhs[2].words],
                                     true),
                    items: _.flatten([rhs[0].items, [rhs[2]]], true),
                    };
                }],
            ],
            
            'undelimited-list': [
                [['expression'], function(rhs) {
                    return {
                    type: 'undelimited-list',
                    words: rhs[0].words,
                    items: [rhs[0]],
                    };
                }],
                [['undelimited-list', 'expression'], function(rhs) {
                    return {
                    type: 'undelimited-list',
                    words: _.flatten([rhs[0].words, rhs[1].words], true),
                    items: _.flatten([rhs[0].items, [rhs[1]]], true),
                    };
                }],
            ],
        },
    },
-}


{-
_itemSetsEqual: function(itemSetA, itemSetB) {
    for(var i = 0; i < itemSetA.length; i++) {
        var found = false;
        for(var j = 0; j < itemSetB.length; j++) {
            if(this._itemsEqual(itemSetA[i], itemSetB[j])) {
                found = true;
                break;
            }
        }
        
        if(!found) return false;
    }
    
    for(var i = 0; i < itemSetB.length; i++) {
        var found = false;
        for(var j = 0; j < itemSetA.length; j++) {
            if(this._itemsEqual(itemSetB[i], itemSetA[j])) {
                found = true;
                break;
            }
        }
        
        if(!found) return false;
    }
    
    return true;
},

_itemsEqual: function(itemA, itemB) {
    var parser = this;
    
    if(!parser._productionsEqual(itemA.production, itemB.production))
        return false;
    if(itemA.position != itemB.position) return false;
    return true;
},

_productionsEqual: function(productionA, productionB) {
    var parser = this;
    
    if(!parser._symbolsEqual(productionA.lhs, productionB.lhs)) return false;
    if(productionA.rhs.length != productionB.rhs.length) return false;
    for(var i = 0; i < productionA.rhs.length; i++) {
        if(!parser._symbolsEqual(productionA.rhs[i], productionB.rhs[i]))
            return false;
    }
    return true;
},

_symbolsEqual: function(symbolA, symbolB) {
    return symbolA == symbolB;
},

_decode: function(grammar, stateCode, tokens) {
    var parser = this;
    
    var state = grammar._states[stateCode];
    
    var results = [];
    
    for(var i = 0; i < state.reductions.length; i++) {
        var production = state.reductions[i];
        
        if(_.isNull(production.lhs)) {
            results.push({
            type: 'accept',
            });
        } else {
            results.push({
            type: 'reduce',
            production: production,
            });
        }
    }
    
    for(var i = 0; i < tokens.length; i++) {
        var token = tokens[i];
        
        var shift = state.shiftMap[token];
        if(shift) {
            results.push({
            type: 'shift',
            state: shift,
            });
        }
    }
    
    if(results.length == 0) {
        var expected = [];
        _.each(state.gotoMap, function(shift, nonterminal) {
            expected.push(nonterminal);
        });
        _.each(state.shiftMap, function(shift, token) {
            expected.push(token);
        });
        
        var message;
        message = 'Unexpected ';
        if(inputItem) message += inputItem.type;
        else message += 'end';
        message += '; would have preferred ';
        if(expected.length == 0) {
            message += 'end.';
        } else if(expected.length == 1) {
            message += expected[0] + '.';
        } else if(expected.length == 2) {
            message += expected[0] + ' or ' + expected[1] + '.';
        } else {
            message += expected[0];
            for(var i = 1; i + 1 < expected.length; i++) {
                message += ', ' + expected[i];
            }
            message += ', or ' + expected[i] + '.';
        }
        
        results.push({
        type: 'error',
        message: message,
        expected: expected,
        actual: inputItem,
        });
    }
    
    return results;
},

prepare: function() {
    var parser = this;
    
    _.each(parser.grammars, function(grammar) {
        parser._prepareGrammar(grammar);
    });
},

_prepareGrammar: function(grammar) {
    var parser = this;
    
    _.bindAll(grammar, 'tokensFromInputItem');
    
    grammar._nonterminals = [];
    grammar._productions =
    _.flatten(_.map(grammar.productions, function(specification, lhs) {
        grammar._nonterminals.push(lhs);
        return _.map(specification, function(subspecification) {
            return {
            lhs: lhs,
            rhs: subspecification[0],
            action: subspecification[1],
            };
        });
    }), true);
    
    grammar._initialStateMap = {};
    grammar._states = [];
    for(var i = 0; i < grammar.startSymbols.length; i++) {
        var startSymbol = grammar.startSymbols[i];
        var startProduction = {
        lhs: null,
        rhs: [startSymbol],
        };
        var startItem = {
        production: startProduction,
        position: 0,
        };
        var startState = {
        items: parser._transitiveClosureOfItemSet(grammar, [startItem]),
        gotoMap: {},
        shiftMap: {},
        reductions: [],
        }
        grammar._states.push(startState);
        grammar._initialStateMap[startSymbol] = i;
    }
    
    for(var i = 0; i < grammar._states.length; i++) {
        var state = grammar._states[i];
        
        for(var j = 0; j < grammar._nonterminals.length; j++) {
            var nonterminal = grammar._nonterminals[j];
            var advancedItems = _.flatten(_.map(state.items, function(item) {
                if((item.position < item.production.rhs.length)
                   && (item.production.rhs[item.position] == nonterminal))
                {
                    return [{
                    production: item.production,
                    position: item.position + 1,
                    }];
                } else return [];
            }), true);
            advancedItems =
                parser._transitiveClosureOfItemSet(grammar, advancedItems);
            
            if(advancedItems.length == 0) continue;
            
            var foundStateIndex;
            for(var k = 0; k < grammar._states.length; k++) {
                var foundState = grammar._states[k];
                if(parser._itemSetsEqual(foundState.items, advancedItems)) {
                    foundStateIndex = k;
                    break;
                }
            }
            if(k == grammar._states.length) {
                grammar._states.push({
                items: advancedItems,
                gotoMap: {},
                shiftMap: {},
                reductions: [],
                });
                foundStateIndex = k;
            }
            
            state.gotoMap[nonterminal] = foundStateIndex;    
        }
        
        for(var j = 0; j < grammar.terminals.length; j++) {
            var token = grammar.terminals[j];
            var advancedItems = _.flatten(_.map(state.items, function(item) {
                if((item.position < item.production.rhs.length)
                   && (item.production.rhs[item.position] == token))
                {
                    return [{
                    production: item.production,
                    position: item.position + 1,
                    }];
                } else return [];
            }), true);
            advancedItems =
                parser._transitiveClosureOfItemSet(grammar, advancedItems);
            
            if(advancedItems.length == 0) continue;
            
            var foundStateIndex;
            for(var k = 0; k < grammar._states.length; k++) {
                var foundState = grammar._states[k];
                if(parser._itemSetsEqual(foundState.items, advancedItems)) {
                    foundStateIndex = k;
                    break;
                }
            }
            if(k == grammar._states.length) {
                grammar._states.push({
                items: advancedItems,
                gotoMap: {},
                shiftMap: {},
                reductions: [],
                });
                foundStateIndex = k;
            }
            
            state.shiftMap[token] = foundStateIndex;    
        }
        
        for(var k = 0; k < state.items.length; k++) {
            var item = state.items[k];
            if(item.position == item.production.rhs.length) {
                state.reductions.push(item.production);
            }
        }
    }
    
    /*
    console.log('productions:');
    console.log(util.inspect(grammar._productions, false, null, false));
    console.log('');
    console.log('initial state map:');
    console.log(util.inspect(grammar._initialStateMap, false, null, false));
    for(var i = 0; i < grammar._states.length; i++) {
        console.log('');
        console.log('state ' + i + ':');
        console.log(util.inspect(grammar._states[i], false, null, false));
    }
    */
},

_valuesFromState: function(depth, rhses, state, reduceAction) {
    var parser = this;
    
    if(depth == 0) {
        var results = [];
        for(var i = 0; i < rhses.length; i++) {
            results.push({
            statesBefore: [state],
            data: reduceAction(rhses[i]),
            });
        }
        
        return results;
    }
    
    var subresults = [];
    for(var i = 0; i < state.valuesBefore.length; i++) {
        var value = state.valuesBefore[i];
        
        subresults.push
            (parser._valuesFromValue(depth,
                                     rhses,
                                     value,
                                     reduceAction));
    }
    return _.flatten(subresults, true);
},

_valuesFromValue: function(depth, rhses, value, reduceAction) {
    var parser = this;
        
    var intermediate = [];
    for(var i = 0; i < rhses.length; i++) {
        intermediate.push(_.flatten([[value.data], rhses[i]], true));
    }
    
    var subresults = [];
    for(var i = 0; i < value.statesBefore.length; i++) {
        var state = value.statesBefore[i];
        
        subresults.push
            (parser._valuesFromState(depth - 1,
                                     intermediate,
                                     state,
                                     reduceAction));
    }
    return _.flatten(subresults, true);
},

_performReduction: function(grammar, parseState, action) {
    var parser = this;
    
    var production = action.production;
    
    var values = parser._valuesFromState
          (production.rhs.length,
           [[]],
           action.fromState,
           production.action);
    
    for(var k = 0; k < values.length; k++) {
        var value = values[k];
        
        var fromStates = value.statesBefore;
        
        for(var l = 0; l < fromStates.length; l++) {
            var fromState = fromStates[l];
            var fromStateImplementation =
                  grammar._states[fromState.stateCode];
            var newStateCode =
                  fromStateImplementation.gotoMap[production.lhs];
            
            var shift = {
            type: 'shift',
            state: newStateCode,
            fromState: fromState,
            data: value.data,
            };
            if(parseState.trace) shift.debug = production.lhs;
            
            if(parseState.trace) {
                var message = 'REDUCE ';
                message += 'state ' + fromState.stateCode + '; ';
                if(production.lhs) message += production.lhs;
                else message += '(start)';
                message += ' ->';
                for(var traceI = 0; traceI < production.rhs.length; traceI++) {
                    message += ' ' + production.rhs[traceI];
                }
                console.log(message); 
            }
            
            parser._performShift(grammar, parseState, shift);
        }
    }
},

_performShift: function(grammar, parseState, action) {
    var parser = this;
    
    var newValue = {
    statesBefore: [action.fromState],
    data: action.data,
    };
    
    var foundState;
    for(var j = 0; j < parseState.headStates.length; j++) {
        if(action.state == parseState.headStates[j].stateCode) {
            foundState = parseState.headStates[j];
            break;
        }
    }
    if(j == parseState.headStates.length) {
        parseState.headStates.push({
        stateCode: action.state,
        valuesBefore: [newValue],
        });
    } else {
        foundState.valuesBefore.push(newValue);
    }
    
    if(parseState.trace) {
        var message = 'SHIFT ' + action.debug;
        message += '; state ' + action.state;
        console.log(message);
    }                
},

parse: function(grammarName, startSymbol, input) {
    var parser = this;
    var grammar = parser.grammars[grammarName];
    var parses = [];
    var errors = [];
    
    var parseState = {
    trace: false,
    offset: 0,
    headStates: [{
    stateCode: grammar._initialStateMap[startSymbol],
    valuesBefore: [],
    }],
    pendingShifts: [],
    pendingAccepts: [],
    pendingErrors: [],
    headStateIndex: 0,
    };
    
    // if(grammarName == 'sentence') parseState.trace = true;
    
    if(parseState.trace) {
        console.log('START ' + grammar._initialStateMap[startSymbol]);
    }
    
    while(true) {
        if(parseState.offset < input.length)
            inputItem = input[parseState.offset];
        else inputItem = null;
        
        /*
        console.log(_.map(parseState.headStates, function(state) {
            return state.stateCode;
        }));
        */
        
        for(parseState.headStateIndex = 0;
            parseState.headStateIndex < parseState.headStates.length;
            parseState.headStateIndex++)
        {
            var state = parseState.headStates[parseState.headStateIndex];
            
            var token;
            if(!inputItem) tokens = [null];
            else tokens = grammar.tokensFromInputItem(inputItem);
            
            var actions = parser._decode(grammar, state.stateCode, tokens);
            
            for(var i = 0; i < actions.length; i++) {
                if(parseState.trace) {
                    console.log(_.map(parseState.headStates, function(item) {
                        return item.stateCode;
                    }));
                }
                
                var action = actions[i];
                
                if(action.type == 'shift') {
                    action.fromState = state;
                    action.data = inputItem;
                    if(parseState.trace) action.debug = tokens;
                    parseState.pendingShifts.push(action);
                } else if(action.type == 'accept') {
                    action.fromState = state;
                    action.values = state.valuesBefore;
                    parseState.pendingAccepts.push(action);
                } else if(action.type == 'error') {
                    parseState.pendingErrors.push(action);
                } else if(action.type == 'reduce') {
                    action.fromState = state;
                    parser._performReduction(grammar, parseState, action);
                }
            }
        }
        
        if(parseState.trace) {
            console.log('ADVANCING');
        }
        
        if(inputItem
           && (parseState.headStates.length > 0)
           && (parseState.pendingShifts.length > 0))
        {
            var shifts = parseState.pendingShifts;
            
            parseState.headStates = [];
            parseState.pendingShifts = [];
            parseState.pendingAccepts = [];
            parseState.pendingErrors = [];
            parseState.offset++;
            
            for(var i = 0; i < shifts.length; i++) {
                var action = shifts[i];
                
                parser._performShift(grammar, parseState, action);
            }
        } else if(!inputItem && (parseState.pendingAccepts.length > 0)) {
            if(parseState.trace) {
                console.log('ACCEPT');
            }
            
            for(var i = 0; i < parseState.pendingAccepts.length; i++) {
                var action = parseState.pendingAccepts[i];
                for(var j = 0; j < action.values.length; j++) {
                    parses.push(action.values[j].data);
                }
            }
            
            if(parses.length > 0) errors = [];
            
            break;
        } else {
            if(parseState.trace) {
                console.log('ERROR');
            }
            
            errors = parseState.pendingErrors;
            break;
        }
        
        if(parseState.trace) {
            console.log('ADVANCED');
        }
    }
    
    return {
    parses: parses,
    errors: errors,
    };
},

_callCombiningResult: function(result, item, action) {
    var subresult = action(item);
    _.extend(result.errors, subresult.errors);
    
    return subresult.parse;
},

_mapCombiningResult: function(result, list, action) {
    return _.map(list, function(item) {
        var subresult = action(item);
        _.extend(result.errors, subresult.errors);
        
        return subresult.parse;
    });
},

parseWording: function(input) {
    var parser = this;
    var result = parser.parse('wording', 'toplevel', input);
    return result;
},

parseToplevel: function(wording) {
    var parser = this;
    var result = {
    parse: null,
    errors: [],
    };
    var toplevel = {
    type: 'toplevel',
    };
    
    toplevel.introduction = _.flatten(parser._mapCombiningResult
    (result, wording.introduction, parser._parseParagraph), true);
    
    toplevel.sections = _.map(wording.sections, function(section) {
        section.body = _.flatten(parser._mapCombiningResult
        (result, section.body, parser._parseParagraph), true);
        
        return section;
    });
    
    if(result.errors.length == 0) result.parse = toplevel;
    
    return result;
},

_parseParagraph: function(paragraph) {
    var parser = this;
    var result = {
    parse: [],
    errors: [],
    };
    
    var declarations;
    declarations = _.flatten(parser._mapCombiningResult
    (result, paragraph.body, parser._parseSententialForm), true);
    
    result.parse = _.flatten(declarations, true);
    
    return result;
},

_parseSententialForm: function(sententialForm) {
    var parser = this;
    
    var result = {
    parse: null,
    errors: [],
    };
    
    var declarations;
    if(sententialForm.type == 'sentence') {
        declarations = [parser._callCombiningResult
        (result, sententialForm.body, parser._parseSentence)];
    } else if(sententialForm.type == 'sentential-parenthetical') {
        declarations = _.flatten(parser._mapCombiningResult
        (result, sententialForm.body, parser._parseSententialForm), true);
    }
    
    result.parse = _.flatten(declarations, true);
    
    return result;
},

_parseSentence: function(body) {
    var parser = this;
    
    var result = {
    parse: null,
    errors: [],
    };
    
    var subresult = parser.parse('sentence', 'sentence', body.words);
    
    if(subresult.parses.length > 1) {
        result.errors.push({
        type: 'error',
        message: 'Multiple parses.',
        parses: subresult.parses,
        });
    } else if(subresult.parses.length == 1) {
        result.parse = subresult.parses[0];
        result.errors = subresult.errors;
    } else {
        result.errors = subresult.errors;
    }
    
    return result;
},

});

module.exports = Parser;
-}
