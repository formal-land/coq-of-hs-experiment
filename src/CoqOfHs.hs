module CoqOfHs (plugin) where

import Control.Concurrent (putMVar)
import Control.Exception (bracket)
import qualified Control.Monad
import GhcPlugins hiding (brackets, nest, parens, sep, space, (<+>), (<>))
import Prettyprinter
import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.FilePath (takeDirectory)
import TyCoRep

plugin :: Plugin
plugin =
  defaultPlugin
    { installCoreToDos = install
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (CoreDoPluginPass "Translate to Coq" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  dflags <- getDynFlags
  putMsgS "Translation to Coq"
  putMsgS $ moduleNameString $ moduleName $ mg_module guts
  putMsgS $ show $ mg_hsc_src guts
  putMsgS $ show $ mg_loc guts
  let fileName = case mg_loc guts of
        UnhelpfulSpan loc -> show loc
        _ -> "unknown.hs"
  let fileNameWithoutExtension = take (length fileName - 5) $ drop 1 fileName
  let outputDir = "coq_translation"
  let outputFileName = outputDir ++ "/" ++ fileNameWithoutExtension ++ ".v"
  putMsgS $ "File name: " ++ fileName
  putMsgS $ "Output file name: " ++ outputFileName
  liftIO $ ensureDirectoryExists outputFileName
  liftIO $ writeFile outputFileName header
  liftIO $ appendFile outputFileName $ show $ bindsToCoq dflags $ mg_binds guts
  return guts

-- | Ensures that the directory for the given file path exists.
ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists filePath = do
  let dirPath = takeDirectory filePath
  dirExists <- doesPathExist dirPath
  createDirectoryIfMissing True dirPath

header :: String
header =
  unlines
    [ "Require Export Coq.Strings.Ascii.",
      "Require Import Coq.Strings.String.",
      "Require Import Coq.ZArith.ZArith.",
      "",
      "Local Open Scope list_scope.",
      "Local Open Scope string_scope.",
      "Local Open Scope Z_scope.",
      "",
      "Import List.ListNotations.",
      "",
      "Parameter Rational : Set.",
      "",
      "Module Lit.",
      "  Inductive t : Set :=",
      "  | Char (_ : ascii)",
      "  | Number (_ : Z)",
      "  | String (_ : string)",
      "  | NullAddr",
      "  | Rubbish",
      "  | Float (_ : Rational)",
      "  | Double (_ : Rational)",
      "  | Label (_ : string).",
      "End Lit.",
      "",
      "Module Case.",
      "  Inductive t (A : Set) : Set :=",
      "  | Con (_ : string) (_ : list A -> A)",
      "  | Lit (_ : Lit.t) (_ : A)",
      "  | Default (_ : A).",
      "  Arguments Con {A}.",
      "  Arguments Lit {A}.",
      "  Arguments Default {A}.",
      "End Case.",
      "",
      "Module Val.",
      "  #[bypass_check(positivity)]",
      "  CoInductive t : Set :=",
      "  | Lit (_ : Lit.t)",
      "  | Con (_ : string) (_ : list t)",
      "  | App (_ _ : t)",
      "  | Lam (_ : t -> t)",
      "  | Case (_ : t) (_ : t -> list (Case.t t))",
      "  | Impossible.",
      "End Val.",
      "",
      "Definition Isharp' : Val.t :=",
      "  Val.Lam (fun z => Val.Con \"I#\" [z]).",
      "",
      "Definition lbracket'rbracket' : Val.t :=",
      "  Val.Con \"[]\" [].",
      "",
      "Definition colon' : Val.t :=",
      "  Val.Lam (fun x => Val.Lam (fun xs => Val.Con \":\" [x; xs])).",
      "",
      "Definition build : Val.t :=",
      "  Val.Lam (fun l => Val.App (Val.App l colon') lbracket'rbracket').",
      "",
      "Parameter plus'plus' : Val.t.",
      "",
      "Parameter _'Module : Val.t.",
      "",
      "Parameter TrNameS : Val.t.",
      "",
      "Parameter eq'eq' : Val.t.",
      "",
      "Parameter dollar'fEqNatural : Val.t.",
      "",
      "Parameter fromInteger : Val.t.",
      "",
      "Parameter dollar'fNumNatural : Val.t.",
      "",
      "Parameter minus' : Val.t.",
      "",
      "Parameter _'False : Val.t.",
      "",
      "Parameter _'True : Val.t.",
      "",
      "Parameter _'return : Val.t.",
      "",
      "Parameter dollar'fMonadIO : Val.t.",
      "",
      "Parameter lparen'rparen' : Val.t.",
      "",
      "Parameter runMainIO : Val.t.",
      ""
    ]

escapeCoqName :: String -> String
escapeCoqName name = do
  let name' = foldr (\(from, to) -> charReplace from (to ++ "'")) name charReplacements
  foldr fullEscape name' fullEscapes
  where
    charReplace :: Char -> String -> String -> String
    charReplace from to = concatMap (\c -> if c == from then to else [c])

    charReplacements :: [(Char, String)]
    charReplacements =
      [ ('#', "sharp"),
        ('+', "plus"),
        ('-', "minus"),
        ('*', "times"),
        ('/', "div"),
        ('%', "mod"),
        ('=', "eq"),
        ('<', "lt"),
        ('>', "gt"),
        ('!', "bang"),
        ('?', "question"),
        ('~', "tilde"),
        ('@', "at"),
        ('$', "dollar"),
        ('&', "and"),
        ('|', "or"),
        ('^', "caret"),
        ('[', "lbracket"),
        (']', "rbracket"),
        ('{', "lbrace"),
        ('}', "rbrace"),
        ('(', "lparen"),
        (')', "rparen"),
        ('.', "dot"),
        (',', "comma"),
        (';', "semicolon"),
        (':', "colon"),
        ('\'', "prime")
      ]

    fullEscape :: String -> String -> String
    fullEscape from name = if name == from then "_'" ++ name else name

    fullEscapes :: [String]
    fullEscapes =
      [ "False",
        "Module",
        "True",
        "return"
      ]

concatGroup :: [Doc ()] -> Doc ()
concatGroup = group . vsep

concatNest :: [Doc ()] -> Doc ()
concatNest = group . nest 2 . vsep

varToCoq :: DynFlags -> Var -> Doc ()
varToCoq dflags var = pretty $ escapeCoqName $ showSDoc dflags $ ppr var

tyConToCoq :: DynFlags -> TyCon -> Doc ()
tyConToCoq dflags tyCon = pretty $ escapeCoqName $ showSDoc dflags $ ppr tyCon

varBndrToCoq :: DynFlags -> VarBndr Var ArgFlag -> Doc ()
varBndrToCoq dflags varBndr = varToCoq dflags $ binderVar varBndr

tyLitToCoq :: DynFlags -> TyLit -> Doc ()
tyLitToCoq dflags tyLit =
  case tyLit of
    NumTyLit n -> pretty $ show n
    StrTyLit s -> pretty $ show s

coercionToCoq :: DynFlags -> Coercion -> Doc ()
coercionToCoq _ _ = pretty "coercion"

tyToCoq :: DynFlags -> Type -> Doc ()
tyToCoq dflags ty = pretty $ showSDoc dflags $ ppr ty

litToCoq :: DynFlags -> Literal -> Doc ()
litToCoq dflags lit =
  parens $
    pretty "Val.Lit"
      <+> parens
        ( case lit of
            LitChar c -> pretty "Lit.Char" <+> pretty c
            LitNumber _ n _ -> pretty "Lit.Number" <+> pretty n
            LitString s -> pretty "Lit.String" <+> pretty (show s)
            LitNullAddr -> pretty "Lit.NullAddr"
            LitRubbish -> pretty "Lit.Rubbish"
            LitFloat r -> pretty "Lit.Float" <+> pretty (show r)
            LitDouble r -> pretty "Lit.Double" <+> pretty (show r)
            LitLabel s _ _ -> pretty "Lit.Label" <+> pretty (show s)
        )

type Counter = Int

matchListOfKnownSizeToCoq :: DynFlags -> [Var] -> Doc () -> Doc ()
matchListOfKnownSizeToCoq dflags vars expr =
  parens $
    concatGroup
      [ concatGroup
          [ concatNest
              [ pretty "match",
                pretty "α"
              ],
            pretty "with"
          ],
        concatNest
          [ pretty "| ["
              <> concatNest
                ( map
                    (\var -> varToCoq dflags var <> pretty ";")
                    vars
                )
              <> pretty "] =>",
            expr
          ],
        pretty "| _ => Val.Impossible",
        pretty "end"
      ]

exprToCoq :: DynFlags -> CoreExpr -> Doc ()
exprToCoq dflags expr =
  case expr of
    Var name -> varToCoq dflags name
    Lit lit -> litToCoq dflags lit
    App e1 e2 | isTypeArg e2 -> exprToCoq dflags e1
    App e1 e2 ->
      parens $
        concatNest
          [ pretty "Val.App",
            exprToCoq dflags e1,
            exprToCoq dflags e2
          ]
    Lam var expr | isTyVar var -> exprToCoq dflags expr
    Lam var expr ->
      parens $
        concatNest
          [ pretty "Val.Lam",
            parens $
              concatNest
                [ concatNest
                    [ pretty "fun",
                      concatNest
                        [ pretty "("
                            <> varToCoq dflags var
                            <> pretty " :",
                          pretty "Val.t) =>"
                        ]
                    ],
                  exprToCoq dflags expr
                ]
          ]
    Let bind expr2 -> do
      let isRecursive = case bind of
            NonRec _ _ -> False
            Rec _ -> True
      let binds = case bind of
            NonRec var expr1 -> [(var, expr1)]
            Rec binds -> binds
      case binds of
        [(var, expr1)] ->
          concatGroup
            [ concatNest
                [ concatNest
                    [ pretty (if isRecursive then "let cofix" else "let"),
                      varToCoq dflags var <> pretty " :="
                    ],
                  exprToCoq dflags expr1 <> pretty " in"
                ],
              exprToCoq dflags expr2
            ]
        _ -> pretty "TODO-mutual-lets"
    Case expr name _ alts ->
      parens $
        concatNest
          [ pretty "Val.Case",
            exprToCoq dflags expr,
            parens
              ( concatNest
                  [ concatNest
                      [ pretty "fun",
                        varToCoq dflags name
                          <> pretty " =>"
                      ],
                    concatGroup
                      [ concatNest
                          ( pretty "[" :
                            zipWith
                              ( \index (altCon, altVars, altExpr) ->
                                  case altCon of
                                    DataAlt dataCon ->
                                      concatNest
                                        [ pretty "Case.Con",
                                          pretty "\""
                                            <> pretty (showSDoc dflags $ ppr dataCon)
                                            <> pretty "\"",
                                          parens
                                            ( concatNest
                                                [ pretty "fun α =>",
                                                  matchListOfKnownSizeToCoq
                                                    dflags
                                                    altVars
                                                    (exprToCoq dflags altExpr)
                                                ]
                                            )
                                        ]
                                    LitAlt lit ->
                                      concatNest
                                        [ pretty "Case.Lit",
                                          litToCoq dflags lit,
                                          exprToCoq dflags altExpr
                                        ]
                                    DEFAULT ->
                                      concatNest
                                        [ pretty "Case.Default",
                                          exprToCoq dflags altExpr
                                        ]
                                    <> if index == length alts - 1
                                      then emptyDoc
                                      else pretty ";"
                              )
                              [0 ..]
                              alts
                          ),
                        pretty "]"
                      ]
                  ]
              )
          ]
    Cast expr _ -> exprToCoq dflags expr
    Tick _ expr -> exprToCoq dflags expr
    -- We should always avoid this case
    Type ty -> tyToCoq dflags ty
    Coercion coercion -> coercionToCoq dflags coercion

bindToCoq :: DynFlags -> CoreBind -> Doc ()
bindToCoq dflags bind =
  case bind of
    NonRec var expr ->
      concatNest
        [ concatNest
            [ pretty "Definition",
              varToCoq dflags var,
              pretty ":",
              pretty "Val.t",
              pretty ":="
            ],
          exprToCoq dflags expr
        ]
        <> pretty "."
        <> hardline
    Rec bindings ->
      concatGroup
        ( zipWith
            ( \index (var, expr) -> do
                let header =
                      if index == 0
                        then pretty "CoFixpoint"
                        else pretty "with"
                let final_dot =
                      if index == length bindings - 1
                        then pretty "."
                        else emptyDoc
                concatNest
                  [ concatNest
                      [ header,
                        varToCoq dflags var,
                        pretty ":",
                        pretty "Val.t",
                        pretty ":="
                      ],
                    exprToCoq dflags expr
                  ]
                  <> final_dot
                  <> hardline
            )
            [0 ..]
            bindings
        )

bindsToCoq :: DynFlags -> [CoreBind] -> Doc ()
bindsToCoq dflags binds =
  concatGroup $
    map (bindToCoq dflags) binds
