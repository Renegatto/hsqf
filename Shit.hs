module Shit where

check :: Value -> Maybe String
check (normalizeValue -> x) =
  let checked =
        renderErrors $
          runChecker @String
            (checkNormalized @MintingBuilder)
            (mint x)
      msg = "False: " ++ show x ++ " \n " ++ checked
   in if checked == mempty
        then Nothing
        else Just msg


q =
    ScriptContext
        { scriptContextTxInfo =
            TxInfo
                { txInfoInputs =
                    [ TxInInfo
                        { txInInfoOutRef = TxOutRef{txOutRefId = a0, txOutRefIdx = 0}
                        , txInInfoResolved =
                            TxOut
                                { txOutAddress =
                                    Address
                                        { addressCredential = ScriptCredential 6 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a
                                        , addressStakingCredential = Nothing
                                        }
                                , txOutValue =
                                    Value
                                        ( Map
                                            [ ("", Map [("", 2000000)])
                                            , (1865710 ee3b580f02787f3e5830f78ef6e23ed75cf53c304dfce562a, Map [("NFT", 1)])
                                            , (8 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Map [("AuctionEscrow", 1)])
                                            , (9 b7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Map [("FCN", 1)])
                                            ]
                                        )
                                , txOutDatum =
                                    OutputDatum
                                        ( Datum
                                            { getDatum =
                                                Constr
                                                    0
                                                    [ Constr
                                                        0
                                                        [ Map [(B "\CANeq\SO\227\181\128\240'\135\243\229\131\SIx\239n#\237u\207S\195\EOT\223\206V*", Map [(B "NFT", I 1)])]
                                                        , Constr
                                                            0
                                                            [ Constr 0 [Constr 0 [B "|{\250k\136\143\179\230\NUL\212\217P[O\188\169\ENQ\223\138\197\142\214#\183\ETB\n\177*"], Constr 1 []]
                                                            , Map [(Constr 0 [Constr 0 [B "|{\250k\136\143\179\230\NUL\212\217P[O\188\169\ENQ\223\138\197\142\214#\183\ETB\n\177*"], Constr 1 []], I 50), (Constr 0 [Constr 0 [B "\156{\250k\136\143\179\230\NUL\212\217P[O\188\169\ENQ\223\138\197\142\214#\183\ETB\n\177*"], Constr 1 []], I 50)]
                                                            , Constr 1 []
                                                            ]
                                                        , Constr 0 [I 200, I 10, I 20, I 20]
                                                        , Constr 0 [I 1000000, I 2000000, Constr 0 [I 1000, I 2000]]
                                                        ]
                                                    , B "\155{\250k\136\143\179\230\NUL\212\217P[O\188\169\ENQ\223\138\197\142\214#\183\ETB\n\177*"
                                                    ]
                                            }
                                        )
                                , txOutReferenceScript = Nothing
                                }
                        }
                    , TxInInfo
                        { txInInfoOutRef = TxOutRef{txOutRefId = a0, txOutRefIdx = 0}
                        , txInInfoResolved =
                            TxOut
                                { txOutAddress = Address{addressCredential = ScriptCredential 6 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, addressStakingCredential = Nothing}
                                , txOutValue = Value (Map [("", Map [("", 2000200)]), (8 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Map [("BidEscrow", 1)])])
                                , txOutDatum = OutputDatum (Datum{getDatum = Constr 0 [Constr 1 [I 200, I 1500000], Constr 0 [Constr 0 [B "|{\250k\153\143\179\230\NUL\212\217PSO\188\169\ENQ\221J\197\142\214#\183\ETB\n\177*"], Constr 1 []]]})
                                , txOutReferenceScript = Nothing
                                }
                        }
                    ]
                , txInfoReferenceInputs =
                    [ TxInInfo
                        { txInInfoOutRef = TxOutRef{txOutRefId = "", txOutRefIdx = 0}
                        , txInInfoResolved =
                            TxOut
                                { txOutAddress =
                                    Address
                                        { addressCredential = ScriptCredential 7 c7bfa6b888fb3e600d4d9505b4fbca905d23ac23ed623b7170ab12a
                                        , addressStakingCredential = Nothing
                                        }
                                , txOutValue = Value (Map [("", Map [("", 2000000)]), (9 b8bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Map [("MarketEscrow", 1)])])
                                , txOutDatum = OutputDatum (Datum{getDatum = Constr 0 [I 2, I 0, Constr 0 [Constr 0 [B "\173\253\135\&1\155\208\156\158>\161\v%\FS\203\EOTo\135\197D\ETXC\NAK~4\140:\199\193"], Constr 1 []], I 2]})
                                , txOutReferenceScript = Nothing
                                }
                        }
                    ]
                , txInfoOutputs =
                    [ TxOut
                        { txOutAddress = Address{addressCredential = ScriptCredential 6 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, addressStakingCredential = Nothing}
                        , txOutValue =
                            Value
                                ( Map
                                    [ (,Map [("", 4000000)])
                                    ,
                                        ( 8 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a
                                        , Map [("AuctionEscrow", 1), ("BoughtLot", 1)]
                                        )
                                    , (9 b7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Map [("FCN", 1)])
                                    ]
                                )
                        , txOutDatum =
                            OutputDatum
                                ( Datum
                                    { getDatum =
                                        Constr
                                            0
                                            [ Constr
                                                0
                                                [ Map [(B "\CANeq\SO\227\181\128\240'\135\243\229\131\SIx\239n#\237u\207S\195\EOT\223\206V*", Map [(B "NFT", I 1)])]
                                                , Constr
                                                    0
                                                    [ Constr 0 [Constr 0 [B "|{\250k\136\143\179\230\NUL\212\217P[O\188\169\ENQ\223\138\197\142\214#\183\ETB\n\177*"], Constr 1 []]
                                                    , Map [(Constr 0 [Constr 0 [B "|{\250k\136\143\179\230\NUL\212\217P[O\188\169\ENQ\223\138\197\142\214#\183\ETB\n\177*"], Constr 1 []], I 50), (Constr 0 [Constr 0 [B "\156{\250k\136\143\179\230\NUL\212\217P[O\188\169\ENQ\223\138\197\142\214#\183\ETB\n\177*"], Constr 1 []], I 50)]
                                                    , Constr 1 []
                                                    ]
                                                , Constr 0 [I 200, I 10, I 20, I 20]
                                                , Constr 0 [I 1000000, I 2000000, Constr 0 [I 1000, I 2000]]
                                                ]
                                            , B "\155{\250k\136\143\179\230\NUL\212\217P[O\188\169\ENQ\223\138\197\142\214#\183\ETB\n\177*"
                                            ]
                                    }
                                )
                        , txOutReferenceScript = Nothing
                        }
                    , TxOut
                        { txOutAddress = Address{addressCredential = PubKeyCredential 7 c7bfa6b998fb3e600d4d950534fbca905dd4ac58ed623b7170ab12a, addressStakingCredential = Nothing}
                        , txOutValue = Value (Map [(,Map [("", 2000000)]), (1865710 ee3b580f02787f3e5830f78ef6e23ed75cf53c304dfce562a, Map [("NFT", 1)])])
                        , txOutDatum = NoOutputDatum
                        , txOutReferenceScript = Nothing
                        }
                    , TxOut
                        { txOutAddress = Address{addressCredential = PubKeyCredential 7 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, addressStakingCredential = Nothing}
                        , txOutValue = Value (Map [(,Map [("", 101000000)])])
                        , txOutDatum = NoOutputDatum
                        , txOutReferenceScript = Nothing
                        }
                    , TxOut
                        { txOutAddress = Address{addressCredential = PubKeyCredential 9 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, addressStakingCredential = Nothing}
                        , txOutValue = Value (Map [(,Map [("", 101000000)])])
                        , txOutDatum = NoOutputDatum
                        , txOutReferenceScript = Nothing
                        }
                    , TxOut
                        { txOutAddress = Address{addressCredential = PubKeyCredential adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7c1, addressStakingCredential = Nothing}
                        , txOutValue = Value (Map [(,Map [("", 2000000)])])
                        , txOutDatum = NoOutputDatum
                        , txOutReferenceScript = Nothing
                        }
                    ]
                , txInfoFee = Value (Map [])
                , txInfoMint =
                    Value
                        ( Map
                            [
                                ( 8 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a
                                , Map [("BidEscrow", -1), ("BoughtLot", 1)]
                                )
                            , (9 b7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Map [(0x46534e7c7bfa6b998fb3e600d4d950534fbca905dd4ac58ed623b7170ab12a, -1)])
                            ]
                        )
                , txInfoDCert = []
                , txInfoWdrl = Map{unMap = []}
                , txInfoValidRange = Interval{ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                , txInfoSignatories = [7 c7bfa6b998fb3e600d4d950534fbca905dd4ac58ed623b7170ab12a]
                , txInfoRedeemers = Map{unMap = [(Minting 8 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Redeemer{getRedeemer = Constr 0 []}), (Minting 9 b7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Redeemer{getRedeemer = Constr 0 []}), (Minting 8 c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a, Redeemer{getRedeemer = Constr 0 []})]}
                , txInfoData = Map{unMap = []}
                , txInfoId = ""
                }
        , scriptContextPurpose = Spending (TxOutRef{txOutRefId = a0, txOutRefIdx = 0})
        }
