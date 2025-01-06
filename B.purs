module B where

a =
  ( NonEmptyArray
      [ (BreedLeaf (Id (NonEmptyArray [ "affenpinscher" ])) unit)
      , ( BreedNode (Id (NonEmptyArray [ "australian" ])) unit
            ( NonEmptyArray
                [ ( BreedLeaf (Id (NonEmptyArray [ "australian", "kelpie" ]))
                      unit
                  )
                , ( BreedNode (Id (NonEmptyArray [ "australian", "shepherd" ]))
                      unit
                      ( NonEmptyArray
                          [ ( BreedLeaf
                                ( Id
                                    ( NonEmptyArray
                                        [ "australian", "shepherd", "mini" ]
                                    )
                                )
                                unit
                            )
                          ]
                      )
                  )
                ]
            )
        )
      , ( BreedNode (Id (NonEmptyArray [ "bakharwal" ])) unit
            ( NonEmptyArray
                [ ( BreedLeaf (Id (NonEmptyArray [ "bakharwal", "indian" ]))
                      unit
                  )
                ]
            )
        )
      , (BreedLeaf (Id (NonEmptyArray [ "briard" ])) unit)
      , ( BreedNode (Id (NonEmptyArray [ "buhund" ])) unit
            ( NonEmptyArray
                [ ( BreedLeaf (Id (NonEmptyArray [ "buhund", "norwegian" ]))
                      unit
                  )
                ]
            )
        )
      ]
  )
