innsbruck
=========

    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    runhaskell test/TestMain.hs
    echo "Do stuff..."
    cabal sandbox delete

