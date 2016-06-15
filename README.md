innsbruck
=========

    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    ./dist/build/test/test
    echo "Do stuff..."
    cabal sandbox delete

