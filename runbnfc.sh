set +e
set +x

cd src
stack exec bnfc grammar.cf
cd ../
git checkout src/ErrM.hs
rm src/*.bak
rm src/TestGrammar.hs
rm src/SkelGrammar.hs
ed -s src/PrintGrammar.hs << 'EOF'
0a
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
.
w
EOF
