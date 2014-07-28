#!/bin/bash

current=$(pwd)
cargo build

dir=$(mktemp -d)
echo $dir
mkdir $dir/solution
mkdir $dir/code
cp -R . -t $dir/code
rm -rf $dir/code/target
rm -rf $dir/code/.git
rm -rf $dir/code/submission.tar.gz
(
    cd ai
    ../target/icfp-2014 rating-bot.lisp > $dir/solution/lambdaman.gcc
)
(
    cd ghost
    ../target/icfp-2014 --ghost manhattan.asm > $dir/solution/ghost0.ghc
)

cd $dir

ls -aR

tar czf $current/submission.tar.gz *
