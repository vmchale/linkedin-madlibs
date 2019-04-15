#!/usr/bin/env cabal
{- cabal:
build-depends: base, shake, shake-cabal, shake-google-closure-compiler, shake-ext, directory, strict
default-language: Haskell2010
ghc-options: -Wall
-}

import           Development.Shake
import           Development.Shake.Cabal
import           Development.Shake.ClosureCompiler
import           Development.Shake.FileDetect
import           System.Directory
import qualified System.IO.Strict                  as Strict

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake", shakeLint = Just LintBasic } $ do
    want [ "target/index.html", "README.md" ]

    "sample" ~>
        cmd ["madlang", "run", "mad-src/linkedin-madlibs.mad", "-r57"]

    "clean" ~> do
        unit $ cmd ["rm", "-rf", "tags", "mad-src/tags", "build"]
        removeFilesAfter "target" ["//*"]
        removeFilesAfter "dist" ["//*"]
        removeFilesAfter "dist-newstyle" ["//*"]
        removeFilesAfter ".shake" ["//*"]

    "README.md" %> \out -> do
        hs <- getHs ["src"]
        yaml <- getYml
        cabal' <- getDirectoryFiles "" ["//*.cabal"]
        mad <- getMadlang
        html <- getDirectoryFiles "" ["web-src//*.html"]
        css <- getDirectoryFiles "" ["web-src//*.css"]
        need $ hs <> yaml <> cabal' <> mad <> html <> css
        (Stdout out') <- cmd ["poly", "-c", ".", "-e", "README.md", "-e", "TODO.md", "-e", "target", "-e", "Justfile", "-e", "CONTRIBUTING.md"]
        file <- liftIO $ Strict.readFile "README.md"
        let header = takeWhile (/= replicate 79 '-') $ lines file
        let new = unlines header ++ out' ++ "```\n"
        liftIO $ writeFile out new

    [ "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/opt/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js", "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/opt/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js.externs" ] &%> \_ -> do
        need . snd =<< getCabalDepsA "linkedin-madlibs.cabal"
        cmd ["cabal", "new-build", "--ghcjs"]

    googleClosureCompiler [ "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/opt/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js", "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/opt/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js.externs" ] "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/opt/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js"

    "target/all.min.js" %> \out -> do
        need ["dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/opt/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js"]
        copyFile' "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/opt/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js" out

    "target/styles.css" %> \out -> do
        liftIO $ createDirectoryIfMissing True "target"
        need ["web-src/styles.css"]
        copyFile' "web-src/styles.css" out

    "target/index.html" %> \out -> do
        need ["target/all.min.js", "target/styles.css"]
        copyFile' "web-src/index.html" out
