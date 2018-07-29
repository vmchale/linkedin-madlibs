import           Development.Shake
import           Development.Shake.ClosureCompiler
import           System.Directory
import qualified System.IO.Strict                  as Strict

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake", shakeLint = Just LintBasic } $ do
    want [ "target/index.html", "README.md" ]

    "sample" ~>
        cmd ["madlang", "run", "mad-src/linkedin-madlibs.mad", "-r57"]

    "clean" ~> do
        putNormal "cleaning files..."
        unit $ cmd ["rm", "-rf", "tags"]
        removeFilesAfter "target" ["//*"]
        removeFilesAfter "dist" ["//*"]
        removeFilesAfter "dist-newstyle" ["//*"]

    "README.md" %> \out -> do
        hs <- getDirectoryFiles "" ["src//*.hs"]
        yaml <- getDirectoryFiles "" ["//*.yaml"]
        cabal <- getDirectoryFiles "" ["//*.cabal"]
        mad <- getDirectoryFiles "" ["//*.mad"]
        html <- getDirectoryFiles "" ["web-src//*.html"]
        css <- getDirectoryFiles "" ["web-src//*.css"]
        need $ hs <> yaml <> cabal <> mad <> html <> css
        (Stdout out') <- cmd ["poly", "-c", ".", "-e", "README.md", "-e", "TODO.md", "-e", "target", "-e", "Justfile", "-e", "CONTRIBUTING.md"]
        file <- liftIO $ Strict.readFile "README.md"
        let header = takeWhile (/= replicate 79 '-') $ lines file
        let new = unlines header ++ out' ++ "```\n"
        liftIO $ writeFile out new

    "purge" ~> do
        putNormal "purging local files..."
        unit $ cmd ["rm", "-rf", "tags", "shake", "mad-src/tags"]
        removeFilesAfter "dist" ["//*"]
        removeFilesAfter "dist-newstyle" ["//*"]
        removeFilesAfter ".shake" ["//*"]
        removeFilesAfter "target" ["//*"]

    ["dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js.externs", "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js"] &%> \_ -> do
        need ["src/Lib.hs","linkedin-madlibs.cabal","cabal.project.local","mad-src/linkedin-madlibs.mad"]
        unit $ cmd ["bash", "-c", "madlang check mad-src/linkedin-madlibs.mad > /dev/null"]
        cmd ["cabal", "new-build"]

    googleClosureCompiler ["dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js", "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js.externs"] "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js"

    "target/all.min.js" %> \out -> do
        need ["dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js"]
        copyFile' "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js" out

    "target/styles.css" %> \out -> do
        liftIO $ createDirectoryIfMissing True "target"
        need ["web-src/styles.css"]
        cmd ["cp","web-src/styles.css", out]

    "target/index.html" %> \out -> do
        need ["target/all.min.js", "target/styles.css"]
        cmd ["cp","web-src/index.html", out]
