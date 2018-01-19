import           Data.Maybe
import           Data.Monoid
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           System.Directory
import qualified System.IO.Strict           as Strict

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = ".shake", shakeLint = Just LintBasic } $ do
    want [ "target/index.html", "README.md" ]

    "sample" ~> do
        cmd ["madlang", "run", "mad-src/linkedin-madlibs.mad", "-r57"]

    "clean" ~> do
        putNormal "cleaning files..."
        unit $ cmd ["rm", "-rf", "tags"]
        removeFilesAfter "target" ["//*"]
        cmd ["stack", "clean"]

    "README.md" %> \out -> do
        hs <- getDirectoryFiles "" ["src//*.hs"]
        yaml <- getDirectoryFiles "" ["//*.yaml"]
        cabal <- getDirectoryFiles "" ["//*.cabal"]
        mad <- getDirectoryFiles "" ["//*.mad"]
        html <- getDirectoryFiles "" ["web-src//*.html"]
        css <- getDirectoryFiles "" ["web-src//*.css"]
        need $ hs <> yaml <> cabal <> mad <> html <> css
        (Stdout out) <- cmd ["tokei", ".", "-e", "README.md", "-e", "TODO.md", "-e", "target", "-e", "Justfile", "-e", "CONTRIBUTING.md"]
        file <- liftIO $ Strict.readFile "README.md"
        let header = takeWhile (/= replicate 79 '-') $ lines file
        let new = unlines header ++ out ++ "```\n"
        liftIO $ writeFile "README.md" new
        cmd ["rm", "-f", "README.md.original"]

    "purge" ~> do
        putNormal "purging local files..."
        unit $ cmd ["rm", "-rf", "tags", "shake", "mad-src/tags"]
        removeFilesAfter "dist" ["//*"]
        removeFilesAfter "dist-newstyle" ["//*"]
        removeFilesAfter ".shake" ["//*"]
        removeFilesAfter "target" ["//*"]

    "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js" %> \out -> do
        need ["src/Lib.hs","linkedin-madlibs.cabal","cabal.project.local","mad-src/linkedin-madlibs.mad"]
        unit $ cmd ["bash", "-c", "madlang check mad-src/linkedin-madlibs.mad > /dev/null"]
        cmd ["cabal", "new-build"]

    "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js" %> \out -> do
        need ["dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.js"]
        cmd (Cwd "dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe") Shell "ccjs all.js --externs=node --externs=all.js.externs > all.min.js"

    "target/all.min.js" %> \out -> do
        need ["dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js"]
        cmd Shell "cp dist-newstyle/build/x86_64-linux/ghcjs-0.2.1.9008011/linkedin-madlibs-0.1.0.0/x/linkedin-madlibs/build/linkedin-madlibs/linkedin-madlibs.jsexe/all.min.js target/all.min.js"

    "target/styles.css" %> \out -> do
        liftIO $ createDirectoryIfMissing True "target"
        need ["web-src/styles.css"]
        cmd ["cp","web-src/styles.css", "target/styles.css"]

    "target/index.html" %> \out -> do
        need ["target/all.min.js", "target/styles.css"]
        cmd ["cp","web-src/index.html", "target/index.html"]
