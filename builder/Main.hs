{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

_PANDOC :: [String] -> [FilePath] -> Action ()
_PANDOC cmds files = do
    let clean = unwords . words
    Stdout userId <- cmd "id -u"
    Stdout groupId <- cmd "id -g"
    Stdout dataPath <- cmd Shell (Cwd ".") "pwd"
    let volume :: String = clean dataPath ++ ":/data"
    let user :: String =  clean userId ++ ":" ++ clean groupId
    let pandocCmds = words . unwords $ cmds
    let dockerCmds = ["run", "--volume", volume, "--user", user, "pandoc/latex:2.6"]
    command_ [] "docker" $ dockerCmds ++ pandocCmds ++ files

getFlags :: Action [String]
getFlags = do
    let _CUSTOM_HTML = "templates/custom.html"
    let _FOOT_NOTE_HTML = "templates/footer.html"
    let _HEADER_HTML = "templates/header.html"
    let _NAV_HTML = "html/navigation.html"
    need [_CUSTOM_HTML, _FOOT_NOTE_HTML, _HEADER_HTML, _NAV_HTML] 
    return
        [ "--standalone"
        , "--from markdown"
        , "--to html"
        , "--mathjax"
        , "--toc"
        , "--toc-depth=3"
        , "--css=all.css"
        , "--template=" ++ _CUSTOM_HTML
        , "--include-after-body=" ++ _FOOT_NOTE_HTML
        , "--include-before-body=" ++ _NAV_HTML
        , "--include-in-header=" ++ _HEADER_HTML
        ]

navFlags :: [String]
navFlags =
    [ "--from markdown+native_divs"
    , "--to html"
    ]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="html"} $ do
    want ["singlepage"]

    phony "clean" $ do
        putInfo "Cleaning files in html"
        removeFilesAfter "html" ["//*"]

    "singlepage" %> \out -> do
        mds <- getDirectoryFiles "" ["//*.md"]
        need mds
        flags <- getFlags
        _PANDOC (flags ++ ["--output", out]) mds
    
    "html/navigation.html" %> \out -> do
        let nav = "navigation.md" 
        need [nav]
        _PANDOC (navFlags ++ ["--output", out]) [nav]