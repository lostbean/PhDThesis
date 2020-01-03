{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath

type Container = String

buildDocker :: Action ()
buildDocker = do
    need ["Dockerfile"]
    command_ [] "docker" ["build", "-t", "img-converter", "."]

runPandoc :: [String] -> [FilePath] -> Action ()
runPandoc = runDockerCmd "pandoc/latex:2.6"

runPdfLaTex :: [String] -> FilePath -> Action ()
runPdfLaTex cmds = runDockerCmd "blang/latex:ubuntu" (["pdflatex"] ++ cmds) . pure

runPdf2Svg :: [String] -> FilePath -> FilePath -> Action ()
runPdf2Svg cmds pdfIn svgOut = do
    need ["imgConverter"]
    runDockerCmd "img-converter:latest" ["pdf2svg"] [pdfIn, svgOut]

runDockerCmd :: Container -> [String] -> [FilePath] -> Action ()
runDockerCmd container dockerCmds entryPointCmds = do
    let clean = unwords . words
    Stdout userId <- cmd "id -u"
    Stdout groupId <- cmd "id -g"
    Stdout dataPath <- cmd Shell (Cwd ".") "pwd"
    let volume :: String = clean dataPath ++ ":/data"
    let user :: String =  clean userId ++ ":" ++ clean groupId
    let dockerCmds' = words . unwords $ dockerCmds
    let baseCmds = ["run", "--volume", volume, "--user", user, container]
    command_ [] "docker" $ baseCmds ++ dockerCmds' ++ entryPointCmds

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
    want ["html/fullpage.html"]

    phony "imgConverter" buildDocker

    phony "clean" $ do
        putInfo "Cleaning files in html"
        removeFilesAfter "html" ["//*"]

    "html/fullpage.html" %> \out -> do
        mds <- getDirectoryFiles "" ["//*.md"]
        texs <- getDirectoryFiles "" ["//*.tex"]
        let svgs = [c -<.> "svg" | c <- texs]
        need svgs
        flags <- getFlags
        runPandoc (flags ++ ["--output", out]) mds
    
    "html/navigation.html" %> \out -> do
        let nav = "navigation.md" 
        need [nav]
        runPandoc (navFlags ++ ["--output", out]) [nav]

    "//*.pdf" %> \out -> do
        let tex = out -<.> "tex" 
        let texFolder = takeDirectory tex
        need [tex]
        runPdfLaTex ["-output-directory", texFolder] tex
        removeFilesAfter texFolder ["*.aux"]
        removeFilesAfter texFolder ["*.log"]

    "//*.svg" %> \out -> do
        let pdf = out -<.> "pdf" 
        need [pdf]
        runPdf2Svg [] pdf out