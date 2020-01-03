{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
import Development.Shake
import Development.Shake.FilePath
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static

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
    need ["buildDockerImg"]
    runDockerCmd "img-converter:latest" (["pdf2svg"] ++ cmds) [pdfIn, svgOut]

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

serveFiles :: FilePath -> Action ()
serveFiles dir = liftIO $ do
    let port = 8800
    putStrLn $ "Serving files on port " ++ show port
    run port (staticApp (defaultFileServerSettings dir))

getFlags :: Action [String]
getFlags = do
    let _CUSTOM_HTML = "templates/custom.html"
    let _FOOT_NOTE_HTML = "templates/footer.html"
    let _HEADER_HTML = "templates/header.html"
    let _NAV_HTML = "html/navigation.html"
    let _ALL_CSS = "html/all.css"
    need
        [ _CUSTOM_HTML
        , _FOOT_NOTE_HTML
        , _HEADER_HTML
        , _NAV_HTML
        , _ALL_CSS
        ] 
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
    want ["build"]

    phony "buildDockerImg" buildDocker

    phony "clean" $ do
        putInfo "Cleaning files in html"
        removeFilesAfter "html" ["//*"]

    phony "build" $ do
        need ["html/fullpage.html", "renderTex", "copyImages", "copyStatic"]

    phony "test" $ do
        need ["build"]
        serveFiles "html"

    phony "copyStatic" $ do
        sts <- getDirectoryFiles "static" ["//*"]
        mapM_ (\p -> copyFileChanged ("static" </> p) ("html" </> p)) sts

    phony "copyImages" $ do
        need ["renderTex"]
        svgs <- getDirectoryFiles "src/img" ["//*.svg", "//*.png", "//*.jpg"]
        mapM_ (\p -> copyFileChanged ("src/img" </> p) ("html/img" </> p)) svgs
        
        pdfs <- getDirectoryFiles "src/img" ["//*.pdf"]
        need ["html/img" </> p -<.> "svg" | p <- pdfs]

    phony "renderTex" $ do
        texs <- getDirectoryFiles "src" ["//*.tex"]
        let pdfs = ["src" </> c -<.> "pdf" | c <- texs]
        need pdfs

    "html/fullpage.html" %> \out -> do
        mds <- getDirectoryFiles "src" ["//*.md"]
        flags <- getFlags
        runPandoc (flags ++ ["--output", out]) ["src" </> m | m <- mds]
    
    "html/navigation.html" %> \out -> do
        let nav = "navigation.md" 
        need [nav]
        runPandoc (navFlags ++ ["--output", out]) [nav]

    "src/img/tikz//*.pdf" %> \out -> do
        let tex = out -<.> "tex" 
        let texFolder = takeDirectory tex
        need [tex]
        runPdfLaTex ["-output-directory", texFolder] tex
        removeFilesAfter texFolder ["*.aux"]
        removeFilesAfter texFolder ["*.log"]

    "html/img//*.svg" %> \out -> do
        let pdf = "src/img" </> (makeRelative "html/img" out) -<.> "pdf" 
        need [pdf]
        runPdf2Svg [] pdf out

    "html/all.css" %> \out -> do
        baseCSS   <- readFileLines "css/bootstrap.min.css"
        screenCSS <- readFileLines "css/screen.css"
        writeFileLines out (baseCSS ++ screenCSS)