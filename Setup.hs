import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  {  postCopy = copyLib
  }

copyLib :: :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    rawSystemExit verbosity "cp" ["libHSdear-imgui-2.0.0-inplace-ghc8.10.7.so", libPref]
