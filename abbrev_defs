;;-*-coding: utf-8;-*-
(define-abbrev-table 'global-abbrev-table
  '(
    ("datakinds" "{-# LANGUAGE DataKinds #-}" nil :count 1)
    ("elisppackage" ";;; my-package.el --- A brief description of my package -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Your Name

;; Author: Your Name <you@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs \"27.1\"))
;; Keywords: lisp, extensions
;; URL: https://github.com

;;; Commentary:
;; Put a longer description of what your package does right here.

;;; Code:

(defun my-package-hello ()
  \"Say hello to the user.\"
  (message \"Hello from my custom package!\"))

(provide 'my-package)
;;; my-package.el ends here" nil :count 0)
    ("elmhtml" "<!DOCTYPE HTML>
<html>
<head>
  <meta charset=\"UTF-8\">
  <title>Main</title>
  <link rel=\"stylesheet\" href=\"whatever-you-want.css\">
  <script src=\"main.js\"></script>
</head>
<body>
  <script>var app = Elm.Main.init();</script>
</body>
</html>
" nil :count 2)
    ("elmignore" "# elm-package generated files
elm-stuff
# elm-repl generated files
repl-temp-*
# emacs
*~" nil :count 1)
    ("elmmain" "module Main exposing (..)

import Browser
import Html exposing (Html)
import Element exposing (Element)

type alias Flags = ()
type alias Msg = ()
type alias Model = ()    

view : Model -> Html Msg
view _ =
    let element =
            Element.el
                [ Element.centerX
                , Element.centerY
                ]
                (Element.text \"Hello Elm !\")
    in Element.layout [] element                   
    
    
update : Msg -> Model -> Model
update _ model = model         

main : Program Flags Model Msg
main =
    Browser.sandbox
        { init = ()
        , update = update
        , view = view
        }

" nil :count 0)
    ("elmmodule" "module Name exposing (..)" nil :count 2)
    ("haskellignore" "dist
dist-*
cabal-dev
*.o
*.hi
*.hie
*.chi
*.chs.h
*.dyn_o
*.dyn_hi
.hpc
.hsenv
.cabal-sandbox/
cabal.sandbox.config
*.prof
*.aux
*.hp
*.eventlog
.stack-work/
cabal.project.local
cabal.project.local~
.HTF/
.ghc.environment.*
dist-newstyle/
*.env
*~" nil :count 0)
    ("haskelllanguage" "{-# LANGUAGE   #-} " nil :count 0)
   ))

