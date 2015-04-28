#!/usr/bin/guile \
-e main -s
!#
;;; config.scm --- Deploy config files

;; Copyright © 2015 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>
;; Created:  3 Mar 2015

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This script may be used to list, show info and deploy (create
;; symlinks) various configuration files.

;;; Code:

(use-modules
 (ice-9 format)
 (ice-9 match)
 (srfi srfi-1)
 (srfi srfi-26)
 (srfi srfi-37)
 (al places)
 (al files)
 (al messages)
 (al links)
 (al configs))

(setlocale LC_ALL "")


;;; Configs

(define-syntax-rule (make-configs (config-name (filename target) ...) ...)
  (list (make-config config-name
                     (list (make-link filename target) ...))
        ...))

(define %configs
  (make-configs
   ("config"
    ((home-file "bin/config") "../config/config.scm"))

   ("conkeror"
    ((home-file ".conkerorrc") "config/conkeror/init.js"))
   ("emacs"
    ((home-file ".emacs.d/init.el") "../config/emacs/config/init.el"))
   ("stumpwm"
    ((home-file ".stumpwmrc") "config/stumpwm/init.lisp"))
   ("dmd"
    ((home-file ".dmd.d/init.scm") "../config/dmd/init.scm"))

   ("ssh"
    ((home-file ".ssh") "config/ssh"))
   ("gpg"
    ((home-file ".gnupg") "config/gpg"))
   ("auth"
    ((home-file ".authinfo.gpg") "config/auth/authinfo.gpg"))
   ("top"
    ((home-file ".toprc") "config/top/toprc"))
   ("sbcl"
    ((home-file ".sbclrc") "config/sbcl/sbclrc"))
   ("git"
    ((home-file ".gitconfig") "config/git/gitconfig"))
   ("lirc"
    ((home-file ".lircrc")  "config/lirc/lircrc"))
   ("rtorrent"
    ((home-file ".rtorrent.rc") "config/rtorrent/rc"))
   ("tvtime"
    ((home-file ".tvtime") "config/tvtime"))
   ("mplayer"
    ((home-file ".mplayer") "config/mplayer"))

   ("mpv"
    ((home-file ".config/mpv") "../config/mpv"))

   ("X"
    ((home-file ".Xmodmap")    "config/X/Xmodmap")
    ((home-file ".Xresources") "config/X/Xresources")
    ((home-file "XTerm")       "config/X/XTerm"))

   ("bash"
    ((home-file ".inputrc")      "config/bash/inputrc")
    ((home-file ".bashrc")       "config/bash/bashrc")
    ((home-file ".bash_profile") "config/bash/profile"))))

(define (configs-names)
  "Return list of all available config names."
  (map config-name %configs))

(define (configs-links)
  "Return list of all available config links."
  (append-map config-links %configs))


;;; Command-line args

(define (show-help)
  (display "Usage: config OPTION [CONFIG]...
List, show info or deploy available/specified configurations.")
  (display "\n
Options:
  -h, --help        display this help and exit
  -l, --list        list available configurations and exit
  -O, --list-old    list old configuration files and exit
  -o, --ls-old      perform 'ls -l' on the old configuration files and exit
  -D, --delete-old  delete old configuration files and exit
  -s, --show        show info of the specified configurations
  -d, --deploy      deploy (create symlinks) the specified configurations")
  (display "\n
If '--show' or '--deploy' option is used and no configuration is
specified, then all available ones will be shown or deployed.")
  (newline))

(define* (list-strings strings #:key title
                       (proc (lambda (s)
                               (format #t "~{~a~%~}" (sort s string-ci<)))))
  "Display list of STRINGS using PROC."
  (when title
    (display title)
    (newline))
  (proc strings))

(define (show-configs-names)
  (list-strings (configs-names)
                #:title "Available configurations:"))

(define (call-on-old-files proc)
  "Call PROC on old configuration files."
  (let ((files (old-files)))
    (if (null? files)
        (display "There are no old configuration files.\n")
        (proc (old-files)))))

(define (show-old-files)
  (call-on-old-files
   (lambda (files)
     (list-strings files
                   #:title "Old configuration files:"))))

(define (ls-old-files)
  "Perform 'ls -l' on the old configuration files."
  (call-on-old-files
   (lambda (files)
     (list-strings
      files
      #:title "Old configuration files:"
      #:proc (lambda (files)
                   (apply system*
                          "ls" "-l" "--directory" "--color=auto"
                          (sort files string-ci<)))))))

(define (delete-old-files)
  (call-on-old-files (cut map delete-file-recursively <>)))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda _
                  (show-help)
                  (exit 0)))
        (option '(#\l "list") #f #f
                (lambda _
                  (show-configs-names)
                  (exit 0)))
        (option '(#\O "list-old") #f #f
                (lambda _
                  (show-old-files)
                  (exit 0)))
        (option '(#\o "ls-old") #f #f
                (lambda _
                  (ls-old-files)
                  (exit 0)))
        (option '(#\D "delete-old") #f #f
                (lambda _
                  (delete-old-files)
                  (exit 0)))
        (option '(#\s "show") #f #f
                (lambda (opt name arg seed)
                  (alist-cons 'action show-config seed)))
        (option '(#\d "deploy") #f #f
                (lambda (opt name arg seed)
                  (alist-cons 'action
                              (cut deploy-config <> old-unique-filename)
                              seed)))))

(define (parse-args args)
  "Return alist of options from command-line ARGS."
  (reverse
   (args-fold args %options
              (lambda (opt name arg seed)
                ((message-proc #:destination (current-error-port))
                 "Unrecognized option: '~a'." name)
                seed)
              (lambda (arg seed)
                (alist-cons 'config arg seed))
              '())))



(define (old-filename filename)
  "Return name of an old config file that should be backed up."
  (string-append filename "-old"))

(define (old-unique-filename filename)
  "Return unique name of an old config file that should be backed up."
  (unique-filename (old-filename filename)))

(define (old-files)
  "Return list of old config files."
  (append-map (lambda (link)
                (find-matching-files
                 (old-filename (link-filename link))))
              (configs-links)))

(define (options->configs-names opts)
  "Return list of config names from OPTS alist."
  (filter-map (match-lambda
               (('config . name) name)
               (_ #f))
              opts))

(define (config-name->config name)
  "Return config record from '%configs' list by its NAME."
  (find (lambda (config)
          (equal? (config-name config) name))
        %configs))

(define (configs-names->configs names)
  "Return config records from '%configs' list by their NAMES."
  (filter-map
   (lambda (name)
     (or (config-name->config name)
         (begin ((message-proc #:destination (current-error-port))
                 "No '~a' configuration was found." name)
                #f)))
   names))

(define (main args)
  (let* ((opts    (parse-args (cdr args)))
         (action  (or (assq-ref opts 'action)
                      (leave (string-append
                              "No action is specified.~%"
                              "Try '~a --help' for more information.")
                             (car args))))
         (names   (options->configs-names opts))
         (configs (if (null? names)
                      %configs
                      (configs-names->configs names))))
    (map action configs)))

;;; config.scm ends here
